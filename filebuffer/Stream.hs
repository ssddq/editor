{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnboxedTuples         #-}

module Stream where

import Common
import Utils

import Control.Monad.IO.Class
import GHC.ForeignPtr
import System.IO

import Data.Word

import Data.Edits
import Data.Lines
import Data.Tree  hiding (Color)

import FlatParse.Basic

import Data.Tree         qualified as Tree
import Data.Vector.Async qualified as Async

import Data.ByteString          qualified as Strict
import Data.ByteString.Internal qualified as Strict

import Data.IntMap.Strict qualified as Map

import Streaming          qualified as S
import Streaming.Internal qualified as S


-- | The first argument is the position relative to the base file.
-- | The second argument is a flag indicating whether the chunk is a base file chunk (0)
-- | or an inserted chunk (1).
-- | Note, a base file chunk need not have a starting position with offset = 0
-- | if the chunk immediately succeeds an insert chunk.

data Chunk = Chunk {-# UNPACK #-} !Position
                   {-# UNPACK #-} !Word8
                   {-# UNPACK #-} !Strict.ByteString
  deriving (Show)


{-# INLINE decode #-}
decode
  :: S.Stream (S.Of Chunk) IO ()
  -> S.Stream (S.Of Word32) IO ()
decode = decodeStream (Byte0 0)

{-# INLINE decodeByteString #-}
decodeByteString
  :: forall a
   . Strict.ByteString
  -> (Word32 -> a)                            -- converter
  -> (DecodeState -> S.Stream (S.Of a) IO ()) -- stream (continuation)
  -> DecodeState
  -> S.Stream (S.Of a) IO ()
decodeByteString !bytestring@(Strict.BS _ len) converter stream = loop
  where
    loop :: DecodeState -> S.Stream (S.Of a) IO ()
    loop state = case (state) of
      Byte0 offset
        | offset < len -> loop   $ Byte1 (offset + 1) byte
        | otherwise    -> stream $ Byte0 0
        where byte = Strict.index bytestring offset
      Byte1 offset byte1
        | offset < len -> loop $ Byte2 (offset + 1) byte1 byte
        | otherwise    -> case (decodeUtf8 byte1 0 0 0) of
                            Decoded w 0 -> S.Step (converter w S.:> stream (Byte0 0))
                            _           -> stream (Byte1 0 byte1)
        where byte = Strict.index bytestring offset
      Byte2 offset byte1 byte2
        | offset < len -> loop $ Byte3 (offset + 1) byte1 byte2 byte
        | otherwise    -> case (decodeUtf8 byte1 byte2 0 0) of
                            Decoded w 0 -> S.Step (converter w S.:> loop   (Byte1 offset byte2))
                            Decoded w 2 -> S.Step (converter w S.:> stream (Byte0 0           ))
                            _           -> stream (Byte2 0 byte1 byte2)
        where byte = Strict.index bytestring offset
      Byte3 offset byte1 byte2 byte3
        | offset < len -> case (decodeUtf8 byte1 byte2 byte3 byte) of
                            Decoded w 4 -> S.Step (converter w S.:> loop (Byte0 (offset+4)                 ))
                            Decoded w 3 -> S.Step (converter w S.:> loop (Byte1 (offset+3)             byte))
                            Decoded w 2 -> S.Step (converter w S.:> loop (Byte2 (offset+2)       byte3 byte))
                            Decoded w _ -> S.Step (converter w S.:> loop (Byte3 (offset+1) byte2 byte3 byte))
        | otherwise    -> case (decodeUtf8 byte1 byte2 byte3 0) of
                            Decoded w 0 -> S.Step (converter w S.:> loop   (Byte2 offset byte2 byte3))
                            Decoded w 2 -> S.Step (converter w S.:> loop   (Byte1 offset       byte3))
                            Decoded w 3 -> S.Step (converter w S.:> stream (Byte0 0                 ))
                            Decoded _ _ -> stream (Byte3 0 byte1 byte2 byte3)
        where byte = Strict.index bytestring offset

{-# INLINE decodeStream #-}
decodeStream
  :: DecodeState
  -> S.Stream (S.Of Chunk) IO ()
  -> S.Stream (S.Of Word32) IO ()
decodeStream _ = loop
  where loop bss = case bss of
          S.Return r -> S.Return r
          S.Effect m -> S.Effect (fmap loop m)
          S.Step (Chunk _ _ string S.:> bs) ->
            decodeByteString string (\x -> x) (\s -> decodeStream s bs) (Byte0 0)

{-# INLINE dropChars #-}
dropChars
  :: Int                          -- count to drop
  -> Color                        -- default color to use
  -> S.Stream (S.Of Symbol) IO ()
  -> S.Stream (S.Of Symbol) IO ()
dropChars = loop
  where loop !0 !color stream = S.Step (ColorChange color S.:> stream)
        loop !n !color stream = case stream of
          S.Return ()          -> S.Return ()
          S.Effect m           -> S.Effect (fmap (loop n color) m)
          S.Step (b S.:> rest) -> case b of
            Char{}   -> loop (n - 1) color rest
            Cursor{} -> loop n color rest
            ColorChange color' -> loop n color' rest

{-# INLINE injectAfter #-}
injectAfter
  :: Symbol                       -- element to inject
  -> Int                          -- offset into stream
  -> S.Stream (S.Of Symbol) IO ()
  -> S.Stream (S.Of Symbol) IO ()
injectAfter a = loop
  where loop !0 stream = S.Step (a S.:> stream)
        loop !n stream = case stream of
          -- If we reach the end of the stream,
          -- just place the cursor anyway.
          S.Return _           -> S.Step (a S.:> S.Return ())
          S.Effect m           -> S.Effect (fmap (loop n) m)
          S.Step (b S.:> rest) -> case b of
            c@Char{}        -> S.Step (c S.:> loop (n-1) rest)
            c@Cursor{}      -> S.Step (c S.:> loop n rest)
            c@ColorChange{} -> S.Step (c S.:> loop n rest)

-- | This function will try to match the last character of the given string
-- | to the current position in the bytestring chunk so far,
-- | and advance according to a lookup table if the character does not match.
-- |
-- | This avoids having to examine every byte in the bytestring,
-- | though causes some issues at chunk boundaries
-- | if we don't have a upper bounds on the input string
-- | or lower bounds on the chunk size
-- | (which may be a single byte in the context of this program).
-- |
-- | To resolve this, match will switch to linear scanning (without skips)
-- | if there are few bytes left in the chunk,
-- | and only switch back when the forward linear scanning list of partial matches
-- | becomes empty.
-- |
-- | (!!) Note that this function will cause an error
-- | if the given ByteString does not have length >= 2.

{-# INLINE match #-}
match
  :: Position          -- chunk start
  -> Word8             -- chunk flag
  -> Strict.ByteString -- current bytestring chunk
  -> Strict.ByteString -- target string to match
  -> Word8             -- last character of the string
  -> Map.IntMap Int    -- skip value table
  -> MatchState        -- state of match so far
  -> MatchState        -- final state after parsing bytestring
match !start !flag !string@(Strict.BS fp size) !target@(Strict.BS _ length) !last !skips !s@(MatchState _ _ FNil) = loop s
  where loop :: MatchState -> MatchState
        loop !(MatchState offset done FNil) = case (next + 1 <= size, char == last) of
          (True , True ) -> if (matchBackwards target (length - 2) string (offset - 1)) then
                              loop $ MatchState next (Match start flag (offset - length + 1) done) FNil
                            else
                              loop $ MatchState next done FNil
          (True , False) -> loop $ MatchState next done FNil
          (False, True ) -> if (matchBackwards target (length - 2) string (offset - 1)) then
                              matchForwards
                                |- start
                                |- flag
                                |- target
                                |- MatchState offset (Match start flag (offset + 1 - length) done) FNil
                                |- Strict.BS (plusForeignPtr fp offset') (size - offset')
                            else
                              matchForwards
                                |- start
                                |- flag
                                |- target
                                |- MatchState offset done FNil
                                |- Strict.BS (plusForeignPtr fp offset') (size - offset')
          (False, False) -> matchForwards
                              |- start
                              |- flag
                              |- target
                              |- MatchState offset done FNil
                              |- Strict.BS (plusForeignPtr fp offset') (size - offset')
          where char = Strict.index string offset
                next = offset + skip
                skip = Map.findWithDefault length (fromIntegral char) skips
                offset' = offset + 1
        loop _ = error "The loop in match entered an invalid state"
match !start !flag !string !target !last !skips !state@(MatchState offset _ _) =
  if (next == offset) then
    state'
  else
    match
      |- start
      |- flag
      |- string
      |- target
      |- last
      |- skips
      |- state'
  where string128 = Strict.take 128 $ Strict.drop (offset+1) string
        state'@(MatchState next _ _) = matchForwards start flag target state string128

-- | There are several ways matchBackwards could be implemented,
-- | but this seemed to perform the best.

{-# INLINE matchBackwards #-}
matchBackwards
  :: Strict.ByteString -- string to match
  -> Int               -- position to match at
  -> Strict.ByteString -- chunk
  -> Int               -- offset into chunk
  -> Bool
matchBackwards !target !i !string !offset =
  if (offset < 0) then
    False
  else if (Strict.index target i == Strict.index string offset) then
    if (i == 0) then
      True
    else
      matchBackwards target (i-1) string (offset-1)
  else
    False

{-# INLINE matchChar #-}
matchChar
  :: Position
  -> Word8
  -> Strict.ByteString
  -> MatchState
  -> Word8
  -> MatchState
matchChar !start !flag !target !(MatchState offset completed FNil) !char =
  case (char == (Strict.index target 0)) of
    True -> MatchState (offset + 1) completed (PartialMatch start flag offset 1 FNil)
    _    -> MatchState (offset + 1) completed FNil
matchChar !start !flag !target@(Strict.BS _ len) !(MatchState offset completed !(PartialMatch start' flag' offset' index' fs)) !char =
  case (char == Strict.index target index') of
    True -> let MatchState _ completed' fs' = matchChar start flag target (MatchState offset completed fs) char
            in
            case (index' == len - 1) of
              True -> MatchState (offset + 1) (Match start' flag' offset' completed') fs'
              _    -> MatchState (offset + 1) completed' (PartialMatch start' flag' offset' (index' + 1) fs')
    False -> matchChar start flag target (MatchState offset completed fs) char

{-# INLINE matchForwards #-}
matchForwards
  :: Position
  -> Word8
  -> Strict.ByteString
  -> MatchState
  -> Strict.ByteString
  -> MatchState
matchForwards !start !flag !target !state !string = Strict.foldl' (matchChar start flag target) state string

{-# INLINE scanLines #-}
scanLines
  :: FilePath
  -> IO (Tree Lines)
scanLines path = do
  fmap (Tree.new . Base) $ Async.generateNewlines path

{-# INLINE search #-}
search
  :: (Monad m)
  => Strict.ByteString
  -> Map.IntMap Int
  -> MatchState
  -> S.Stream (S.Of Chunk) m r
  -> S.Stream (S.Of Position) m r
search !target@(Strict.BS _ length) !skips !state = case (length) of
  0 -> error "search called with empty target"
  1 -> searchChar (Strict.head target)
  _ -> loop state
  where last = Strict.last target
        fold :: (Monad m) => S.Stream (S.Of Position) m r -> CompletedMatches -> S.Stream (S.Of Position) m r
        fold s !(Match !position !0 !0     ds) = fold (S.Step (position                                          S.:> s)) ds
        fold s !(Match !position !0 !index ds) = fold (S.Step (Position (position.base + index) 0                S.:> s)) ds
        fold s !(Match !position _  !index ds) = fold (S.Step (Position  position.base (position.offset + index) S.:> s)) ds
        fold s !CNil                         = s
        loop :: (Monad m) => MatchState -> S.Stream (S.Of Chunk) m r -> S.Stream (S.Of Position) m r
        loop _      !(S.Return r) = S.Return r
        loop !state !(S.Effect m) = S.Effect $ fmap (loop state) m
        loop !state !(S.Step (chunk S.:> bs)) = fold (loop (MatchState 0 CNil fmatches) bs) done
          where Chunk position flag string = chunk
                MatchState _ done fmatches   = match position flag string target last skips state

{-# INLINE searchChar #-}
searchChar
  :: (Monad m)
  => Word8                        -- char to match
  -> S.Stream (S.Of Chunk) m r
  -> S.Stream (S.Of Position) m r
searchChar _     !(S.Return r) = S.Return r
searchChar !char !(S.Effect m) = S.Effect (fmap (searchChar char) m)
searchChar !char !(S.Step (Chunk start flag string S.:> bs))
  = searchCharChunk start flag char string (searchChar char bs)

{-# INLINE searchCharChunk #-}
searchCharChunk
  :: Position
  -> Word8
  -> Word8                        -- char to match
  -> Strict.ByteString            -- current bytestring chunk
  -> S.Stream (S.Of Position) m r
  -> S.Stream (S.Of Position) m r
searchCharChunk !start !flag !char !string@(Strict.BS _ length) = loop 0
  where loop :: Int -> S.Stream (S.Of Position) m r -> S.Stream (S.Of Position) m r
        loop offset stream =
          if (length == offset) then
            stream
          else
            if (char == next) then
              S.Step (position S.:> loop (offset + 1) stream)
            else
              loop (offset + 1) stream
          where next = Strict.index string offset
                position = case (flag) of
                  0 -> Position (start.base + offset) 0
                  1 -> Position start.base (start.offset + offset)
                  -- Eventually, change 1 to _ for safety. At the moment however,
                  -- I'd prefer to know if a chunk somehow ended up with an invalid flag.
                  _ -> error "invalid chunk flag encountered in searchCharChunk"

{-# INLINE stopStreamAt #-}
stopStreamAt
  :: (Monad m)
  => Position
  -> S.Stream (S.Of Chunk) m r
  -> S.Stream (S.Of Chunk) m ()
stopStreamAt position@Position{base, offset} stream = case (stream) of
  S.Effect m -> S.Effect (fmap (stopStreamAt position) m)
  S.Return _ -> S.Return ()
  S.Step (Chunk position'@Position{base = base', offset = offset'} 0 string S.:> rest) ->
    if (Strict.null string' || (base' == base && offset' > offset)) then
      S.Return ()
    else
      S.Step (Chunk position' 0 string' S.:> stopStreamAt position rest)
    where string' = Strict.take (base - base') string
  S.Step (chunk@(Chunk position'@Position{base = base', offset = offset'} _ string) S.:> rest) ->
    if (base' > base || (base' == base && offset' > offset)) then
      S.Return ()
    else if (base' < base) then
      S.Step (chunk S.:> stopStreamAt position rest)
    else
      S.Step (Chunk position' 1 string' S.:> stopStreamAt position rest)
    where string' = Strict.take (offset - offset') string

{-# INLINE streamChunks #-}
streamChunks
  :: Handle
  -> StreamState                 -- state containing stopping position
  -> S.Stream (S.Of Chunk) IO () -- continuation
  -> S.Stream (S.Of Chunk) IO ()
streamChunks handle (StreamState base offset limit) stream = do
  chunk <- liftIO $ Strict.hGetSome handle k
  if (Strict.null chunk) then
    stream
  else
    S.Step (Chunk (Position base offset) 0 chunk S.:> streamChunks handle (StreamState (base + k) 0 limit) stream)
  where k = min 32768 $ max 0 $ limit - base

{-# INLINE streamFile #-}
streamFile
  :: Handle
  -> S.Stream (S.Of Chunk) IO ()
streamFile handle = streamWithPatches handle noEdits (Position 0 0) (S.Return ())

{-# INLINE streamFromPosition #-}
streamFromPosition
  :: Handle
  -> Edits
  -> Position
  -> (StreamState -> (S.Stream (S.Of Chunk) IO ()))
  -> S.Stream (S.Of Chunk) IO ()
streamFromPosition handle (Branch prefix switch left right) position stream =
  case (compare position.base divider) of
    LT -> streamFromPosition handle left position
          $ \s -> streamWithPatches' handle right s
          $ stream
    _  -> streamFromPosition handle right position
          $ stream
  where divider = calculateDivider prefix switch
streamFromPosition handle (Leaf prefix skip text length) Position{ base, offset } stream =
  case (compare base prefix) of
    LT -> do liftIO $ hSeek handle AbsoluteSeek $ fromIntegral base
             -- If prefix != position.base, position.offset should have been 0 anyway,
             -- since we can only offset into a leaf.
             streamChunks handle (StreamState base 0 prefix)
               $ streamInsertChunks prefix 0 text
               $ do liftIO $ hSeek handle RelativeSeek $ fromIntegral skip
                    stream $ StreamState (prefix + skip) length maxBound
    EQ -> do liftIO $ hSeek handle AbsoluteSeek $ fromIntegral base
             liftIO $ hSeek handle RelativeSeek $ fromIntegral skip
             streamInsertChunks prefix offset (drop offset text)
               $ stream $ StreamState (prefix + skip) length maxBound
    GT -> do liftIO $ hSeek handle AbsoluteSeek $ fromIntegral base
             stream $ StreamState base 0 maxBound
  where -- There's no point rebalancing the tree here, since this is a per-frame throwaway.
        drop :: Int -> ByteTree -> ByteTree
        drop _ nil@Nil{} = nil
        drop n Tree{index, color, left, node, right} = case (compare n index) of
          LT -> Tree (index - n) color (drop n left) node right
          EQ -> Tree 0 color nil node right
          GT -> case (compare offset $ Strict.length node) of
                  LT -> Tree 0 color nil (Strict.drop offset node) right
                  EQ -> right
                  GT -> drop (offset - Strict.length node) right
          where nil = Nil Black
                offset = n - index

{-# INLINE streamInsertChunks #-}
streamInsertChunks
  :: (Monad m)
  => Int
  -> Int
  -> ByteTree
  -> S.Stream (S.Of Chunk) m r
  -> S.Stream (S.Of Chunk) m r
streamInsertChunks _ _ (Nil _) stream = stream
streamInsertChunks !base !offset (Tree leftOffset _ left chunk@(Strict.BS _ len) right) stream =
  if (Strict.null chunk) then
      streamInsertChunks base offset left
    $ streamInsertChunks base (offset' + len) right stream
  else
      streamInsertChunks base offset left
    $ S.Step (Chunk (Position base offset') 1 chunk S.:> streamInsertChunks base (offset' + len) right stream)
  where offset' = offset + leftOffset

{-# INLINE streamWithPatches #-}
streamWithPatches
  :: Handle
  -> Edits
  -> Position
  -> S.Stream (S.Of Chunk) IO ()
  -> S.Stream (S.Of Chunk) IO ()
streamWithPatches handle edits start stream = streamFromPosition handle edits start (\s -> streamChunks handle s stream)

streamWithPatches'
  :: Handle
  -> Edits
  -> StreamState
  -> (StreamState -> (S.Stream (S.Of Chunk) IO ()))
  -> S.Stream (S.Of Chunk) IO ()
streamWithPatches' handle (Branch _ _ left right) state stream =
    streamWithPatches' handle left state
  $ \s -> streamWithPatches' handle right s
  $ stream
streamWithPatches' handle (Leaf key 0 insertSequence insertLength) state@(StreamState base offset limit) stream =
  case (compare base key) of
    EQ -> do streamInsertChunks key 0 insertSequence (stream $ StreamState key (insertLength) limit)
    LT -> do streamChunks handle (StreamState base offset key)
             $ streamInsertChunks key 0 insertSequence (stream $ StreamState key insertLength limit)
    GT -> stream $ state
streamWithPatches' handle (Leaf key deleteCount insertSequence _) state@(StreamState base offset limit) stream =
  case (compare base key) of
    EQ -> do liftIO $ hSeek handle RelativeSeek $ fromIntegral deleteCount
             -- | Note that we are implicitly relying on the fact that no deleteCount ever extends past the divider (+ 1)
             -- | of the parent tree. This is a consequence of the delete implementation.
             streamInsertChunks key 0 insertSequence (stream $ StreamState (key + deleteCount) 0 limit)
    LT -> do streamChunks handle (StreamState base offset key)
             $ streamInsertChunks key 0 insertSequence
             $ do liftIO $ hSeek handle RelativeSeek $ fromIntegral deleteCount
                  (stream $ StreamState (key + deleteCount) 0 limit)
    -- | If we start in a base past a leaf, we choose to completely ignore it.
    -- | In the context of a full stream (from the start of the file), this should really never happen.
    -- | But in principle, if we try to stream from a generic position position,
    -- | we are assuming that the position position is 'valid', i.e. not in a delete range.
    GT -> stream $ state



-- * Syntax highlighting

{-# INLINE flatparseStream #-}
flatparseStream
  :: forall a e
   . (a -> Parser e (Color, a))
  -> Color
  -> a
  -> S.Stream (S.Of Strict.ByteString) IO ()
  -> S.Stream (S.Of ByteStringColored) IO ()
flatparseStream parser defaultColor = loop ""
  where
    loop :: Strict.ByteString -> a -> S.Stream (S.Of Strict.ByteString) IO () -> S.Stream (S.Of ByteStringColored) IO ()
    loop !string !state stream = case (stream) of
      S.Return r -> parseUntilFailure string state $ \remaining _ -> S.Step (ByteStringColored remaining defaultColor S.:> S.Return r)
      S.Effect m -> S.Effect (fmap (loop string state) m)
      S.Step (bs S.:> rest)
        | Strict.length string < 512 -> loop (string <> bs) state rest
        | otherwise -> parseUntilFailure string state $ \remaining state' -> loop (remaining <> bs) state' rest
    parseUntilFailure :: Strict.ByteString -> a
                      -> (Strict.ByteString -> a -> S.Stream (S.Of ByteStringColored) IO ())
                      -> S.Stream (S.Of ByteStringColored) IO ()
    parseUntilFailure !string !state cont = case (runParser (parser state) string) of
      OK (color, state') unparsed -> S.Step (ByteStringColored (diff string unparsed) color S.:> parseUntilFailure unparsed state' cont)
      Fail  -> cont string state
      Err _ -> cont string state
