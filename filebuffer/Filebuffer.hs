{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Filebuffer
  ( Direction (..)
  , module Filebuffer
  , module Stream
  , noEdits
  ) where

import Common
import Stream
import Utils

import Data.Edits
import Data.Lines
import Data.Tree  hiding (Color)

import System.IO

import Data.ByteString qualified as Strict
import Data.Tree       qualified as Tree

import Streaming          qualified as S
import Streaming.Internal qualified as S
import Streaming.Prelude  qualified as SP

-- | Storing the linebuffer as an Async allows us to defer
-- | the `wait` on the result to use-time, which is useful
-- | for extremely large files (> 100 MB). This essentially
-- | makes startup O(1) in the file size.
-- |
-- | Unfortunately, this is crude and certainly not optimal.
-- | It would be far better to somehow represent
-- | partial results from the line scan in a usable way
-- | (since you likely won't need later line positions immediately
-- | as soon as the file is loaded), though I don't see an elegant way of doing this at the moment.
-- |
-- | (update) Actually, the following might work:
-- |   - create an AsyncVector type holding mutable variables for
-- |     the length and handle of an asynchronously generated vector.
-- |     Probably wrap it in Either, with Left .. for the unevaluated component
-- |     and Right .. for the fully evaluated part.
-- |   - define operations (index, take, drop, split, etc..) on AsyncVector
-- |     with unsafePerformIO that read the length of the mvar and block
-- |     until it's long enough to perform the operation, always returning
-- |     an AsyncVector for the tail.

data Filebuffer = Filebuffer
  { cursor          :: {-# UNPACK #-} !Position
  , start           :: {-# UNPACK #-} !Position
  , size            :: {-# UNPACK #-} !Int
  , handle          :: {-# UNPACK #-} !Handle
  , edits           :: !Edits
  , lines           :: !(Tree Lines)
  , visualLineCount :: {-# UNPACK #-} !Int
  }
  deriving (Show)


{-# INLINE delete #-}
delete
  :: Int
  -> Filebuffer
  -> Filebuffer
delete count filebuffer@Filebuffer{cursor, edits, lines} =
  filebuffer { edits = edits'
             , lines   = lines'
             }
  where edits' = deletePatches cursor count edits
        end      = moveForward count edits cursor
        lines'   = Tree.removeLines lines cursor end

{-# INLINE insert #-}
insert
  :: Strict.ByteString
  -> Filebuffer
  -> Filebuffer
insert !string !filebuffer@Filebuffer{cursor, edits, lines} =
  filebuffer { cursor = Position cursor.base $ cursor.offset + Strict.length string
             , edits  = insertPatches       string cursor edits
             , lines  = Tree.insertNewlines string cursor lines
             }

{-# INLINE movePosition #-}
movePosition
  :: Direction
  -> Filebuffer
  -> Filebuffer
movePosition direction filebuffer@Filebuffer{edits, lines, cursor} =
  filebuffer { cursor = max cursor' $ Position 0 0, edits}
  where cursor' = case (direction) of
          Forward -> moveForward  1 edits       cursor
          Back    -> moveBackward 1 edits       cursor
          Down    -> moveDown     1 edits lines cursor
          Up      -> moveUp       1 edits lines cursor

{-# INLINE repositionStart #-}
repositionStart
  :: Filebuffer
  -> Filebuffer
repositionStart filebuffer@Filebuffer{ cursor, lines, start, edits, visualLineCount } =
  -- current_line == first_line should only happen at the very start of the file
  if (current_line == first_line) then
    filebuffer { start = moveForward 1 edits $ Position (-1) 0 }
  else if ( current_line - first_line < div visualLineCount 5 ) then
    filebuffer { start = startOf $ first_line + (current_line - first_line - div visualLineCount 5) }
  else if ( current_line - first_line >= div (4 * visualLineCount) 5 ) then
    filebuffer { start = startOf $ first_line + (current_line - first_line - div (4 * visualLineCount) 5) }
  else
    filebuffer
  where current_line = Tree.findLineNumber cursor lines
        first_line = Tree.findLineNumber start lines
        startOf n = moveForward 1 edits $ Tree.findLinePosition n lines

{-# INLINE scan #-}
scan
  :: Int
  -> Filebuffer
  -> Strict.ByteString
  -> S.Stream (S.Of Position) IO ()
scan n Filebuffer{handle, edits, cursor} string =
  search string skips (MatchState 0 CNil FNil) $ S.takes n $ streamWithPatches handle edits cursor' $ S.Return ()
  where skips = calculateSkips string
        cursor' = moveForward 1 edits cursor

{-# INLINE scanBackwards #-}
scanBackwards
  :: Int
  -> Filebuffer
  -> Strict.ByteString
  -> S.Stream (S.Of Position) IO ()
scanBackwards n Filebuffer{handle, edits, cursor} string =
  search string skips (MatchState 0 CNil FNil) $ stopStreamAt cursor $ streamWithPatches handle edits cursor' $ S.Return ()
  where skips = calculateSkips string
        cursor' = max (Position 0 0) $ moveBackward n edits cursor

-- | This is not actually correct,
-- | since injectAfter measures distance in code points
-- | while the cursor_distance is measured in raw bytes.
-- | Similarly, dropChars measures characters in code points
-- | rather than according to their encoded byte size.
-- |
-- | This should be an easy fix.

{-# INLINE streamFilebuffer #-}
streamFilebuffer
  :: FileParser a e
  -> Filebuffer
  -> S.Stream (S.Of Symbol) IO ()
streamFilebuffer FileParser{parser, defaultColor, defaultState, cursor, requestedContext} filebuffer@Filebuffer{handle, edits, start} = do
  injectAfter (Cursor cursor) cursor_distance
  $ dropChars drop_count defaultColor
  $ loop (Byte0 0)
  $ flatparseStream parser defaultColor defaultState
  $ SP.map chunkToByteString
  $ streamWithPatches handle edits stream_start
  $ S.Return ()
  where cursor_distance = calculateDistance edits filebuffer.start filebuffer.cursor
        drop_count = calculateDistance edits stream_start start
        stream_start = max (Position 0 0) $ moveBackward (max 512 requestedContext) edits start
        chunkToByteString :: Chunk -> Strict.ByteString
        chunkToByteString (Chunk _ _ bs) = bs
        loop :: DecodeState -> S.Stream (S.Of ByteStringColored) IO () -> S.Stream (S.Of Symbol) IO ()
        loop state stream = case (stream) of
          S.Return _ -> S.Return ()
          S.Effect m -> S.Effect (fmap (loop state) m)
          S.Step (ByteStringColored string color S.:> bs) -> S.Step (ColorChange color S.:> decodeByteString string (\x -> Char x) (\s -> loop s bs) state)
