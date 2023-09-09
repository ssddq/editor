{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Vector.Async
  ( module Data.Vector.Async
  , Unbox
  ) where

import Common

import Data.Vector.Dynamic qualified as Dynamic

import Prelude hiding (drop, take)

import GHC.Conc
import GHC.Exts

import Control.Concurrent.MVar
import Control.Monad

import Data.Vector.Unboxed         (Unbox)
import Data.Vector.Unboxed         qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV

import Data.ByteString          qualified as Strict
import Data.ByteString.Internal qualified as Strict

import Control.Monad.ST

import System.IO
import System.IO.Unsafe

-- * Type definition

-- | Wrapper for a Dynamic.Vector RealWorld Int that is calculated and consumed asynchronously.
-- | For referential transparency and purity, the underlying dynamic vector can *only* be appended to.
-- |
-- | Specifically:
-- |   * The Int held by consumedTVar *must* strictly increase when modified.
-- |   * The Dynamic.Vector held by consumedTVar *must* be an initial segment of *every* future entry,
-- |     though the underlying mutable vector backing can be freely changed.
-- | Violating either of these can cause STM transactions to loop indefinitely.
-- |
-- | start is the first valid index in the underlying dynamic vector.
-- | span is the length of the valid range in the underlying dynamic vector.
-- | modifier holds an additive modifier to the valid entries.
-- | vectorTVar holds a mutable reference to the dynamic vector
-- | consumedTVar is a TVar that tracks how much of the input has been consumed,
-- | and is expected to be set to maxBound :: Int when the vector is stable and will no longer mutate.

data Vector = Vector
  { modifier     :: {-# UNPACK #-} !Int
  , start        :: {-# UNPACK #-} !Int
  , span         :: {-# UNPACK #-} !Int
  , consumedTVar :: {-# UNPACK #-} !(TVar Int)
  , vectorTVar   :: {-# UNPACK #-} !(TVar (Dynamic.Vector RealWorld Int))
  }



-- * Utility instances (DO NOT USE!)

instance Eq Vector where
  (==) async1 async2 = unsafePerformSTM $ do
    consumed1 <- readTVar async1.consumedTVar
    consumed2 <- readTVar async2.consumedTVar
    waitUntil $ consumed1 >= async1.span + async1.start && consumed2 >= async2.span + async2.start
    vector1 <- readTVar async1.vectorTVar
    vector2 <- readTVar async2.vectorTVar
    vector1' <- unsafeIOToSTM . stToIO $ Dynamic.freeze vector1
    vector2' <- unsafeIOToSTM . stToIO $ Dynamic.freeze vector2
    return $ (V.take async1.span $ V.drop async1.start vector1') == (V.take async2.span $ V.drop async2.start vector2')

instance IsList Vector where
  type Item Vector = Int
  {-# INLINE fromList #-}
  fromList = fromVectorTVar . V.fromList
  {-# INLINE toList #-}
  toList Vector{modifier, start, span, consumedTVar, vectorTVar} = unsafePerformSTM $ do
    consumed <- readTVar consumedTVar
    waitUntil $ consumed == maxBound
    vector <- unsafeIOToSTM . stToIO . Dynamic.freeze =<< readTVar vectorTVar
    return $ V.toList . V.map (+ modifier) . V.take span . V.drop start $ vector

instance Show Vector where
  show (Vector modifier start span consumedTVar vectorTVar) = unsafePerformSTM $ do
    consumed <- readTVar consumedTVar
    Dynamic.Vector length _ <- readTVar vectorTVar
    waitUntil $ consumed == maxBound || length >= start + span
    vector <- unsafeIOToSTM . stToIO . Dynamic.freeze =<< readTVar vectorTVar
    return $ show $ V.map (+ modifier) . V.take span . V.drop start $ vector



-- * Generators

-- | This really needs to be unsafePerformIO (and not anything unsafer),
-- | because it allocates memory which we absolutely do not want
-- | to happen multiple times.
-- |
-- | We do not inline this, so that all empty vectors can share a common reference.
-- | Note that this is only safe because we never mutate the underlying array after initialization.
-- | Mutating (e.g. appending to) the underlying dynamic vector would require
-- | that we aggressively inline this, to produce independent references for each call.

{-# NOINLINE emptyVector #-}
emptyVector :: Vector
emptyVector = unsafePerformIO $ do
  vector <- stToIO $ Dynamic.new 0
  consumedTVar <- newTVarIO maxBound
  vectorTVar   <- newTVarIO vector
  return $ Vector { modifier = 0, start = 0, span = 0, consumedTVar, vectorTVar }

-- | This really needs to be unsafePerformIO (and not anything unsafer),
-- | because generate allocates memory which we absolutely do not want
-- | to happen multiple times.

{-# INLINE findLineOffsets #-}
findLineOffsets
  :: Int
  -> Strict.ByteString
  -> Vector
findLineOffsets offset string = unsafePerformIO . generate $ scanNewlinesChunk offset maxBound string

{-# INLINE fromDynamic #-}
fromDynamic
  :: Dynamic.Vector MV.RealWorld Int
  -> Vector
fromDynamic vector@(Dynamic.Vector length _) = unsafePerformIO $ do
  consumedTVar <- newTVarIO maxBound
  vectorTVar   <- newTVarIO vector
  return $ Vector { modifier = 0, start = 0, span = length, consumedTVar, vectorTVar }

{-# INLINE fromVectorTVar #-}
fromVectorTVar
  :: V.Vector Int
  -> Vector
fromVectorTVar vector = unsafePerformIO $ do
  mutable <- V.unsafeThaw vector
  let span = V.length vector
      dynamicVector = Dynamic.Vector span mutable
  consumedTVar <- newTVarIO maxBound
  vectorTVar   <- newTVarIO dynamicVector
  return $ Vector { modifier = 0, start = 0, span, consumedTVar, vectorTVar }

-- | If the stream is long, it becomes extremely important that GHC's runtime
-- | has at least 2 physical cores available, or the initial file scan
-- | will completely exhaust processing power in the main thread.
-- | This will result in (very obvious) performance degradation in doing anything
-- | before the scan is completely finished, and will
-- | entirely defeat the purpose of using this module in the first place.
-- |
-- | Note: it is imperative that the TVars are not updated *too* frequently.
-- | The transactions that depend on them are extremely fast,
-- | but if either of the TVars updates faster than the transaction can commit
-- | the entire transaction will be retried (which may block it until the writer terminates).

{-# INLINE generate #-}
generate
  :: (TVar Int -> TVar (Dynamic.Vector RealWorld Int) -> IO ())
  -> IO Vector
generate writer = do
  v <- stToIO $ Dynamic.new 4096
  consumedTVar <- newTVarIO 0
  vectorTVar   <- newTVarIO v
  forkIO $ writer consumedTVar vectorTVar
  let modifier = 0
      start  = 0
      span = maxBound
  return $ Vector { modifier, start, span, consumedTVar, vectorTVar }

-- | A consequence of our conventions with file positions preceding characters
-- | is that Position 0 0 (the start of the first line in the document)
-- | does not actually precede a newline character.
-- |
-- | To correct for this, we add a virtual newline at Position -1 0 at initialization.
-- |
-- | Note: it is imperative that the TVars are not updated *too* frequently.
-- | The transactions that depend on them are extremely fast,
-- | but if either of the TVars updates faster than the transaction can commit
-- | the entire transaction will be retried (which may block it until the writer terminates).
-- |
-- | Currently, scanNewlines consumes 32KB of the file
-- | before updating the TVar, which should give the
-- | simple arithmetic operations in the transactions
-- | enough time to commit (or explicitly retry).

{-# INLINE generateNewlines #-}
generateNewlines
  :: FilePath
  -> IO Vector
generateNewlines path = do
  v <- stToIO $ Dynamic.new 4096
  consumedTVar <- newTVarIO 0
  vectorTVar   <- newTVarIO <=< stToIO $ Dynamic.append v (-1)
  forkIO $ scanNewlines path consumedTVar vectorTVar
  let modifier = 0
      start  = 0
      span = maxBound
  return $ Vector { modifier, start, span, consumedTVar, vectorTVar }

{-# INLINE singleton #-}
singleton
  :: Int
  -> Vector
singleton = fromVectorTVar . V.singleton



-- * O(1) functions

{-# INLINE drop #-}
drop
  :: Int
  -> Vector
  -> Vector
drop n async@Vector{ start, span } = async { start = start + n, span = max 0 $ span - n }

{-# INLINE map #-}
map
  :: Int
  -> Vector
  -> Vector
map n async = async { modifier = async.modifier + n }

{-# INLINE splitAt #-}
splitAt
  :: Int
  -> Vector
  -> (Vector, Vector)
splitAt n v = (take n v, drop n v)

{-# INLINE take #-}
take
  :: Int
  -> Vector
  -> Vector
take n async@Vector{ span } = async { span = min span n }



-- * Functions that block until they have enough input

-- | Cheap enough that we don't realistically expect this to get interrupted.

{-# INLINE (!) #-}
(!)
  :: Vector
  -> Int
  -> Int
(!) Vector{ modifier, start, span, consumedTVar, vectorTVar } n
  | n < 0 = error "index for (!) is negative"
  | n >= start + span = error "index for (!) exceeds range of vector"
  | otherwise = unsafePerformSTM $ do
      consumed <- readTVar consumedTVar
      Dynamic.Vector length mvector <- readTVar vectorTVar
      if (consumed == maxBound || n < length - start) then do
        entry <- unsafeIOToSTM . stToIO $ MV.read mvector (start + n)
        return $ modifier + entry
      else
        retry

{-# INLINE (!?) #-}
(!?)
  :: Vector
  -> Int
  -> Maybe Int
(!?) avector@Vector{ start, span } n
  | n < 0             = Nothing
  | n >= start + span = Nothing
  | otherwise = Just $ avector ! n

-- | Compare the vector's length to the given number,
-- | returning GT if the vector has a greater length.
-- | Strongly prefer this to forceLength where possible,
-- | since this only blocks until enough of the vector has been generated
-- | to be able to decide the comparison.
-- |
-- | Note that GT can return (much) earlier than LT or EQ,
-- | because you cannot return LT or EQ unless the vector is forced.
-- |
-- | Cheap enough we don't realistically expect this to get interrupted.

{-# INLINE compareLength #-}
compareLength
  :: Vector
  -> Int
  -> Ordering
compareLength Vector{ start, span, consumedTVar, vectorTVar } n
  | span <= 0 = compare 0 n
  | otherwise = unsafePerformSTM $ do
      consumed <- readTVar consumedTVar
      Dynamic.Vector length _ <- readTVar vectorTVar
      if (consumed == maxBound || length - start >= span) then
        return $ compare (min span $ length - start) n
      else if (min span (length - start) >= n) then
        return $ GT
      else
        retry

{-# INLINE head #-}
head
  :: Vector
  -> Int
head = (! 0)

-- | Decide whether the Vector is null.
-- | Blocks until enough of the vector has been calculated that
-- | an answer can be given.
-- |
-- | Cheap enough we don't realistically expect this to get interrupted.

{-# INLINE null #-}
null
  :: Vector
  -> Bool
null Vector{ start, span, consumedTVar, vectorTVar }
  | span <= 0 = True
  | otherwise = unsafePerformSTM $ do
      consumed <- readTVar consumedTVar
      Dynamic.Vector length _ <- readTVar vectorTVar
      if (length > start) then
        return $ False
      else if (consumed == maxBound || length >= start + span) then
        return $ True
      else
        retry



-- * Functions that force input

-- | Use unsafePerformIO rather than unsafeDupablePerformIO
-- | since this function allocates.
-- |
-- | It is vital here that the returned vector references a *different* mutable vector,
-- | which in this is obtained by the call to MV.generateM,
-- | since we cannot (ever) modify the underlying mutable vector after initialization.

{-# INLINE forceAppend #-}
forceAppend
  :: Vector
  -> Int
  -> Vector
forceAppend Vector{ modifier, start, span, consumedTVar, vectorTVar } n = unsafePerformIO $ do
  mvar <- newEmptyMVar
  forkIO . putMVar mvar <=< atomically $ do
    consumed <- readTVar consumedTVar
    vector@(Dynamic.Vector length _) <- readTVar vectorTVar
    if (consumed == maxBound || length >= start + span) then
      return $ vector
    else
      retry
  vector'@(Dynamic.Vector length' _) <- cloneMap =<< takeMVar mvar
  consumedTVar' <- newTVarIO maxBound
  vectorTVar' <- newTVarIO vector'
  return $ Vector { modifier = 0, start = 0, span = length', consumedTVar = consumedTVar', vectorTVar = vectorTVar' }
  where cloneMap :: Dynamic.Vector RealWorld Int -> IO (Dynamic.Vector RealWorld Int)
        cloneMap (Dynamic.Vector l v) = do
            v' <- MV.generateM (1 + l') (go v)
            return $ Dynamic.Vector (1 + l') v'
          where l' = min span $ l - start
                go v i =
                  if (i == l') then
                    return $ n
                  else
                    (+ modifier) <$> MV.read v (start + i)

-- | Because we are allocating, we again want unsafePerformIO
-- | rather than unsafeDupablePerformIO.
-- |
-- | This is the naive way of concatenating (simply force all 3 vectors);
-- | we call it forceConcat3 rather than simply implementing Semigroup
-- | to emphasize that it's expensive, forces the vectors and should be used sparingly.
-- | That said, I think (<>) could be written to work without blocking
-- | by simply forking the copy operation. Need some time to think it through.
-- |
-- | (Update) The danger, of course, is that you may be tempted to use (<>)
-- | on the base line number array of a very large file. Forking the copy
-- | would prevent consumers blocking on it, but not the duplicated memory usage.

{-# INLINE forceConcat3 #-}
forceConcat3
  :: Vector
  -> Vector
  -> Vector
  -> Vector
forceConcat3 async1 async2 async3 = unsafePerformIO $ do
  mvar <- newEmptyMVar
  forkIO . putMVar mvar <=< atomically $ do
    consumed1 <- readTVar async1.consumedTVar
    consumed2 <- readTVar async2.consumedTVar
    consumed3 <- readTVar async3.consumedTVar
    waitUntil $ consumed1 == maxBound && consumed2 == maxBound && consumed3 == maxBound
    v1 <- readTVar async1.vectorTVar
    v2 <- readTVar async2.vectorTVar
    v3 <- readTVar async3.vectorTVar
    return $ (v1, v2, v3)
  (Dynamic.Vector l1' v1, Dynamic.Vector l2' v2, Dynamic.Vector l3' v3) <- takeMVar mvar
  let l1 = min async1.span $ l1' - async1.start
      l2 = min async2.span $ l2' - async2.start
      l3 = min async3.span $ l3' - async3.start
      l = l1 + l2 + l3
  vector <- MV.generateM l (go (l1,v1) (l2,v2) (l3,v3))
  consumedTVar <- newTVarIO maxBound
  vectorTVar <- newTVarIO $ Dynamic.Vector l vector
  return $ Vector { modifier = 0, start = 0, span = l, consumedTVar, vectorTVar }
  where go (l1,v1) (l2,v2) (_,v3) i =
          if (i < l1) then
            (+ async1.modifier) <$> MV.read v1 (async1.start + i)
          else if (i < l1 + l2) then
            (+ async2.modifier) <$> MV.read v2 (async2.start + i - l1)
          else
            (+ async3.modifier) <$> MV.read v3 (async3.start + i - l1 - l2)

-- | Cheap enough we don't realistically expect this to get interrupted.

{-# INLINE forceLast #-}
forceLast
  :: Vector
  -> Int
forceLast Vector{ modifier, start, span, consumedTVar, vectorTVar } = unsafePerformSTM $ do
  consumed <- readTVar consumedTVar
  Dynamic.Vector length vector <- readTVar vectorTVar
  if (consumed == maxBound || length >= start + span ) then
    (+ modifier) <$> (unsafeIOToSTM . stToIO $ MV.read vector $ (min length $ start + span) - 1)
  else
    retry

-- | Cheap enough we don't realistically expect this to get interrupted.

{-# INLINE forceLength #-}
forceLength
  :: Vector
  -> Int
forceLength Vector{ start, span, consumedTVar, vectorTVar } = unsafePerformSTM $ do
  consumed <- readTVar consumedTVar
  Dynamic.Vector length _ <- readTVar vectorTVar
  if (consumed == maxBound || length >= start + span ) then
    return $ min span $ length - start
  else
    retry



-- * Utilities

-- | Cheap enough we don't realistically expect this to get interrupted.
-- | This one is more expensive than the others.

{-# INLINE inVectorRange #-}
inVectorRange
  :: Int
  -> Vector
  -> Ordering
inVectorRange n Vector{ modifier, start, span, consumedTVar, vectorTVar } = unsafePerformSTM $ do
  consumed <- readTVar consumedTVar
  Dynamic.Vector length vector <- readTVar vectorTVar
  if (length <= start) then
    return $ EQ
  else do
    head <- (+ modifier) <$> (unsafeIOToSTM . stToIO $ MV.read vector start)
    last <- (+ modifier) <$> (unsafeIOToSTM . stToIO $ MV.read vector $ (min length $ start + span) - 1)
    if (n < head) then
      return $ LT
    else if (n <= last) then
      return $ EQ
    else if (consumed == maxBound || length >= start + span) then
      return $ GT
    else do
      retry

{-# INLINE scanNewlines #-}
scanNewlines
  :: FilePath
  -> TVar Int
  -> TVar (Dynamic.Vector RealWorld Int)
  -> IO ()
scanNewlines !path !consumedTVar !vectorTVar = do
  handle <- openFile path ReadMode
  loop 0 handle
  where loop :: Int -> Handle -> IO ()
        loop position handle = do
          chunk  <- Strict.hGetSome handle 32768
          if (Strict.null chunk) then
            atomically $ writeTVar consumedTVar maxBound
          else do
            scanNewlinesChunk position (position + Strict.length chunk) chunk consumedTVar vectorTVar
            loop (position + Strict.length chunk) handle

{-# INLINE scanNewlinesChunk #-}
scanNewlinesChunk
  :: Int                                 -- starting position (entries in vector written starting at this)
  -> Int                                 -- number to write to consumedTVar after finishing
  -> Strict.ByteString                   -- current bytestring chunk
  -> TVar Int
  -> TVar (Dynamic.Vector RealWorld Int)
  -> IO ()
scanNewlinesChunk !start !consumed !string@(Strict.BS _ length) !consumedTVar !vectorTVar = do
  vector <- readTVarIO vectorTVar
  loop 0 vector
  where loop :: Int -> Dynamic.Vector RealWorld Int -> IO ()
        loop offset vector =
          if (offset == length) then
            atomically $ do
              writeTVar consumedTVar consumed
              writeTVar vectorTVar   vector
          else
            -- 0x0a = fromIntegral '\n', i.e. LF
            if (next == 0x0a) then do
              vector' <- stToIO . Dynamic.append vector $ start + offset
              loop (offset + 1) vector'
            else
              loop (offset + 1) vector
          where next = Strict.index string offset

-- | This one has a risk of getting interrupted
-- | if we run the loop inside the STM transaction.
-- |
-- | The danger is that the transaction may get aborted
-- | if it takes longer to calculate the position in the vecotr
-- | than it does to scan a bytestring chunk.
-- |
-- | Since the latter is constant time while the former is not,
-- | it's entirely conceivable that the TVar gets rewritten before the transaction commit
-- | which would cause the entire transaction to retry.
-- |
-- | To avoid this, we exit the transaction as soon as we can
-- | and perform the loop outside of it.

{-# INLINE searchForPosition' #-}
searchForPosition'
  :: Position
  -> (Int -> Position)
  -> Vector
  -> Int
searchForPosition' position mkPosition Vector{modifier, start, span, consumedTVar, vectorTVar} = unsafePerformIO $ do
  vectorOrIndex <- forkSTM $ do
    consumed <- readTVar consumedTVar
    Dynamic.Vector length v <- readTVar vectorTVar
    let vector = MV.drop start v
        last_index = min (start + span) (length - start)
    last <- (+ modifier) <$> (unsafeIOToSTM . stToIO $ MV.read vector (max 0 $ last_index - 1))
    if (position <= mkPosition last) then
      return $ Left (vector, last_index)
    else if (consumed == maxBound || length >= start + span) then
      return $ Right last_index
    else
      retry
  case (vectorOrIndex) of
    Left (vector, last_index) -> loop last_index vector
    Right last_index          -> return last_index
  where loop :: Int -> MV.IOVector Int -> IO Int
        loop l v
          | l == 0 = return 0
          | l == 1 = do
              head <- (+ modifier) <$> MV.read v 0
              case (compare position $ mkPosition head) of
                GT -> return 1
                _  -> return 0
          | otherwise = do
              middle <- (+ modifier) <$> MV.read v k
              case (position < mkPosition middle) of
                True  -> loop k (MV.take k v)
                False -> (+ k) <$> loop (l - k) (MV.drop k v)
          where k = max (div (l - 1) 2) 1

-- | This one has a risk of getting interrupted
-- | if we run the loop inside the STM transaction.
-- |
-- | The danger is that the transaction may get aborted
-- | if it takes longer to calculate the position in the vecotr
-- | than it does to scan a bytestring chunk.
-- |
-- | Since the latter is constant time while the former is not,
-- | it's entirely conceivable that the TVar gets rewritten before the transaction commit
-- | which would cause the entire transaction to retry.
-- |
-- | To avoid this, we exit the transaction as soon as we can
-- | and perform the loop outside of it.

{-# INLINE searchForPosition_ #-}
searchForPosition_
  :: Position
  -> (Int -> Position)
  -> Vector
  -> Int
searchForPosition_ position mkPosition Vector{modifier, start, span, consumedTVar, vectorTVar} = unsafePerformIO $ do
  vectorOrIndex <- forkSTM $ do
    consumed <- readTVar consumedTVar
    Dynamic.Vector length v <- readTVar vectorTVar
    let vector = MV.drop start v
        last_index = min (start + span) (length - start)
    last <- (+ modifier) <$> (unsafeIOToSTM . stToIO $ MV.read vector (max 0 $ last_index - 1))
    if (position <= mkPosition last) then
      return $ Left (vector, last_index)
    else if (consumed == maxBound || length >= start + span) then
      return $ Right last_index
    else
      retry
  case (vectorOrIndex) of
    Left (vector, last_index) -> loop last_index vector
    Right last_index          -> return last_index
  where loop :: Int -> MV.IOVector Int -> IO Int
        loop l v
          | l == 0 = return 0
          | l == 1 = do
              head <- (+ modifier) <$> MV.read v 0
              case (compare position $ mkPosition head) of
                GT -> return 0
                _  -> return (-1)
          | otherwise = do
              middle <- (+ modifier) <$> MV.read v k
              case (position < mkPosition middle) of
                True  -> loop k (MV.take k v)
                False -> (+ k) <$> loop (l - k) (MV.drop k v)
          where k = max (div (l - 1) 2) 1

{-# INLINE waitUntil #-}
waitUntil
  :: Bool
  -> STM ()
waitUntil b =
  if b then
    return ()
  else
    retry



-- * Unsafe

{-# INLINE forkSTM #-}
forkSTM
  :: STM a
  -> IO a
forkSTM stm = do
  mvar <- newEmptyMVar
  forkIO $ atomically stm >>= putMVar mvar
  takeMVar mvar

-- | Forks an STM transaction into a new thread and waits on the result,
-- | to get around restrictions on using STM inside **PerformIO.
-- |
-- | Do NOT use inlinePerformIO here: it may indeed appear to work safely,
-- | but it very much is not.
-- | While the operations we perform on the arrays are indeed read-only,
-- | I'm almost certain inlinePerformIO won't enable CSE across (forked!) STM transactions.
-- | But it will probably expose newEmptyMVar to CSE, which would be...not ideal.
-- | What's worse, this will probably be undetectable until you do something
-- | where *multiple* calls to unsafePerformSTM block,
-- | which will likely never happen unless you start editing files well over 100 MB.
-- |
-- | unsafeDupablePerformIO should be safe however, since we really don't care
-- | semantically if the read-only transactions are aborted partway or duplicated.
-- |
-- | That said, I'm just using unsafePerformIO because
-- |   * any performance difference is probably imperceptible
-- |   * Forking threads is cheap, but I believe may also degrade performance
-- |     if too many threads are being scheduled simultaneously.
-- |     unsafePerformIO guarantees that the runtime won't fork threads unnecessarily.
-- |
-- | Note that if the forked transaction terminates before completing,
-- | takeMVar will never occur.
-- | GHC's runtime should be able to detect this and terminate the program,
-- | but in principle you may worry that this could block the main thread indefinitely.

{-# INLINE unsafePerformSTM #-}
unsafePerformSTM
  :: STM a
  -> a
unsafePerformSTM stm = unsafeDupablePerformIO $ do
  mvar <- newEmptyMVar
  forkIO $ atomically stm >>= putMVar mvar
  takeMVar mvar
