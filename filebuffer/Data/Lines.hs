{-# LANGUAGE OverloadedRecordDot #-}

module Data.Lines where

import Common
import Utils

import Data.ByteString   qualified as Strict
import Data.Vector.Async qualified as Async

{-# INLINE inRange #-}
inRange
  :: Position
  -> Lines
  -> Ordering
inRange position (Base v) = Async.inVectorRange position.base v
inRange position (Offset n _) = compare position.base n


data Lines
  = Base   { vector :: {-# UNPACK #-} !(Async.Vector)
           }
  | Offset { base   :: {-# UNPACK #-} !Int
           , vector :: {-# UNPACK #-} !(Async.Vector)
           }
  deriving (Eq, Show)


instance Contiguous Lines where
  {-# INLINE forceLength #-}
  forceLength lines = Async.forceLength lines.vector
  {-# INLINE removeSegment #-}
  removeSegment = \start len a -> case a of
    Base v -> (Base head, Base $ Async.drop len tail)
      where (head, tail) = Async.splitAt start v
    Offset n v -> (Offset n head, Offset n $ Async.drop len tail)
      where (head, tail) = Async.splitAt start v
  {-# INLINE empty #-}
  empty = Base Async.emptyVector
  {-# INLINE isEmpty #-}
  isEmpty = \a -> case a of
    Base     v -> Async.null v
    Offset _ v -> Async.null v



{-# INLINE bumpEmpty #-}
bumpEmpty
  :: (Lines, Lines, Lines)
  -> (Lines, Lines, Lines)
bumpEmpty (a,b,c) = case (isEmpty a, isEmpty b, isEmpty c) of
  (True, True, True)  -> (a,b,c)
  (True, True, False) -> (c, empty, empty)
  (True, False, _)    -> (b, c, empty)
  (False, True, _ )   -> (a, c, empty)
  (False, False, _ )  -> (a, b, c)

{-# INLINE bumpEmpty2 #-}
bumpEmpty2
  :: (Lines, Lines, a)
  -> (Lines, Lines, a)
bumpEmpty2 (a,b,c) =
  if (isEmpty a) then
    (b, empty, c)
  else
    (a, b, c)

-- | findIndex' returns the 0-based index in the array
-- | rounded up.

{-# INLINE findIndex' #-}
findIndex'
  :: Lines
  -> Position
  -> Int
findIndex' (Base v) position = Async.searchForPosition' position (\base -> Position base 0) v
findIndex' (Offset n v) position = case (compare position.base n) of
  EQ -> Async.searchForPosition' position (\offset -> Position position.base offset) v
  LT -> 0
  GT -> Async.forceLength v

-- | findIndex_ returns the 0-based index in the array
-- | rounded down.

{-# INLINE findIndex_ #-}
findIndex_
  :: Lines
  -> Position
  -> Int
findIndex_ (Base v) position = Async.searchForPosition_ position (\base -> Position base 0) v
findIndex_ (Offset n v) position = case (compare position.base n) of
  EQ -> Async.searchForPosition_ position (\offset -> Position position.base offset) v
  LT -> (-1)
  GT -> Async.forceLength v - 1

{-# INLINE insertLines #-}
insertLines
  :: Position
  -> Strict.ByteString
  -> Lines
  -> (Lines, Lines, Lines)
insertLines position string target@(Base v) =
  if (Async.null tail) then
    bumpEmpty $ (Base head, insertLines, empty)
  else if (position.base == tail Async.! 0) then
    bumpEmpty $ (Base head, Offset position.base $ Async.forceAppend insertVector $ Strict.length string, Base $ Async.drop 1 tail)
  else
    bumpEmpty $ (Base head, insertLines, Base tail)
  where (head, tail) = Async.splitAt index v
        index = findIndex' target position
        insertVector = Async.findLineOffsets position.offset string
        insertLines = Offset position.base insertVector
insertLines position string target@(Offset n v) = case (compare position.base n) of
  LT -> bumpEmpty $ (insertLines, target, empty)
  GT -> bumpEmpty $ (target, insertLines, empty)
  EQ -> bumpEmpty $ (Offset n $ Async.forceConcat3 head insertVector $ Async.map (Strict.length string) tail, empty, empty)
  where (head, tail) = Async.splitAt index v
        index = findIndex' target position
        insertVector = Async.findLineOffsets position.offset string
        insertLines = Offset position.base insertVector

-- | skipFromTo returns the head and tail of the given array
-- | with the lines between the corresponding positions removed.
-- |
-- | The start position is removed if present,
-- | while the stop position is never removed.

{-# INLINE skipFromTo #-}
skipFromTo
  :: Position
  -> Position
  -> Lines
  -> (Lines, Lines, Int)
skipFromTo start stop array@(Base v) = case (Async.compareLength v deleteCount) of
    GT -> bumpEmpty2 $ (Base head, Base $ Async.drop deleteCount tail, deleteCount)
    _  -> bumpEmpty2 $ (empty, empty, Async.forceLength v)
  where n = findIndex' array start
        m = findIndex_ array stop
        deleteCount =
          case (Async.compareLength v m) of
            GT -> m - n + 1
            _  -> Async.forceLength v - n
        (head, tail) = Async.splitAt n v
skipFromTo start stop array@(Offset k v) = case (Async.compareLength v deleteCount) of
    GT -> bumpEmpty2 $ (Offset k $ Async.forceConcat3 head (Async.map (start.offset - stop.offset) $ Async.drop deleteCount tail) Async.emptyVector, empty, deleteCount)
    _  -> bumpEmpty2 $ (empty, empty, Async.forceLength v)
  where n = findIndex' array start
        m = findIndex_ array stop
        deleteCount =
          case (Async.compareLength v m) of
            GT -> m - n + 1
            _  -> Async.forceLength v - n
        (head, tail) = Async.splitAt n v
