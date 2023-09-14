{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE UnboxedTuples       #-}

module Utils where

import Common

import Unsafe.ByteString

import Data.Bits

import GHC.Int
import GHC.Prim
import GHC.Word

import Data.ByteString          qualified as Strict
import Data.ByteString.Internal qualified as Strict
import Data.IntMap.Strict       qualified as Map

import Data.Vector.Async qualified as Async


{-# INLINE diff #-}
diff
  :: Strict.ByteString
  -> Strict.ByteString
  -> Strict.ByteString
diff !full !tail = Strict.take (Strict.length full - Strict.length tail) full

data ByteStringColored = ByteStringColored
  { string :: {-# UNPACK #-} !Strict.ByteString
  , color  :: {-# UNPACK #-} !Color
  }


data CompletedMatches
  = Match {-# UNPACK #-} !Position
          {-# UNPACK #-} !Word8
          {-# UNPACK #-} !Int
          !CompletedMatches
  | CNil

data DecodeState
  = Byte0 {-# UNPACK #-} !Int
  | Byte1 {-# UNPACK #-} !Int
          {-# UNPACK #-} !Word8
  | Byte2 {-# UNPACK #-} !Int
          {-# UNPACK #-} !Word8
          {-# UNPACK #-} !Word8
  | Byte3 {-# UNPACK #-} !Int
          {-# UNPACK #-} !Word8
          {-# UNPACK #-} !Word8
          {-# UNPACK #-} !Word8

data Decoded = Decoded {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Int
  deriving (Eq, Show)

data DeleteState = DeleteState
  { position  :: {-# UNPACK #-} !Position
  , remaining :: {-# UNPACK #-} !Int
  , extras    :: {-# UNPACK #-} !Int
  }
  deriving (Show)

data ForwardMatches
  = PartialMatch {-# UNPACK #-} !Position
                 {-# UNPACK #-} !Word8
                 {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Int
                 !ForwardMatches
  | FNil

-- | Performed measurably better than a tuple in some (non-rigorous) tests.

data I2 = I2 {-# UNPACK #-} !Int
             {-# UNPACK #-} !Int
  deriving (Eq, Show)

data MatchState = MatchState
  { offset  :: {-# UNPACK #-} !Int               -- offset into current bytestring chunk
  , done    :: CompletedMatches
  , forward :: ForwardMatches
  }

data StreamState = StreamState {-# UNPACK #-} !Int
                               {-# UNPACK #-} !Int
                               {-# UNPACK #-} !Int  -- stream limit for segment, maxBound :: Int to start, lowered in branches
  deriving (Show)


class Contiguous a where
  removeSegment :: Int -> Int -> a -> (a,a)
  forceLength :: a -> Int
  empty :: a
  isEmpty :: a -> Bool

instance Contiguous Strict.ByteString where
  {-# INLINE removeSegment #-}
  removeSegment = \a b bs -> (unsafeRemoveByteString a b bs, empty)
  {-# INLINE empty #-}
  empty = Strict.empty
  {-# INLINE isEmpty #-}
  isEmpty = Strict.null
  {-# INLINE forceLength #-}
  forceLength = Strict.length


-- | Clamped indexing into a vector,
-- | wrapping to the first or last element if out of bounds.

{-# INLINE (>!<) #-}
(>!<)
  :: Async.Vector
  -> Int
  -> Int
(>!<) v n =
  if (n <= 0) then
    Async.head v
  else
    case (Async.compareLength v n) of
      GT -> v Async.! n
      _  -> v Async.! (Async.forceLength v - 1)

{-# INLINE InLeft #-}
pattern InLeft :: (Bool, Bool)
pattern InLeft = (True, False)

{-# INLINE InRight #-}
pattern InRight :: (Bool, Bool)
pattern InRight = (False, True)

-- | Calculate the divider (between the left/right subtrees) given a prefix and switch.

{-# INLINE calculateDivider #-}
calculateDivider
  :: Int
  -> Int
  -> Int
calculateDivider !prefix !switch = (prefix .&. shiftL (-switch) 1) + (switch)

-- | Create an IntMap of skips which allows you to lookup how far you can advance
-- | a pointer into the bytestring given the current character.
-- |
-- | I'm actually not sure why I chose an IntMap; it's pretty objectively the wrong structure.
-- | Note that matching is decided at the byte level, not the code point level.
-- | Hence, we could simply use an array of size 2^8 to hold the skip values for all possible bytes.
-- | With 1 machine word (8 bytes) per entry, this is 2 KB of memory -- completely trivial.
-- |
-- | (I think I was worried about the ~8 MB it would take to do this for code points rather than bytes;
-- | holding off on fixing because I'm slightly curious about the performance impact
-- | so I want to benchmark it first).

{-# INLINE calculateSkips #-}
calculateSkips
  :: Strict.ByteString
  -> Map.IntMap Int
calculateSkips target@(Strict.BS _ len) = snd $ Strict.foldl' insertWord (1, Map.empty) target
  where insertWord (n, t) w = (n+1, Map.insert (fromIntegral w) (max (len - n) 1) t)

{-# INLINE commonPrefix #-}
commonPrefix
  :: Int
  -> Int
  -> I2  -- (Int, Int)
commonPrefix !prefix1 !prefix2 =
  I2 ((-switch) .&. (min prefix1 prefix2)) switch
  where switch = rotateR highestBit64 $ countLeadingZeros $ prefix1 `xor` prefix2

{-# INLINE decodeUtf8 #-}
decodeUtf8
  :: Word8
  -> Word8
  -> Word8
  -> Word8
  -> Decoded
decodeUtf8 !(W8# a) !(W8# b) !(W8# c) !(W8# d) = Decoded (W32# w) (I# i)
  where !(# w, i #) = decodeUtf8# a b c d

-- | Decodes a 4 byte sequence to the first UTF-8 codepoint,
-- | returning the advance (i.e. the number of bytes decoded).

{-# INLINE decodeUtf8# #-}
decodeUtf8#
  :: Word8#
  -> Word8#
  -> Word8#
  -> Word8#
  -> (# Word32#, Int# #)
decodeUtf8# !byte1 !byte2 !byte3 !byte4 =
  (# uncheckedShiftRLWord32# (word1 `orWord32#` word2 `orWord32#` word3 `orWord32#` word4) shift, leadingZeros #)
  where word1 = uncheckedShiftLWord32# (wordToWord32# (word8ToWord# (byte1 `andWord8#` mask1  ))) 18#
        word2 = uncheckedShiftLWord32# (wordToWord32# (word8ToWord# (byte2 `andWord8#` mask234))) 12#
        word3 = uncheckedShiftLWord32# (wordToWord32# (word8ToWord# (byte3 `andWord8#` mask234))) 6#
        word4 =                        (wordToWord32# (word8ToWord# (byte4 `andWord8#` mask234)))
        -- guard ensures that clz# does not return a result > 7, which would result in undefined behavior.
        -- in particular, this ensures that the advance returned is always guaranteed to be >= 1.
        leadingZeros = word2Int# (clz8# (word8ToWord# ((byte1 `xorWord8#` allOnes) `orWord8#` guard)))
        shift = word2Int# (word8ToWord# ( wordToWord8# ( word64ToWord# (uncheckedShiftRL64# utf8Shifts (leadingZeros *# 8#)))))
        !(W8#  allOnes   ) = (0b11111111 :: Word8)
        !(W8#  guard     ) = (0b00000001 :: Word8)
        !(W8#  mask234   ) = (0b00111111 :: Word8)
        !(W64# utf8Shifts) = (0b00000110000011000000000000010010 :: Word64)
        !(W64# mask      ) = (0b0000011100001111000111110000000011111111 :: Word64)
        mask1 = wordToWord8# (word64ToWord# (uncheckedShiftRL64# mask (leadingZeros *# 8#)))

-- | The binary literal
-- |
-- | 0b1000000000000000000000000000000000000000000000000000000000000000
-- |
-- | is half the maximum *unsigned* integer (i.e. = maxBound @Int + 1)
-- | As a signed integer, this overflows to the correct thing.
-- | To avoid overflowed literal warnings, we can take its negative
-- | which is the Int whose signed twos-complement binary representation is
-- |
-- | 1000000000000000000000000000000000000000000000000000000000000000

{-# INLINE highestBit64 #-}
highestBit64 :: Int
highestBit64 = -0b1000000000000000000000000000000000000000000000000000000000000000

{-# INLINE inLeftRight #-}
inLeftRight
  :: Int
  -> Int
  -> Int
  -> (Bool, Bool)
inLeftRight key first switch =
  case ((key `xor` first) .&. mask == zeroBits) of
    False -> (False, False)
    True  -> ( (key .&. switch) == zeroBits
             , (key .&. switch) /= zeroBits
             )
  where mask = -(shiftL switch 1)

{-# INLINE insertSegment #-}
insertSegment
  :: Strict.ByteString
  -> Int
  -> Strict.ByteString
  -> (Strict.ByteString, Strict.ByteString, Strict.ByteString)
insertSegment = unsafeInsertByteString
