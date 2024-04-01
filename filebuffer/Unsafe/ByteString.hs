module Unsafe.ByteString where

import Common

import Data.ByteString          qualified as Strict
import Data.ByteString.Internal qualified as Strict

import Foreign.Marshal.Utils

import GHC.ForeignPtr

import System.IO.Unsafe

-- | Size (in bytes) at which bytestrings get split.
-- | Note that when we start tracking edit history,
-- | this number may well be the memory cost of each edit
-- | in normal use cases on a large file. Ideally, we want
-- | this to not be too large or too small.

{-# INLINE blockSizeLimit #-}
blockSizeLimit :: Int
blockSizeLimit = 1024

-- | This function is very unsafe!
-- | Inserts one bytestring into another at the given offset,
-- | returning overflow if the resulting bytestring exceeds blockSizeLimit.

{-# INLINE unsafeInsertByteString #-}
unsafeInsertByteString
  :: Strict.ByteString
  -> Int
  -> Strict.ByteString
  -> (Strict.ByteString, Strict.ByteString, Strict.ByteString)
unsafeInsertByteString (Strict.BS insertFp insertLen) offset (Strict.BS targetFp targetLen) = unsafeDupablePerformIO $ do
  let memcpy target source length = unsafeWithForeignPtr target
                                  $ \t -> unsafeWithForeignPtr source
                                  $ \s -> copyBytes t s length
      totalLen = insertLen + targetLen
  if (totalLen <= blockSizeLimit) then
    do let len = totalLen
       fp <- Strict.mallocByteString len
       memcpy
         |- fp
         |- targetFp
         |- offset
       memcpy
         |- fp `plusForeignPtr` offset
         |- insertFp
         |- insertLen
       memcpy
         |- fp `plusForeignPtr` (offset + insertLen)
         |- targetFp `plusForeignPtr` offset
         |- targetLen - offset
       return $ (Strict.BS fp len, Strict.empty, Strict.empty)
  else
    do let len1 = div totalLen 2
           len2 = totalLen - len1
           insertEnd = offset + insertLen
       fp1 <- Strict.mallocByteString len1
       fp2 <- Strict.mallocByteString len2
       if (offset < len1) then
         if (insertEnd > len1) then
           do memcpy
                |- fp1
                |- targetFp
                |- offset
              let a = len1 - offset
              memcpy
                |- fp1 `plusForeignPtr` offset
                |- insertFp
                |- a
              let b = insertEnd - len1
              memcpy
                |- fp2
                |- insertFp `plusForeignPtr` a
                |- b
              memcpy
                |- fp2 `plusForeignPtr` b
                |- targetFp `plusForeignPtr` offset
                |- targetLen - offset
         else
           do memcpy
                |- fp1
                |- targetFp
                |- offset
              memcpy
                |- fp1 `plusForeignPtr` offset
                |- insertFp
                |- insertLen
              let a = len1 - insertEnd
              memcpy
                |- fp1 `plusForeignPtr` insertEnd
                |- targetFp `plusForeignPtr` offset
                |- a
              memcpy
                |- fp2
                |- targetFp `plusForeignPtr` a
                |- len2
       else
         do memcpy
              |- fp1
              |- targetFp
              |- len1
            let a = offset - len1
            memcpy
              |- fp2
              |- targetFp `plusForeignPtr` len1
              |- a
            memcpy
              |- fp2 `plusForeignPtr` a
              |- insertFp
              |- insertLen
            memcpy
              |- fp2 `plusForeignPtr` (targetLen - offset)
              |- targetFp `plusForeignPtr` offset
              |- targetLen
       return (Strict.BS fp1 len1, Strict.BS fp2 len2, Strict.empty)

-- | This function is very unsafe!
-- | In particular, it is not checked whether the offset+count is indeed smaller than stringLen,
-- | or whether the offset is even positive.
-- | This function will segfault if the inputs are not sensible.

{-# INLINE unsafeRemoveByteString #-}
unsafeRemoveByteString
  :: Int
  -> Int
  -> Strict.ByteString
  -> Strict.ByteString
unsafeRemoveByteString offset count (Strict.BS stringFp stringLen) = unsafeDupablePerformIO $ do
  let len = stringLen - count
      memcpy target source length = unsafeWithForeignPtr target
                                  $ \t -> unsafeWithForeignPtr source
                                  $ \s -> copyBytes t s length
  fp <- Strict.mallocByteString len
  memcpy
    |- fp
    |- stringFp
    |- offset
  memcpy
    |- fp `plusForeignPtr` offset
    |- stringFp `plusForeignPtr` (offset + count)
    |- len - offset
  return $ Strict.BS fp len
