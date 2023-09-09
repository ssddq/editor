{-# LANGUAGE BangPatterns #-}

module Data.Vector.Dynamic where

import Control.Monad.ST

import Data.Vector.Unboxed         qualified as V
import Data.Vector.Unboxed.Mutable qualified as MV


-- vector must have length at least 2; this is not checked
-- and will cause errors/segfault if violated.

data Vector s a = Vector {-# UNPACK #-} !Int
                         !(MV.STVector s a)


{-# INLINE append #-}
append
  :: (MV.Unbox a)
  => Vector s a
  -> a
  -> ST s (Vector s a)
append (Vector n v) a =
  if (n < MV.length v) then do
    MV.write v n a
    return $ Vector (n+1) v
  else do
    -- implicitly assuming that v has length >= 2, which we ensure in new
    -- but could be violated by manual construction or other operations.
    let l = div (MV.length v) 2
    v' <- MV.unsafeGrow v l
    MV.write v' n a
    return $ Vector (n+1) v'

-- Freezes a Vector by copying.

{-# INLINE freeze #-}
freeze
  :: (MV.Unbox a)
  => Vector s a
  -> ST s (V.Vector a)
freeze (Vector n v) = V.freeze $ MV.take n v

-- Freezes a Vector by copying.

{-# INLINE freezeIO #-}
freezeIO
  :: (MV.Unbox a)
  => Vector RealWorld a
  -> ST RealWorld (V.Vector a)
freezeIO (Vector n v) = V.freeze $ MV.take n v

{-# INLINE length #-}
length
  :: Vector s a
  -> Int
length (Vector n _) = n

{-# INLINE new #-}
new
  :: (MV.Unbox a)
  => Int
  -> ST s (Vector s a)
new !n = Vector 0 <$> MV.new (max n 64)

-- Freezes a Vector in place (i.e. without copying).
-- The Vector should not be mutated after freezing.

{-# INLINE unsafeFreeze #-}
unsafeFreeze
  :: (MV.Unbox a)
  => Vector s a
  -> ST s (V.Vector a)
unsafeFreeze (Vector n v) = V.unsafeFreeze $ MV.take n v
