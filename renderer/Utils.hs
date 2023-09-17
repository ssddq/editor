{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Utils
  ( module Data.Time.Clock.System
  , module Utils
  ) where

import Common
import VMA

import Control.Monad.State.Strict

import Data.Time.Clock.System
import Data.Vector            qualified as V

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple ()

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Internal
import Graphics.Vulkan.Marshal.Create

(<.>)
  :: (Functor f)
  => (b -> c)
  -> (a -> f b)
  -> (a -> f c)
(<.>) g h = (fmap g) . h

{-# INLINE p #-}
p
  :: a ~ VkStruct b
  => a
  -> Ptr a
p = unsafePtr

{-# INLINE perform #-}
perform
  :: (Storable a)
  => (Ptr a -> IO b)
  -> IO a
perform f = do
  ptr <- malloc
  f ptr
  a   <- peek ptr
  free ptr
  return a

perform3
  :: (Storable a, Storable b, Storable c)
  => (Ptr a -> Ptr b -> Ptr c -> IO d)
  -> IO (a, b, c)
perform3 f = do
  ptrA <- malloc
  ptrB <- malloc
  ptrC <- malloc
  f ptrA ptrB ptrC
  a <- peek ptrA
  b <- peek ptrB
  c <- peek ptrC
  free ptrA
  free ptrB
  free ptrC
  return (a,b,c)

performArray
  :: (Storable a)
  => Int
  -> (Ptr a -> IO b)
  -> IO (V.Vector a)
performArray n f = do
  ptr <- mallocArray n
  f ptr
  a <- peekArray n ptr
  free ptr
  return $ V.fromList a

performEnumerate
  :: (Storable a, Storable n, Integral n)
  => (Ptr n -> Ptr a -> IO b)
  -> IO [a]
performEnumerate f = do
  pCount <- malloc
  f pCount VK_NULL
  count  <- peek pCount
  pEnum  <- mallocArray (fromIntegral count)
  f pCount pEnum
  enum   <- peekArray (fromIntegral count) pEnum
  free pCount
  free pEnum
  return enum

traverseUntil
  :: (Monad m)
  => (a -> (StateT s m) b)
  -> [a]
  -> (s -> Bool)
  -> (StateT s m) s
traverseUntil _ [] _ = get
traverseUntil f (a : as) condition = do
  state <- get
  case (condition state) of
    False -> do f a
                traverseUntil f as condition
    True  -> return $ state

with2
  :: (Storable a, Storable b)
  => a
  -> b
  -> (Ptr a -> Ptr b -> IO c)
  -> IO c
with2 a b f = do
  ptrA <- malloc
  ptrB <- malloc
  poke ptrA a
  poke ptrB b
  c <- f ptrA ptrB
  free ptrA
  free ptrB
  return c

{-# INLINE withVector #-}
withVector
  :: (Storable a)
  => V.Vector a
  -> (Int -> Ptr a -> IO b)
  -> IO b
withVector v f = withArray (V.toList v) (f $ V.length v)

{-# INLINE mkOffset2D #-}
mkOffset2D :: Int32 -> Int32 -> VkOffset2D
mkOffset2D x y = createVk @VkOffset2D
  $ set @"x" |* x
 &* set @"y" |* y

{-# INLINE mkExtent2D #-}
mkExtent2D :: Word32 -> Word32 -> VkExtent2D
mkExtent2D w h = createVk @VkExtent2D
  $ set @"width"  |* w
 &* set @"height" |* h

{-# INLINE mkRect2D #-}
mkRect2D :: (Int32, Int32) -> (Word32, Word32)-> VkRect2D
mkRect2D (x,y) (w,h) = createVk @VkRect2D
  $ set @"offset" |* mkOffset2D x y
 &* set @"extent" |* mkExtent2D w h


writeAllocation
  :: (Storable a)
  => [a]
  -> VmaAllocator
  -> VmaAllocation
  -> IO ()
writeAllocation list allocator allocation = do
  pMemory <- perform $ vmaMapMemory
                         |- allocator
                         |- allocation
  pokeArray (castPtr pMemory) list
  vmaUnmapMemory
    |- allocator
    |- allocation
