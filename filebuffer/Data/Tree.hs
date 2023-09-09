module Data.Tree
  ( module Data.Tree
  , module Data.Tree.ByteString
  , module Data.Tree.Common
  , module Data.Tree.Lines
  ) where

import Prelude hiding (drop)

import Data.Tree.ByteString
import Data.Tree.Common
import Data.Tree.Lines

{-# INLINE new #-}
new
  :: a
  -> Tree a
new a = Tree 0 Black (Nil Black) a (Nil Black)
