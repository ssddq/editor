{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Data.Tree.ByteString where

import Utils

import Data.Tree.Common

import Data.ByteString qualified as Strict

{-# INLINE compareToRange #-}
compareToRange
  :: Int
  -> (Int, Int)
  -> Ordering
compareToRange n (a, b) =
  if (n < a) then
    LT
  else if (n < a + b) then
    EQ
  else
    GT

{-# INLINE insertByteTree #-}
insertByteTree
  :: Strict.ByteString
  -> Int
  -> ByteTree
  -> ByteTree
insertByteTree !string !offset !tree = makeBlack $ insert string offset tree
  where
    insert :: Strict.ByteString -> Int -> ByteTree -> ByteTree
    insert !a !offset !tree@Tree{ index, color, left, node, right } =
      if (Strict.null a || offset < 0) then
        tree
      else case (compareToRange offset (index, Strict.length node + 1)) of
        LT -> balance (index + Strict.length a) color (insert a offset left) node (right                 )
        GT -> balance (index                  ) color (left                ) node (insert a offset' right)
              where offset' = offset - index - Strict.length node
        EQ -> prependRight node'' . prependRight node''' $ balance index color left node' right
              where (node', node'', node''') = insertSegment a (offset - index) node
    insert a _ nil@(Nil _) =
      if (Strict.null a) then
        nil
      else
        Tree 0 Red nil a nil
    makeBlack :: Tree Strict.ByteString -> Tree Strict.ByteString
    makeBlack (Tree index _ left node right) = Tree index Black left node right
    makeBlack (Nil k)                        = Nil $ max k 1

{-# INLINE removeByteTree #-}
removeByteTree
  :: Tree Strict.ByteString
  -> Int                    -- offset
  -> Int                    -- count
  -> Tree Strict.ByteString
removeByteTree !tree !offset !count = remove offset count tree
  where
    start = offset
    stop = offset + count
    remove :: Int -> Int -> Tree Strict.ByteString -> Tree Strict.ByteString
    remove _ 0 tree = tree
    remove _ _ nil@(Nil _) = nil
    remove !offset !count !Tree{ index, color, left, node, right } =
      case (compareToRange start (index, Strict.length node)) of
        LT -> case (compareToRange stop (index, Strict.length node)) of
                GT -> deleteNode index' color (remove offset left_count left) (remove 0 right_count right)
                _  -> resolve $ Tree index' color (remove offset left_count left) (Strict.drop (stop - index) node) (right)
                where left_count = min count (index - offset)
                      index' = index - left_count
        EQ -> if (isEmpty node') then
                deleteNode index color left (remove 0 right_count right)
              else if (isEmpty node'') then
                resolve $ Tree index color left node' (remove 0 right_count right)
              else
                prependRight node'' $ resolve $ Tree index color left node' right
        GT -> resolve $ Tree index color left node (remove (offset - index - Strict.length node) count right)
      where right_count = max 0 $ stop - index - Strict.length node
            (node', node'') = removeSegment (offset - index) (min count $ Strict.length node - offset - index) node
