{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Data.Tree.Lines where

import Common
import Utils

import Data.Tree.Common

import Data.Lines

import Data.ByteString   qualified as Strict
import Data.Vector.Async qualified as Async

{-# INLINE findLineNumber #-}
findLineNumber
  :: Position
  -> Tree Lines
  -> Int
findLineNumber !position !Tree{ index, left = Nil{}, node, right = Nil{} } = case (inRange position node) of
  EQ -> index + findIndex' node position
  LT -> index
  GT -> index + forceLength node
findLineNumber !position !Tree{ index, left = Nil{}, node, right } = case (inRange position node) of
  EQ -> index + findIndex' node position
  LT -> index
  GT -> index + forceLength node + findLineNumber position right
findLineNumber !position !Tree{ index, left, node, right = Nil{} } = case (inRange position node) of
  EQ -> index + findIndex' node position
  LT -> findLineNumber position left
  GT -> index + forceLength node
findLineNumber !position !Tree{ index, left, node, right } = case (inRange position node) of
  EQ -> index + findIndex' node position
  LT -> findLineNumber position left
  GT -> index + forceLength node + findLineNumber position right
findLineNumber _ !Nil{} = error "findLineNumber called on nil; invalid"

{-# INLINE findLinePosition #-}
findLinePosition
  :: Int
  -> Tree Lines
  -> Position
findLinePosition !n !Tree{ index, left = Nil{}, node, right = Nil{} }
  | otherwise = case node of
      Base     v -> Position (v >!< offset) 0
      Offset k v -> Position k (v >!< offset)
  where offset  = n - index
findLinePosition !n !Tree{ index, left = Nil{}, node, right }
  | subindex >= 0 = findLinePosition subindex right
  | otherwise = case node of
      Base     v -> Position (v >!< offset) 0
      Offset k v -> Position k (v >!< offset)
  where offset  = n - index
        subindex = offset - forceLength node
findLinePosition !n !Tree{ index, left, node, right = Nil{} }
  | offset < 0 = findLinePosition n left
  | otherwise = case node of
      Base     v -> Position (v >!< offset) 0
      Offset k v -> Position k (v >!< offset)
  where offset  = n - index
findLinePosition !n !Tree{ index, left, node, right }
  | offset < 0 = findLinePosition n left
  | subindex >= 0 = findLinePosition subindex right
  | otherwise = case node of
      Base     v -> Position (v >!< offset) 0
      Offset k v -> Position k (v >!< offset)
  where offset  = n - index
        subindex = offset - forceLength node
findLinePosition _ !Nil{} = error "findLinePosition was called on nil; invalid"

{-# INLINE insertNewlines #-}
insertNewlines
  :: Strict.ByteString
  -> Position
  -> Tree Lines
  -> Tree Lines
insertNewlines !string !position !tree = makeBlack $ insert tree
  where
    lines = Async.findLineOffsets position.offset string
    lines_count = Async.forceLength lines
    insert :: Tree Lines -> Tree Lines
    insert !Tree{ index, color, left, node, right } = case (inRange position node) of
      LT -> balance (index + lines_count) color (insert left) node (right       )
      GT -> balance (index              ) color (left       ) node (insert right)
      EQ -> prependRight node'' . prependRight node''' $ Tree index color left node' right
            where (node', node'', node''') = insertLines position string node
    insert !nil@Nil{} =
      if (Async.null lines) then
        nil
      else
        Tree 0 Red nil (Offset position.base lines) nil
    makeBlack :: Tree Lines -> Tree Lines
    makeBlack !Tree{ index, left, node, right } = Tree index Black left node right
    makeBlack !Nil{ color }                     = Nil $ max color 1

-- | Remove newlines between two base positions,
-- | including the starting position and excluding the final position.

{-# INLINE removeLines #-}
removeLines
  :: Tree Lines
  -> Position   -- from
  -> Position   -- to
  -> Tree Lines
removeLines !tree !start !stop = fst $ remove tree
  where
    remove :: Tree Lines -> (Tree Lines, Int)
    remove !nil@Nil{} = (nil, 0)
    remove !Tree{index, color, left, node, right} =
      case (inRange start node) of
        GT -> ( resolve $ Tree index color left node right'
              , right_count
              )
        EQ -> if (isEmpty node') then
                ( deleteNode index color left right'
                , node_count + right_count
                )
              else
                ( prependRight node'' . resolve $ Tree index color left node' right'
                , node_count + right_count
                )
        LT -> case (inRange stop node) of
                GT -> ( deleteNode index' color left' right'
                      , left_count + node_count + right_count
                      )
                LT -> ( resolve $ Tree index' color left' node right
                      , left_count
                      )
                EQ -> if (isEmpty node') then
                        ( deleteNode index' color left' right
                        , left_count + node_count
                        )
                      else
                        ( prependRight node'' . resolve $ Tree index' color left' node' right
                        , left_count + node_count
                        )
      where (left' , left_count) = remove left
            (right', right_count) = remove right
            (node', node'', node_count) = skipFromTo start stop node
            index' = index - left_count
