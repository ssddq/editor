{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Data.Tree.Common where

import Utils

import Data.Lines

import Data.String
import Data.Word

import Data.ByteString       qualified as Strict
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy  qualified as Lazy


deriving instance Eq (Tree Lines)


-- | -------------------------------------------------------------------------------------------------
-- | class Tree' a where
-- |   data Tree a
-- |   nil :: Tree a -> Bool
-- |   apply :: (a -> Color -> Int -> Tree_ a -> Tree_ a -> b) -> (Color -> b) -> Tree a -> b
-- |
-- | instance Tree' Strict.ByteString where
-- |   data Tree Strict.ByteString
-- |     = Branch_ByteString {-# UNPACK #-} !Strict.ByteString
-- |                         {-# UNPACK #-} !Color
-- |                         {-# UNPACK #-} !Int
-- |                         !(Tree Strict.ByteString)
-- |                         !(Tree Strict.ByteString)
-- |     | Nil_ByteString {-# UNPACK #-} !Color
-- |   {-# INLINE nil #-}
-- |   nil Nil_ByteString{} = True
-- |   nil _ = False
-- |   {-# INLINE apply #-}
-- |   apply f _ (Branch_ByteString string color offset left right) = f string color offset left right
-- |   apply _ g (Nil_ByteString color) = g color
-- |
-- | instance Tree' Lines where
-- |   data Tree Lines
-- |     = Branch_Lines {-# UNPACK #-} !Lines
-- |                    {-# UNPACK #-} !Color
-- |                    {-# UNPACK #-} !Int
-- |                    !(Tree Lines)
-- |                    !(Tree Lines)
-- |     | Nil_Lines {-# UNPACK #-} !Color
-- |   {-# INLINE nil #-}
-- |   nil Nil_Lines{} = True
-- |   nil _ = False
-- |   {-# INLINE apply #-}
-- |   apply f _ (Branch_Lines string color offset left right) = f string color offset left right
-- |   apply _ g (Nil_Lines color) = g color
-- |
-- | -------------------------------------------------------------------------------------------------
-- |
-- | I would really like to unpack the node in the first constructor of Tree,
-- | but there isn't a good way to do this without code duplication or metaprogramming.
-- |
-- | Polymorphic fields can't be unpacked (of course), but we can instead create a typeclass
-- | with an associated data family whose instances can be defined with unpacked fields.
-- | See above for the sketch: if we inline everything, it should all get specialized away at runtime
-- |
-- | This is the same trick e.g. vector uses to provide unboxed vectors of various types,
-- | but unfortunately falls short for us. The issue is that performing any pattern matching
-- | requires associated pattern synonyms (see https://gitlab.haskell.org/ghc/ghc/-/issues/8583).
-- | Without that, we can provide helper functions which should be fine for most of what we need to write,
-- | but e.g. the function 'resolve' is a complete nightmare to write with something like apply as defined above.
-- |
-- | On a separate note, it might be worthwhile having a nil = Nil 1 constant to reuse for
-- | whenever a tree has a Nil 1 tip (which should happen almost always, except temporarily during insert/delete operations).
-- | Otherwise, each leaf node will probably be allocated separately with no sharing and take up a full machine word (?),
-- | costing 8n +/- O(1) bytes.

data Tree a
  = Tree { index :: {-# UNPACK #-} !Int
         , color :: {-# UNPACK #-} !Color
         , left  :: !(Tree a)
         , node  :: !a
         , right :: !(Tree a)
         }
  | Nil  { color :: {-# UNPACK #-} !Color     -- implicitly either black or higher (2-black, 3-black etc).
         }
  deriving (Show)


type ByteTree = Tree Strict.ByteString

type Color = Word8


instance Eq (Tree Strict.ByteString) where
  (==) tree1 tree2 = (toLazyByteString tree1 == toLazyByteString tree2)

instance IsString (Tree Strict.ByteString) where
  fromString string =
    Tree { index = 0
         , color = Black
         , left  = (Nil Black)
         , node  = (Char8.pack string)
         , right = (Nil Black)
         }


{-# INLINE Black #-}
pattern Black = 1
pattern Black :: Color

{-# INLINE Red #-}
pattern Red :: Color
pattern Red = 0

-- | The normal implementation of the following balance operations is that the original root is black, and the resulting root is red.
-- | to extend this to cases where the original root is k-black for k > 1, note that this will simply blacken the resulting root color k stages.
-- |
-- | A priori, there is a worry here that you may inadvertently force the length of the tail when rebalancing;
-- | but since the tail is always the rightmost node in the tree, note that it can never be `x` or `y` in any of the cases below.
-- | Fortunately, `x` and `y` are the only nodes whose lengths we force.
-- | Note that this is still true even if we violate the red-black property, so long as the tree retains its ordering.

{-# INLINE balance #-}
balance
  :: (Contiguous a)
  => Int
  -> Color
  -> Tree a
  -> a
  -> Tree a
  -> Tree a
balance zIndex k (Tree yIndex Red (Tree xIndex Red a x b) y c) z d =
  Tree yIndex (k-1) (Tree xIndex Black a x b) y (Tree (zIndex - yIndex - forceLength y) Black c z d)
balance zIndex k (Tree xIndex Red a x (Tree yIndex Red b y c)) z d =
  Tree (yIndex + xIndex + forceLength x) (k-1) (Tree xIndex Black a x b) y (Tree (zIndex - xIndex - forceLength x - yIndex - forceLength y) Black c z d)
balance xIndex k a x (Tree zIndex Red (Tree yIndex Red b y c) z d) =
  Tree (xIndex + forceLength x + yIndex) (k-1) (Tree xIndex Black a x b) y (Tree (zIndex - yIndex - forceLength y) Black c z d)
balance xIndex k a x (Tree yIndex Red b y (Tree zIndex Red c z d)) =
  Tree (xIndex + forceLength x + yIndex) (k-1) (Tree xIndex Black a x b) y (Tree (zIndex) Black c z d)
balance index color a x b = Tree index color a x b

-- | The tree passed should never be (Nil k),
-- | but included for completeness.

{-# INLINE deleteLeftAndReturn #-}
deleteLeftAndReturn
  :: (Contiguous a)
  => Tree a
  -> (Tree a, a, Int)
deleteLeftAndReturn Tree{ index, color, left = left@Nil{}, node, right } = (deleteNode index color left right, node, forceLength node)
deleteLeftAndReturn Tree{ index, color, left, node, right } = (resolve $ Tree (index - len) color left' node right, entry, len)
  where (left', entry, len) = deleteLeftAndReturn left
deleteLeftAndReturn nil@Nil{} = (nil, empty, 0)

{-# INLINE deleteNode #-}
deleteNode
  :: (Contiguous a)
  => Int
  -> Color
  -> Tree a         -- left
  -> Tree a         -- right
  -> Tree a
deleteNode index color left right@Tree{} = resolve $ Tree index color left entry right'
  where (right', entry, _) = deleteLeftAndReturn right
deleteNode _ color (Tree xIndex k a x b) (Nil _) = Tree xIndex (color + k) a x b
  -- | i != j should never happen; we arbitrarily pick the first one.
deleteNode _ color (Nil i) (Nil _) = Nil (color + i)

{-# INLINE prependRight #-}
prependRight
  :: (Contiguous a)
  => a
  -> Tree a
  -> Tree a
prependRight a tree =
  if (isEmpty a) then
    tree
  else
    insert tree
  where insert nil@(Nil _) = Tree 0 Red nil a nil
        insert (Tree count color left node right) =
          balance count color left node (prependTree a right)

{-# INLINE prependTree #-}
prependTree
  :: (Contiguous a)
  => a
  -> Tree a
  -> Tree a
prependTree a tree =
  if (isEmpty a) then
    tree
  else
    insert tree
  where insert nil@(Nil _) = Tree 0 Red nil a nil
        insert (Tree count color left node right) =
          balance (count + forceLength a) color (insert left) node right

-- | Resolve any k > 1-black color in child nodes in a valid tree.
-- | Note -- resolve is NOT recursive! It only deals with the immediate children.
-- | Hence, any k > 1-black nodes further down will be unaffected.
-- |
-- | A priori, there is a worry here that you may inadvertently force the length of the tail when rebalancing;
-- | but since the tail is always the rightmost node in the tree, note that it can never be `x` or `y` in any of the cases below.
-- | Fortunately, `x` and `y` are the only nodes whose lengths we force.
-- | Note that this is true even if we break the red-black property; it only requires that the tree's order is maintained.

{-# INLINE resolve #-}
resolve
  :: (Contiguous a)
  => Tree a
  -> Tree a
resolve (Tree xIndex Black left@(Tree wIndex i a w b) x right@(Tree zIndex Red (Tree yIndex Black c y d) z e)) =
  if (i < 2) then
    Tree xIndex Black left x right
  else
    resolve $ Tree (xIndex + xLen + zIndex) Black left' z e
    where left' = resolveLeft $ balance (xIndex + xLen + yIndex) Black (Tree xIndex Red (Tree wIndex (i-1) a w b) x c) y d
          xLen = forceLength x
resolve (Tree yIndex Black left@(Tree wIndex Red a w (Tree xIndex Black b x c)) y right@(Tree zIndex i d z e)) =
  if (i < 2) then
    Tree yIndex Black left y right
  else
    resolve $ Tree wIndex Black a w right'
    where right' = resolveRight $ balance xIndex Black b x (Tree (yIndex - wIndex - wLen - xIndex - xLen) Red c y (Tree zIndex (i - 1) d z e))
          xLen = forceLength x
          wLen = forceLength w
resolve (Tree yIndex Red _left@(Tree xIndex i a x b) y _right@(Tree zIndex j c z d)) =
  -- Since the root is red, both nodes must be k-Black for k >= 1.
  -- If i, j < 2 then they're equal and both 1-black, so the following is the identity.
  --
  -- Importantly for the non-EQ cases, because this case pushes the greater i (resp. j)-black node further down the tree before resolving, it is a priori possible that the j (resp. i)-black root may see a higher-degree black node on its left (resp. right).
  -- However, note that c (resp. b) can be assumed to be "resolved" (inductive hypothesis, only k>1-black in the immediate children). This means that the i (resp. j)-black that appears in the root will never need to be resolved with a k-black on its left (resp. right), so we don't actually need an extra case.
  --
  -- It is still possible that the tree incurs a red-red violation from the reparenting operations, but these are resolved by the call to balance (with the augmented function that can handle a higher-degree black root).
  case (compare i j) of
    EQ -> Tree yIndex (i - 1) (Tree xIndex Black a x b) y (Tree zIndex Black c z d)
    GT -> balance (yIndex + yLen + zIndex) j left' z d
    LT -> balance xIndex i a x right'
    where left'  = resolve $ Tree yIndex Red (Tree xIndex (i - j) a x b) y c
          right' = resolve $ Tree (yIndex - xIndex - xLen) Red b y (Tree zIndex (j - i) c z d)
          xLen = forceLength x
          yLen = forceLength y
resolve tree@(Tree yIndex Black _left@(Tree xIndex i a x b) y _right@(Tree zIndex j c z d)) =
  -- This is identical to the previous case, except that we now discharge the root's black to the new root.
  -- So we have the same cases with the new root's count simply incremented by 1.
  -- No change in the logic, except that the children being red is not immediately excluded.
  case (compare i j) of
    EQ -> if (i == 0) then
            tree
          else
            Tree yIndex i (Tree xIndex Black a x b) y (Tree zIndex Black c z d)
    GT -> balance (yIndex + yLen + zIndex) (j + 1) left' z d
    LT -> balance xIndex (i + 1) a x right'
    where left'  = resolve $ Tree yIndex Red (Tree xIndex (i - j) a x b) y c
          right' = resolve $ Tree (yIndex - xIndex - xLen) Red b y (Tree zIndex (j - i) c z d)
          xLen = forceLength x
          yLen = forceLength y
  -- -- note that the case where either one of these subtrees was red was already handled. so both of them are necessarily black at this point.
  -- case (0)
resolve tree = tree

{-# INLINE resolveLeft #-}
resolveLeft
  :: (Contiguous a)
  => Tree a
  -> Tree a
resolveLeft Tree{index, color, left, node, right} = resolve $ Tree index color (resolve left) node right
resolveLeft nil@Nil{}                             = nil

{-# INLINE resolveRight #-}
resolveRight
  :: (Contiguous a)
  => Tree a
  -> Tree a
resolveRight Tree{index, color, left, node, right} = resolve $ Tree index color left node (resolve right)
resolveRight nil@Nil{}                             = nil

{-# INLINE toLazyByteString #-}
toLazyByteString
  :: ByteTree
  -> Lazy.ByteString
toLazyByteString Tree{left, node, right} = Lazy.concat [toLazyByteString left, Lazy.fromStrict node, toLazyByteString right]
toLazyByteString Nil{}    = Lazy.empty
