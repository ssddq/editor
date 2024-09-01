{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Data.Edits where

import Data.ByteString          qualified as Strict
import Data.ByteString.Internal qualified as Strict
import Data.Lines
import Data.Tree
import Data.Tree                qualified as Tree

import Common
import Utils

-- * Common

data Direction
  = Forward
  | Back
  | Down
  | Up

data Edits
  = Branch { prefix :: {-# UNPACK #-} !Int
           , switch :: {-# UNPACK #-} !Int
           , left   :: !Edits
           , right  :: !Edits
           }
  | Leaf   { prefix :: {-# UNPACK #-} !Int
           , delete :: {-# UNPACK #-} !Int
           , insert :: !ByteTree
           , length :: {-# UNPACK #-} !Int
           }
  deriving (Eq, Show)


{-# INLINE calculateDistance #-}
calculateDistance
  :: Edits
  -> Position
  -> Position
  -> Int
calculateDistance !Branch{ prefix, switch, left, right } !start !position
  | position.base == start.base = (position.offset - start.offset)
  | position.base <  divider && start.base <  divider = calculateDistance left  start position
  | position.base >= divider && start.base >= divider = calculateDistance right start position
  | otherwise = calculateDistance left start (Position divider 0) + calculateDistance right (Position divider 0) position
  where divider = calculateDivider prefix switch
calculateDistance !Leaf{ prefix, delete, length } !start !position =
  case (compare position.base prefix, compare start.base prefix) of
    (EQ, EQ) -> position.offset - start.offset
    (GT, EQ) -> (position.base - start.base) + (length - start.offset) - delete
    (EQ, LT) -> position.base - start.base + position.offset
    (GT, LT) -> (position.base - start.base) + length - delete
    -- last case includes the (valid) (LT, LT) and (GT, GT) cases,
    -- but also invalid combinations where position.base > start.base
    _        -> position.base - start.base

{-# INLINE noEdits #-}
noEdits :: Edits
noEdits = Leaf 0 0 (Nil Black) 0

-- | Note, naiveJoin and naiveJoinMaybe do *not* correctly merge branches
-- | or even verify that they were supplied in the right order!

{-# INLINE naiveJoin #-}
naiveJoin
  :: Edits
  -> Edits
  -> Edits
naiveJoin !a !b = Branch prefix switch a b
  where I2 prefix switch = commonPrefix (prefixOf a) (prefixOf b)

{-# INLINE naiveJoinMaybe #-}
naiveJoinMaybe
  :: Maybe Edits
  -> Maybe Edits
  -> Maybe Edits
naiveJoinMaybe Nothing  Nothing  = Nothing
naiveJoinMaybe (Just a) Nothing  = Just a
naiveJoinMaybe Nothing  (Just b) = Just b
naiveJoinMaybe (Just a) (Just b) = Just $ Branch prefix switch a b
  where I2 prefix switch = commonPrefix (prefixOf a) (prefixOf b)

{-# INLINE prefixOf #-}
prefixOf
  :: Edits
  -> Int
prefixOf !(Branch prefix _ _ _) = prefix
prefixOf !(Leaf   key    _ _ _) = key



-- * Move

-- | Note the lack of symmetry with moveForward,
-- | since the divider here is in the right branch and
-- | there is a fundamental asymmetry/orientation to insertions/deletions.

{-# INLINE moveBackward #-}
moveBackward
  :: Int
  -> Edits
  -> Position
  -> Position
moveBackward !count !Branch{prefix, switch, left, right} !position@Position{base} =
  case (compare base divider) of
    LT -> moveBackward count left position
    _  -> if (offset' == 0) then
            moveBackward (base - base') left (Position base 0)
          else
            position'
  where position'@(Position base' offset') = moveBackward count right position
        divider = calculateDivider prefix switch
moveBackward !count !leaf@Leaf{prefix, delete, length} !Position{base, offset} =
  case (compare base prefix) of
    LT -> Position final 0
    GT -> if ( final > prefix + delete ) then
            Position final 0
          else if (delete == 0 && length /= 0) then
            moveBackward (prefix - final - 1     ) leaf (Position prefix $ length - 1)
          else
            moveBackward (prefix + delete - final) leaf (Position prefix $ length    )
    EQ -> if (offset < count) then
            Position (prefix - (count - offset)) 0
          else
            Position prefix $ offset - count
  where final = base - count

{-# INLINE moveDown #-}
moveDown
  :: Int
  -> Edits
  -> Tree Lines
  -> Position
  -> Position
moveDown n = moveUp (-n)

{-# INLINE moveForward #-}
moveForward
  :: Int
  -> Edits
  -> Position
  -> Position
moveForward !count !Branch{prefix, switch, left, right} !position@Position{base} =
  case (compare base divider) of
    LT | base' < divider -> position'
       | otherwise           -> moveForward (base' - divider) right (Position divider 0)
    _  -> moveForward count right position
  where divider = calculateDivider prefix switch
        position'@(Position base' _) = moveForward count left position
moveForward !count !leaf@Leaf{prefix, delete, length} !Position{base, offset} =
  case (compare base prefix) of
    GT -> Position final 0
    LT -> if (final < prefix) then
            Position final 0
          else
            moveForward (final - prefix) leaf (Position prefix 0)
    EQ -> if (length < offset + count) then
            Position (prefix + offset + count - length + delete) 0
          else if (length == 0) then
            Position (prefix + delete) 0
          else
            Position base $ offset + count
  where final = base + count

{-# INLINE moveUp #-}
moveUp
  :: Int
  -> Edits
  -> Tree Lines
  -> Position
  -> Position
moveUp !k !edits !tree !position =
  -- Check to see if we've hit the start of the file
  if (target_start == target_end) then
    moveForward offset edits target_start
  else
    min target_end $ moveForward offset edits target_start
  where n            = Tree.findLineNumber position tree - 1
        offset       = max 1 $ calculateDistance edits start position
        start        = Tree.findLinePosition (n        ) tree
        target_start = Tree.findLinePosition (n - k    ) tree
        target_end   = Tree.findLinePosition (n - k + 1) tree



-- * Insert functions

{-# INLINE insertPatches #-}
insertPatches
  :: Strict.ByteString
  -> Position
  -> Edits
  -> Edits
insertPatches !string@(Strict.BS _ len) !position@Position{ base } !branch@Branch{ prefix, switch, left, right } =
  case (inLeftRight base prefix switch) of
    InLeft  -> naiveJoin
                 |- insertPatches string position left
                 |- right
    InRight -> naiveJoin
                 |- left
                 |- insertPatches string position right
    _ | base > prefix -> naiveJoin
                               |- branch
                               |- Leaf base 0 (Tree.new string) len
      | otherwise -> naiveJoin
                       |- Leaf base 0 (Tree.new string) len
                       |- branch
insertPatches !string !(Position base offset) !leaf@Leaf{prefix, delete, insert, length} =
  case (compare base prefix) of
    LT -> naiveJoin
            |- Leaf base 0 (Tree.new string) (Strict.length string)
            |- leaf
    GT -> naiveJoin
            |- leaf
            |- Leaf base 0 (Tree.new string) (Strict.length string)
    EQ -> Leaf prefix delete (Tree.insertByteTree string offset insert) (length + Strict.length string)



-- * Delete functions

{-# INLINE deletePatches #-}
deletePatches
  :: Position
  -> Int
  -> Int
  -> Edits
  -> (Edits, Position)
deletePatches !position !count !limit !edits = case (deleted 0) of
  Just    edits' -> (edits' , endState.newCursor)
  Nothing        -> (noEdits, endState.newCursor)
  where (deleted, endState) = startDelete edits $ DeleteState position count 0 limit position

{-# INLINE finishDelete #-}
finishDelete
  :: DeleteState
  -> Edits
  -> (Maybe Edits, DeleteState)
finishDelete !state !Branch{left, right} =
  ( naiveJoinMaybe
      |- left'
      |- right'
  , state''
  )
  where (left' , state' ) = if (state.remaining  > 0) then
                              finishDelete state left
                            else
                              (Just left , state )
        (right', state'') = if (state'.remaining > 0) then
                              finishDelete state' right
                            else
                              (Just right, state')
finishDelete !state !leaf@Leaf{prefix, delete, insert, length} = case (compare stop prefix) of
  LT -> ( Just leaf
        , state
            { remaining = 0
            , position  = Position (prefix + 1) 0
            }
        )
  EQ -> case (length) of
          0 -> ( Nothing
               , state
                   { remaining = 0
                   , position  = Position (prefix + 1) 0
                   , extras    = extras + delete
                   }
               )
          _ -> ( Just leaf
               , state
                   { remaining = 0
                   , position  = Position (prefix + 1) 0
                   }
               )
  GT -> if (stop <= end) then
           ( Just Leaf
               { prefix
               , delete
               , insert = Tree.removeByteTree insert 0 count
               , length = length - count
               }
           , state
               { remaining = 0
               , position  = Position (prefix + 1) 0
               , extras    = extras - count
               }
           )
        else
           ( Nothing
           , state
               { remaining = max 0 $ stop - end + delete - 1
               , position  = Position (prefix + 1) 0
               , extras    = extras + delete - length
               }
           )
  where position  = state.position.base
        remaining = state.remaining
        extras    = state.extras
        stop      = position + remaining
        count     = stop - prefix
        end       = prefix + length

{-# INLINE startDelete #-}
startDelete
  :: Edits
  -> DeleteState
  -> (Int -> Maybe Edits, DeleteState)
startDelete !branch@Branch{prefix, switch, left, right} !state = case (inLeftRight base prefix switch) of
  InLeft -> ( \n -> naiveJoinMaybe
                      |- left' (n + state''.extras)
                      |- right'
            , state'' { newCursor = state'.newCursor }
            )
            where (left' , state' ) = startDelete left state
                  (right', state'') = if (state'.remaining > 0) then
                                        finishDelete (state' { extras = 0 }) right
                                      else
                                        (Just right, state' { extras = 0 })
  InRight -> ( \n -> naiveJoinMaybe
                       |- Just left
                       |- right' n
             , state'
             )
             where (right', state') = startDelete right state
  _ | base > prefix ->
        ( \n -> naiveJoinMaybe
                  |- Just branch
                  |- Just Leaf
                       { prefix = base
                       , delete = min (state.limit - base) (remaining + n)
                       , insert = Nil Black
                       , length = 0
                       }
        , state
            { position  = Position (base + 1) 0
            , remaining = remaining - 1
            , newCursor = Position base offset
            }
        )
    | otherwise ->
        ( \n -> naiveJoinMaybe
                  |- Just Leaf
                       { prefix = base
                       , delete = min (state.limit - base) (remaining + state'.extras + n)
                       , insert = Nil Black
                       , length = 0
                       }
                  |- branch'
        , state' { extras    = 0
                 , newCursor = Position base offset
                 }
        )
        where (branch', state') = finishDelete
                                    |- state
                                         { position  = Position (base + 1) 0
                                         , remaining = remaining - 1
                                         }
                                    |- branch
  where base      = state.position.base
        offset    = state.position.offset
        remaining = state.remaining
startDelete leaf@Leaf{prefix, delete, insert, length} state =
  case (compare base prefix) of
    LT -> ( \n -> naiveJoinMaybe
                    |- Just Leaf
                         { prefix = base
                         , delete = min (state.limit - base) (remaining + state'.extras + n)
                         , insert = Nil Black
                         , length = 0
                         }
                    |- leaf'
          , state'
              { extras    = 0
              , newCursor = Position base offset
              }
          )
          where (leaf', state') = finishDelete state leaf
    GT | prefix + delete < base ->
            ( \n -> naiveJoinMaybe
                      |- Just leaf
                      |- Just Leaf
                           { prefix = base
                           , delete = min (state.limit - base) (remaining + n)
                           , insert = Nil Black
                           , length = 0
                           }
            , state
                { position  = Position (base + 1) 0
                , remaining = remaining - 1
                , newCursor = Position base offset
                }
            )
       | otherwise ->
            ( \n -> Just Leaf
                      { prefix
                      , delete = min (state.limit - prefix) (n + delete + remaining)
                      , insert
                      , length
                      }
            , state
                { position  = Position (base + 1) 0
                , remaining = remaining - 1 + (prefix + delete - base)
                , newCursor = Position prefix 0
                }
            )
    EQ | offset + remaining < length ->
            ( \n -> Just Leaf
                      { prefix = base
                      , delete = min (state.limit - base) (n + delete)
                      , insert = Tree.removeByteTree insert offset remaining
                      , length = length - remaining
                      }
            , state
                { position  = Position (base + 1) 0
                , remaining = 0
                , newCursor = Position base offset
                }
            )
       | otherwise ->
            ( \n -> Just Leaf
                      { prefix = base
                      , delete = min (state.limit - base) (n + delete + remaining - length + offset)
                      , insert = Tree.removeByteTree insert offset $ length - offset
                      , length = offset
                      }
            , state { position  = Position (base + 1) 0
                    , remaining = remaining + delete - length + offset - 1
                    , newCursor = Position base offset
                    }
            )
  where position  = state.position
        remaining = state.remaining
        base      = position.base
        offset    = position.offset
