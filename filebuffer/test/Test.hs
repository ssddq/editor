{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}

module Test where

import Common
import Filebuffer
import Utils

import Data.Bits

import Data.Edits
import Data.Lines
import Data.Tree

import Data.Tree         qualified as Tree
import Data.Vector.Async qualified as Async

import Test.Hspec

import System.IO

import Unsafe.Coerce

-- * Main

-- | A note on these tests:
-- | Some of them are specific and targeted,
-- | but a lot of the functions involving trees
-- | are tricky to come up with 'principled' unit tests for.
-- |
-- | For these functions, I'm just adding unit tests as I notice bugs.
-- | A failing test does *not* mean the code is wrong or has a bug;
-- | merely that there's something worth paying attention to.

main
  :: HasCallStack
  => IO ()
main = do
   hspec $ do
     unitTests

unitTests
  :: HasCallStack
  => SpecWith ()
unitTests = do
  clampedIndexTests
  commonPrefixTests
  decodeUtf8Tests
  deletePatchesTests
  findLineNumberTests
  findLinePositionTests
  calculateDistanceTests
  inRangeTests
  insertNewlinesTest
  insertPatchesTests
  findIndexTests
  moveBackwardTests
  moveUpTests
  asyncVectorTests
  moveDownTests
  insertLinesTests
  removeLinesTests
  moveForwardTests
  filebufferTests
  skipFromToTests
  deleteTests



-- * Test groups

asyncVectorTests
  :: HasCallStack
  => SpecWith ()
asyncVectorTests = describe "AsyncVector" $ do
  let vector = Async.drop 1 [-1,1,2,13,55,56,88,123,159]
      vector0 = [1,2,13,55,56,88,123,159]
      head = [1,2,13,55,56]
      tail = [88,123,159]
  specify "1" $
    Async.splitAt 5 vector
      `shouldBe`
    (head, tail)
  specify "2" $
    Async.splitAt 5 vector
      `shouldBe`
    Async.splitAt 5 vector0
  specify "3" $
    Async.splitAt 5 vector0
      `shouldBe`
    (head, tail)
  specify "4" $ vector Async.! 0 `shouldBe` vector0 Async.! 0
  specify "5" $ vector Async.! 7 `shouldBe` vector0 Async.! 7
  specify "6" $ vector Async.! 0 `shouldBe` 1
  specify "7" $ vector Async.! 7 `shouldBe` 159
  specify "8" $ vector Async.! 4 `shouldBe` 56
  specify "9" $ Async.forceLength vector `shouldBe` 8
  specify "10" $ Async.forceLength vector `shouldBe` Async.forceLength vector0
  let (head, tail) = Async.splitAt 6 vector
  specify "11" $ Async.forceLength vector `shouldBe` (Async.forceLength head + Async.forceLength tail)
  specify "12" $ Async.searchForPosition' (Position 56 0) (\p -> Position p 0) vector `shouldBe` 4
  specify "13" $ Async.searchForPosition' (Position 57 0) (\p -> Position p 0) vector `shouldBe` 5
  specify "14" $ Async.searchForPosition' (Position 57 0) (\p -> Position p 0) vector `shouldBe` 5
  specify "15" $ Async.searchForPosition' (Position 1 0) (\p -> Position p 0) vector `shouldBe` 0
  specify "16" $ Async.searchForPosition' (Position 158 0) (\p -> Position p 0) vector `shouldBe`  7
  specify "17" $ Async.searchForPosition' (Position 159 0) (\p -> Position p 0) vector `shouldBe`  7
  specify "18" $ Async.searchForPosition' (Position 160 0) (\p -> Position p 0) vector `shouldBe`  8
  specify "19" $ Async.searchForPosition' (Position 57 0) (\p -> Position p 0) vector
                   `shouldBe`
                 Async.searchForPosition' (Position 57 0) (\p -> Position p 0) vector0

calculateDistanceTests
  :: HasCallStack
  => SpecWith ()
calculateDistanceTests = describe "calculateDistance" $ do
  specify "1" $ calculateDistance testPatch3   (Position 127 0) (Position 128 7) `shouldBe` 8
  specify "2" $ calculateDistance testPatch4   (Position 88  0) (Position 93  0) `shouldBe` 4
  specify "3" $ calculateDistance noEdits (Position 88  0) (Position 93  0) `shouldBe` 5
  where testPatch3 = Branch 0 128 (Branch 0 32 (Leaf 0 0 (Nil 1) 0) (Leaf 44 0 (Tree 0 1 (Nil 1) "1231 414 1\n" (Nil 1)) 11)) (Leaf 128 0 (Tree 0 1 (Nil 1) "124 14 14\n1421 4214\n" (Nil 1)) 20)
        testPatch4 = Branch 0 64 (Leaf 0 0 (Nil 1) 0) (Leaf 89 1 (Nil 1) 0)

clampedIndexTests
  :: HasCallStack
  => SpecWith ()
clampedIndexTests = describe "(>!<)" $ do
  specify "1" $ testVector >!< (-1) `shouldBe` 0
  specify "2" $ testVector >!< 0    `shouldBe` 0
  specify "3" $ testVector >!< 2    `shouldBe` 20
  specify "4" $ testVector >!< 4    `shouldBe` 40
  specify "5" $ testVector >!< 5    `shouldBe` 40
  specify "6" $ testVector >!< 10   `shouldBe` 40
  where testVector :: Async.Vector
        testVector = [0,10,20,30,40]

commonPrefixTests
  :: HasCallStack
  => SpecWith ()
commonPrefixTests = describe "commonPrefix" $ do
  specify "1" $ commonPrefix 2  4  `shouldBe` I2 0  4
  specify "2" $ commonPrefix 3  10 `shouldBe` I2 0  8
  specify "3" $ commonPrefix 5  10 `shouldBe` I2 0  8
  specify "4" $ commonPrefix 11 10 `shouldBe` I2 10 1
  specify "5" $ commonPrefix 12 10 `shouldBe` I2 8  4

decodeUtf8Tests
  :: HasCallStack
  => SpecWith ()
decodeUtf8Tests = describe "decodeUtf8" $ do
  specify "1" $ decodeUtf8 0b01110011 zeroBits zeroBits zeroBits `shouldBe` Decoded 0b01110011 0
  specify "2" $ decodeUtf8 0b01110011 oneBits  oneBits  oneBits  `shouldBe` Decoded 0b01110011 0
  specify "3" $ decodeUtf8 0xC2       0xA3     0        0        `shouldBe` Decoded 0x00A3     2
  specify "4" $ decodeUtf8 0xD0       0x98     0        0        `shouldBe` Decoded 0x0418     2
  specify "5" $ decodeUtf8 0xE0       0xA4     0xB9     0        `shouldBe` Decoded 0x0939     3
  specify "6" $ decodeUtf8 0xE2       0x82     0xAC     0        `shouldBe` Decoded 0x20AC     3
  specify "7" $ decodeUtf8 0xED       0x95     0x9C     0        `shouldBe` Decoded 0xD55C     3
  specify "8" $ decodeUtf8 0xF0       0x90     0x8D     0x88     `shouldBe` Decoded 0x10348    4

deletePatchesTests
  :: HasCallStack
  => SpecWith ()
deletePatchesTests = describe "deletePatches" $ do
  specify "1" $
    deletePatches (Position 3 0) 2 (Leaf 10 0 "test" 4)
      `shouldBe`
    Branch 0 8 (Leaf 3 2 "" 0) (Leaf 10 0 "test" 4)
  specify "2" $
    deletePatches (Position 3 0) 2 (insertPatches "test" (Position 10 0) noEdits)
      `shouldBe`
    Branch 0 8 (Branch 0 2 (Leaf 0 0 "" 0) (Leaf 3 2 "" 0)) (Leaf 10 0 "test" 4)
  specify "3" $
    deletePatches (Position 3 0) 7 (insertPatches "test" (Position 10 0) noEdits)
      `shouldBe`
    Branch 0 8 (Branch 0 2 (Leaf 0 0 "" 0) (Leaf 3 7 "" 0)) (Leaf 10 0 "test" 4)
  specify "4" $
    ( deletePatches (Position 3 0) 3 $ deletePatches (Position 5 0) 4 $ deletePatches (Position 9 0) 2 $ Leaf 10 0 "test" 4 )
      `shouldBe`
    Branch 0 8 (Leaf 3 7 "" 0) (Leaf 10 0 "st" 2)
  specify "5" $
    deletePatches (Position 1 0) 1 (Branch 0 2 (Leaf 1 0 "" 0) (Leaf 2 0 "?" 1))
      `shouldBe`
    Branch 0 2 (Leaf 1 1 "" 0) (Leaf 2 0 "?" 1)
  specify "6" $
    ( deletePatches (Position 3 0) 1 $ deletePatches (Position 4 1) 1 $ insertPatches "?" (Position 4 0) $ noEdits )
      `shouldBe`
    ( deletePatches (Position 4 1) 1 $ insertPatches "?" (Position 4 0) $ deletePatches (Position 3 0) 1 $ noEdits )
  specify "7" $
    ( deletePatches (Position 1 0) 1   $ deletePatches (Position 1 0) 1 $ deletePatches (Position 2 1) 1
    $ insertPatches "?" (Position 2 0) $ deletePatches (Position 3 1) 1 $ insertPatches "?" (Position 3 0) $ noEdits )
      `shouldBe`
    ( deletePatches (Position 1 0) 1   $ deletePatches (Position 2 0) 1 $ deletePatches (Position 2 1) 1
    $ insertPatches "?" (Position 2 0) $ deletePatches (Position 3 1) 1 $ insertPatches "?" (Position 3 0) $ noEdits )

deleteTests
  :: HasCallStack
  => SpecWith ()
deleteTests = describe "delete" $ do
  let filebuffer = Filebuffer {cursor = Position {base = 2, offset = 1}, start = Position 0 0, size = 3458, handle = fakeHandle, edits = Branch {prefix = 0, switch = 2, left = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, right = Leaf {prefix = 2, delete = 0, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "1", right = Nil {color = 1}}, length = 1}}, lines = Tree {index = 2, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Nil {color = 1}}, node = Offset 2 [1], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [13,55,56], right = Nil {color = 1}}}, visualLineCount = maxBound}
      lines' = Tree.resolve $ Tree.deleteNode 2 1 (Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Nil {color = 1}}) (Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [13,55,56], right = Nil {color = 1}})
  specify "1" $
    (delete 1 filebuffer).lines
      `shouldBe`
    lines' -- (delete 2 $ filebuffer { handle = fakeHandle, cursor = Position 2 0 }).lines
  let filebuffer = Filebuffer {cursor = Position {base = 1, offset = 0}, start = Position 0 0, size = 3458, handle = fakeHandle, edits = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, lines = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1,2,13], right = Nil {color = 1}}, visualLineCount = maxBound}
      filebuffer' = Filebuffer {cursor = Position {base = 1, offset = 0},start = Position 0 0, size = 3458, handle = fakeHandle, edits = Branch {prefix = 0, switch = 1, left = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, right = Leaf {prefix = 1, delete = 1, insert = Nil {color = 1}, length = 0}}, lines = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Tree {index = 0, color = 0, left = Nil {color = 1}, node = Base [2,13], right = Nil {color = 1}}}, visualLineCount = maxBound}
  specify "2" $
    (delete 1 filebuffer).lines
      `shouldBe`
    filebuffer'.lines

fakeHandle :: Handle
fakeHandle = unsafeCoerce $ (0 :: Int)

filebufferTests
  :: HasCallStack
  => SpecWith ()
filebufferTests = describe "filebuffer" $ do
  let lines = Tree.new $ Base [-1,1,2,13,55,56,88,123,159,196,235,274,321]
      filebuffer = Filebuffer { cursor = Position 0 0, start = Position 0 0, size = 3000, handle = fakeHandle, edits = noEdits, lines , visualLineCount = maxBound}
  specify "1" $
    (movePosition Up . delete 1 . insert "?" . movePosition Down . movePosition Down . delete 1 . insert "?" . movePosition Down $ filebuffer).cursor
      `shouldBe`
    Position 15 0
  specify "2" $
    (movePosition Down . delete 1 . movePosition Up . movePosition Up .  movePosition Up . delete 1 . insert "?" . movePosition Down . movePosition Down . delete 1 . insert "?" . movePosition Down $ filebuffer).cursor
      `shouldBe`
    Position 15 0
  -- note this is not a valid line tree in terms of color, but that should not affect the test here (it would be an error if it did!)
  let lines = Tree {index = 3, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Tree {index = 0, color = 0, left = Nil {color = 1}, node = Offset 2 [1], right = Nil {color = 1}}}, node = Base [13,55], right = Nil {color = 2}}
      filebuffer = Filebuffer {cursor = Position {base = 2, offset = 3}, start = Position 0 0, size = 3458, handle = fakeHandle, edits = Branch {prefix = 0, switch = 2, left = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, right = Leaf {prefix = 2, delete = 1, insert = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "1", right = Nil {color = 1}}, node = "\n", right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "1", right = Nil {color = 1}}}, length = 3}}, lines , visualLineCount = maxBound}
  specify "3" $
    (delete 1 filebuffer).cursor
      `shouldBe`
    (Position 2 3)
  let filebuffer = Filebuffer {cursor = Position {base = 108, offset = 9}, start = Position {base = 0, offset = 0}, size = 844, handle = fakeHandle, edits = Branch {prefix = 0, switch = 64, left = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, right = Leaf {prefix = 108, delete = 18, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "1234\n1234", right = Nil {color = 1}}, length = 9}}, lines = Tree {index = 7, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base {vector = [-1,19,20,39,57,76,107]}, right = Nil {color = 1}}, node = Offset {base = 108, vector = [4]}, right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base {vector = [126,145,162,181,199,210,232,258,274,297,328,354,374,396,418,444,459,479,496,507,520,530,545,562,579,596,611,630,646,657,671,692,703,714,727,740,750,767,778,794,811,843]}, right = Nil {color = 1}}}, visualLineCount = 36}
      lines' = Tree {index = 7, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base {vector = [-1,19,20,39,57,76,107]}, right = Nil {color = 1}}, node = Offset {base = 108, vector = [4]}, right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base {vector = [145,162,181,199,210,232,258,274,297,328,354,374,396,418,444,459,479,496,507,520,530,545,562,579,596,611,630,646,657,671,692,703,714,727,740,750,767,778,794,811,843]}, right = Nil {color = 1}}}
  specify "4" $
    (delete 1 filebuffer).lines
      `shouldBe`
    lines'

findIndexTests
  :: HasCallStack
  => SpecWith ()
findIndexTests = do
  describe "findIndex'" $ do
    specify "1" $ findIndex' testArray (Position (-1)  0) `shouldBe` 0
    specify "2" $ findIndex' testArray (Position 0  0) `shouldBe` 1
    specify "3" $ findIndex' testArray (Position 0  5) `shouldBe` 1
    specify "4" $ findIndex' testArray (Position 5  0) `shouldBe` 1
    specify "5" $ findIndex' testArray (Position 10 0) `shouldBe` 1
    specify "6" $ findIndex' testArray (Position 40 0) `shouldBe` 4
    specify "7" $ findIndex' testArray (Position 40 4) `shouldBe` 5
  describe "findIndex_" $ do
    specify "1" $ findIndex_ testArray (Position (-1) 0) `shouldBe` (-1)
    specify "2" $ findIndex_ testArray (Position 0  0) `shouldBe` 0
    specify "3" $ findIndex_ testArray (Position 0  5) `shouldBe` 0
    specify "4" $ findIndex_ testArray (Position 5  0) `shouldBe` 0
    specify "5" $ findIndex_ testArray (Position 10 0) `shouldBe` 0
    specify "6" $ findIndex_ testArray (Position 40 0) `shouldBe` 3
    specify "7" $ findIndex_ testArray (Position 40 4) `shouldBe` 5
  where testArray = Base [-1,10,20,30,40]

findLineNumberTests
  :: HasCallStack
  => SpecWith ()
findLineNumberTests = describe "findLineNumber" $ do
  let testArray = Base [-1,10,20,30,40]
      testLines = Tree.new $ testArray
  specify "1" $ Tree.findLineNumber (Position 0  0) testLines `shouldBe` 1
  specify "2" $ Tree.findLineNumber (Position 5  0) testLines `shouldBe` 1
  specify "3" $ Tree.findLineNumber (Position 10 0) testLines `shouldBe` 1
  specify "4" $ Tree.findLineNumber (Position 15 0) testLines `shouldBe` 2
  specify "5" $ Tree.findLineNumber (Position 30 0) testLines `shouldBe` 3
  specify "6" $ Tree.findLineNumber (Position 35 0) testLines `shouldBe` 4
  specify "7" $ Tree.findLineNumber (Position 40 0) testLines `shouldBe` 4
  specify "8" $ Tree.findLineNumber (Position 41 0) testLines `shouldBe` 5
  let lines = Tree 0 1 (Nil 1) (Offset 0 [-1,0,1]) (Tree 0 0 (Nil 1) (Base [227,228,229,387,388,389,497]) (Nil 1))
  specify "9" $ Tree.findLineNumber (Position 0 1) lines `shouldBe` 2

findLinePositionTests
  :: HasCallStack
  => SpecWith ()
findLinePositionTests = describe "findLinePosition" $ do
  specify "1" $ Tree.findLinePosition 0 testLines `shouldBe` Position (-1) 0
  specify "2" $ Tree.findLinePosition 1 testLines `shouldBe` Position 10   0
  specify "3" $ Tree.findLinePosition 2 testLines `shouldBe` Position 20   0
  specify "4" $ Tree.findLinePosition 4 testLines `shouldBe` Position 40   0
  where testArray = Base [-1,10,20,30,40]
        testLines = Tree.new $ testArray

inRangeTests
  :: HasCallStack
  => SpecWith ()
inRangeTests = describe "inRange" $ do
  specify "1" $ inRange (Position 0  0) testArray `shouldBe` EQ
  specify "1" $ inRange (Position 1  0) testArray `shouldBe` EQ
  specify "1" $ inRange (Position 40 0) testArray `shouldBe` EQ
  specify "1" $ inRange (Position 41 0) testArray `shouldBe` GT
  where testArray = Base [-1,10,20,30,40]

insertLinesTests
  :: HasCallStack
  => SpecWith ()
insertLinesTests = describe "insertLines" $ do
  specify "1" $
    insertLines (Position 10 0) "\n" testArray
      `shouldBe`
    (Base [-1], Offset 10 [0,1], Base [20,30,40])
  let positions = Offset 10 [-1,10,20,30,40]
  specify "1" $
    insertLines (Position 10 2) "\n" positions
      `shouldBe`
    (Offset 10 [-1,2,11,21,31,41], empty, empty)
  where testArray = Base [-1,10,20,30,40]

insertNewlinesTest
  :: HasCallStack
  => SpecWith ()
insertNewlinesTest = describe "insertNewlines" $ do
  specify "1" $
    Tree.insertNewlines "\n" (Position 5 0) testLines
      `shouldBe`
    Tree 1 1 (Tree.new $ Base [-1]) (Offset 5 [0]) (Tree.new $ Base [10,20,30,40])
  specify "2" $
    Tree.insertNewlines "\n" (Position 5 1) testLines
      `shouldBe`
    Tree 1 1 (Tree.new $ Base [-1]) (Offset 5 [1]) (Tree.new $ Base [10,20,30,40])
  specify "3" $
    Tree.insertNewlines "\n" (Position 40 1) testLines
      `shouldBe`
    Tree 0 1 (Nil 1) (Base [-1,10,20,30,40]) (Tree 0 0 (Nil 1) (Offset 40 [1]) (Nil 1))
  specify "4" $
    Tree.insertNewlines "\n" (Position 10 0) testLines
      `shouldBe`
    Tree 1 1 (Tree.new $ Base [-1]) (Offset 10 [0,1]) (Tree.new $ Base [20,30,40])
  specify "5" $
    Tree.insertNewlines "?" (Position 10 0) testLines
      `shouldBe`
    Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Nil {color = 1}}, node = Offset 10 [1], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [20,30,40], right = Nil {color = 1}}}
  let lines = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Nil {color = 1}}, node = Offset 0 [0], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [1,2,13,55,56,88,123], right = Nil {color = 1}}}
      lines' = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Nil {color = 1}}, node = Offset 0 [0], right = Tree {index = 4, color = 0, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [1,2,13,55], right = Nil {color = 1}}, node = Offset 56 [0,1], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [88,123], right = Nil {color = 1}}}}
  specify "6" $
    Tree.insertNewlines "\n" (Position 56 0) lines
      `shouldBe`
    lines'
  where testArray = Base [-1,10,20,30,40]
        testLines = Tree.new $ testArray

insertPatchesTests
  :: HasCallStack
  => SpecWith ()
insertPatchesTests = describe "insertPatches" $ do
  specify "1" $
    ( insertPatches "Patch" (Position 10 2) $ insertPatches "test" (Position 10 0) noEdits )
      `shouldBe`
    ( insertPatches "tePatchst" (Position 10 0) noEdits )
  specify "2" $
    ( insertPatches "Patch" (Position 3 0) $ insertPatches "test" (Position 10 0) noEdits )
      `shouldBe`
    Branch 0 8 (Branch 0 2 (Leaf 0 0 "" 0) (Leaf 3  0 "Patch" 5)) (Leaf 10 0 ("test") 4)
  specify "3" $
    ( insertPatches "Patch" (Position 9 0) $ deletePatches (Position 9 0) 2 $ insertPatches "test" (Position 10 0) noEdits )
      `shouldBe`
    Branch 0 8 (Leaf 0 0 "" 0) (Branch 8 2 (Leaf 9  1 ("Patch" ) 5) (Leaf 10 0 ("est" ) 3))
  specify "4" $
    ( insertPatches "?" (Position 4 0) $ insertPatches "?" (Position 7 0)
    $ insertPatches "?" (Position 6 0) $ insertPatches "?" (Position 5 0) $ noEdits )
      `shouldBe`
    ( insertPatches "?" (Position 7 0) $ insertPatches "?" (Position 6 0)
    $ insertPatches "?" (Position 5 0) $ insertPatches "?" (Position 4 0) $ noEdits )

moveBackwardTests
  :: HasCallStack
  => SpecWith ()
moveBackwardTests = describe "moveBackward" $ do
  let patch = (insertPatches "?" (Position 3 0) $ insertPatches "?" (Position 2 0) $ noEdits)
  specify "1" $
    moveBackward 1 patch (Position 3 0)
      `shouldBe`
    Position 2 1
  specify "2" $
    moveBackward 1 (Leaf 2 0 "?" 1) (Position 3 0)
      `shouldBe`
    Position 2 1
  specify "3" $
    moveBackward 1 (Leaf 8 3 "" 0) (Position 11 0)
      `shouldBe`
    Position 7 0
  specify "4" $
    moveBackward 1 (Leaf 7 1 "?????" 5) (Position 8 0)
      `shouldBe`
    Position 7 4
  specify "5" $
    moveBackward 1 testPatch1 (Position 11 0)
      `shouldBe`
    Position 7 4
  specify "6" $
    moveBackward 1 testPatch2 (Position 3 0)
      `shouldBe`
    Position 0 0
  where testPatch1 = Branch 0 8 (Branch 0 4 (Branch 0 1 (Leaf 0 0 (Nil 1) 0) (Leaf 1 1 (Tree 1 1 (Tree 0 1 (Nil 1) "?" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Tree 0 0 (Nil 1) "?" (Nil 1)))) 4)) (Branch 4 2 (Leaf 4 2 (Tree 1 1 (Tree 0 1 (Nil 1) "?" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Tree 0 0 (Nil 1) "?" (Nil 1)))) 4) (Leaf 7 1 (Tree 0 1 (Nil 1) "?????" (Nil 1)) 5))) (Branch 8 4 (Branch 8 2 (Leaf 8 3 (Nil 1) 0) (Leaf 11 1 (Tree 0 1 (Tree 0 1 (Nil 1) "" (Nil 1)) "" (Tree 0 0 (Tree 0 1 (Nil 1) "" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Nil 1)))) 2)) (Leaf 12 4 (Nil 1) 0))
        testPatch2 = Branch 0 2 (Branch 0 1 (Leaf 0 0 "" 0) (Leaf 1 2 "" 0)) (Leaf 3 0 "????" 4)

moveDownTests
  :: HasCallStack
  => SpecWith ()
moveDownTests = describe "moveDown" $ do
  let lines = Tree.new $ Base [-1,19,20]
  specify "1" $
    moveDown 1 noEdits lines (Position 0 0)
      `shouldBe`
    Position 20 0
  specify "2" $
    moveDown 1 noEdits lines (Position 19 0)
      `shouldBe`
    Position 20 0
  specify "3" $
    moveDown 1 noEdits lines (Position 20 0)
      `shouldBe`
    Position 21 0
  specify "4" $
    moveDown 1 noEdits lines (Position 21 0)
      `shouldBe`
    Position 21 0
  let lines = Tree.new $ Base [-1]
  specify "5" $
    moveDown 1 noEdits lines (Position 1 0)
      `shouldBe`
    Position 1 0
  let lines = Tree.new $ Base [-1,20,30]
  specify "6" $
    moveDown 1 noEdits lines (Position 25 0)
      `shouldBe`
    Position 35 0
  specify "7" $
    moveDown 1 noEdits lines (Position 5 0)
      `shouldBe`
    Position 26 0

moveForwardTests
  :: HasCallStack
  => SpecWith ()
moveForwardTests = describe "moveForward" $ do
  let patch = (insertPatches "?" (Position 3 0) $ insertPatches "?" (Position 2 0) $ noEdits)
  specify "1" $
    moveForward 1 patch (Position 2 0)
      `shouldBe`
    Position 2 1
  specify "2" $
    moveForward 2 patch (Position 2 0)
      `shouldBe`
    Position 3 0
  specify "3" $
    moveForward 3 patch (Position 2 0)
      `shouldBe`
    Position 3 1
  specify "4" $
    moveForward 4 patch (Position 2 0)
      `shouldBe`
    Position 4 0
  let patch = (insertPatches "?" (Position 10 0) $ insertPatches "?" (Position 2 0) $ noEdits)
  specify "5" $
    moveForward 3 patch (Position 2 0)
      `shouldBe`
    Position 4 0
  specify "6" $
    moveForward 1 testPatch1 (Position 7 4)
      `shouldBe`
    Position 7 5
  specify "7" $
    moveForward 1 testPatch1 (Position 7 5)
      `shouldBe`
    Position 11 1
  specify "8" $
    moveForward 1 (Leaf 7 1 "?????" 5) (Position 7 4)
      `shouldBe`
    Position 7 5
  specify "9" $
    moveForward 1 (Leaf 7 1 "?????" 5) (Position 7 5)
      `shouldBe`
    Position 9 0
  specify "10" $
    moveForward 1 (Leaf 8 3 "" 0) (Position 7 0)
      `shouldBe`
    Position 11 0
  let patch  = deletePatches (Position 2 1) 1 $ insertPatches "?" (Position 2 0) $ noEdits
      cursor = moveForward 1 patch $ Position 2 0
  specify "11" $
    moveForward 1 patch cursor
      `shouldBe`
    Position 4 0
  specify "12" $
    moveForward 2 patch (Position 2 0)
      `shouldBe`
    Position 4 0
  let edits = Branch {prefix = 0, switch = 64, left = Leaf {prefix = 0, delete = 0, insert = Nil {color = 1}, length = 0}, right = Leaf {prefix = 108, delete = 18, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "1234\n1234", right = Nil {color = 1}}, length = 9}}
  specify "13" $
    moveForward 1 edits (Position 108 8)
      `shouldBe`
    Position 108 9
  where testPatch1 = Branch 0 8 (Branch 0 4 (Branch 0 1 (Leaf 0 0 (Nil 1) 0) (Leaf 1 1 (Tree 1 1 (Tree 0 1 (Nil 1) "?" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Tree 0 0 (Nil 1) "?" (Nil 1)))) 4)) (Branch 4 2 (Leaf 4 2 (Tree 1 1 (Tree 0 1 (Nil 1) "?" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Tree 0 0 (Nil 1) "?" (Nil 1)))) 4) (Leaf 7 1 (Tree 0 1 (Nil 1) "?????" (Nil 1)) 5))) (Branch 8 4 (Branch 8 2 (Leaf 8 3 (Nil 1) 0) (Leaf 11 1 (Tree 0 1 (Tree 0 1 (Nil 1) "" (Nil 1)) "" (Tree 0 0 (Tree 0 1 (Nil 1) "" (Nil 1)) "?" (Tree 0 1 (Nil 1) "?" (Nil 1)))) 2)) (Leaf 12 4 (Nil 1) 0))

moveUpTests
  :: HasCallStack
  => SpecWith ()
moveUpTests = describe "moveUp" $ do
  let lines = Tree.new $ Base [-1,19,20]
  specify "1" $
    moveUp 1 noEdits lines (Position 20 0)
      `shouldBe`
    Position 0 0
  specify "2" $
    moveUp 1 noEdits lines (Position 21 0)
      `shouldBe`
    Position 20 0
  specify "3" $
    moveUp 1 noEdits lines (Position 22 0)
      `shouldBe`
    Position 20 0
  let edits = Leaf 0 0 (Tree 0 1 (Nil 1) "\n" (Nil 1)) 1
      lines   = Tree 1 1 (Tree 0 1 (Nil 1) (Base [-1]) (Nil 1)) (Offset 0 [0]) (Tree 0 1 (Nil 1) (Base [227,228,229,387,388,389,497]) (Nil 1))
  specify "4" $
    moveUp 1 edits lines (Position 0 1)
      `shouldBe`
    Position 0 0
  specify "5" $
    moveUp 1 edits lines (Position 3 0)
      `shouldBe`
    Position 0 0
  let edits = Leaf {prefix = 0, delete = 0, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "\n", right = Nil {color = 1}}, length = 1}
      lines = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Nil {color = 1}}, node = Offset 0 [0], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [1,2,13,55,56,88,123,159,196,235,274,321], right = Nil { color = 1} }}
  specify "6" $
    moveUp 1 edits lines (Position 276 0)
      `shouldBe`
    Position 237 0
  let base = [1,2,13,55,56,88,123,159,196,235,274,321]
      edits = Leaf {prefix = 0, delete = 0, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "\n", right = Nil {color = 1}}, length = 1}
      lines = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base $ Async.take 1 base, right = Nil {color = 1}}, node = Offset 0 [0], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base $ Async.drop 1 base, right = Nil { color = 1} }}
  specify "7" $
    moveUp 1 edits lines (Position 276 0)
      `shouldBe`
    Position 237 0
  let edits = Branch {prefix = 0, switch = 32, left = Leaf {prefix = 0, delete = 0, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "\n", right = Nil {color = 1}}, length = 1}, right = Leaf {prefix = 56, delete = 0, insert = Tree {index = 0, color = 1, left = Nil {color = 1}, node = "\n", right = Nil {color = 1}}, length = 1}}
      lines = Tree {index = 1, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1], right = Nil {color = 1}}, node = Offset 0 [0], right = Tree {index = 5, color = 0, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [1,2,13,55], right = Nil {color = 1}}, node = Offset 56 [0,1], right = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [88,123,159,196], right = Nil {color = 1}}}}
  specify "8" $
    moveUp 1 edits lines (Position 56 1)
      `shouldBe`
    Position 56 0

removeLinesTests
  :: HasCallStack
  => SpecWith ()
removeLinesTests = describe "removeLines" $ do
  let lines = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Tree {index = 0, color = 0, left = Nil {color = 1}, node = Base [13,55,56], right = Nil {color = 1}}}
  specify "1" $
    Tree.removeLines lines (Position 2 4) (Position 3 0)
      `shouldBe`
    lines
  -- let lines = Tree {index = 4, color = 1, left = Tree {index = 2, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Nil {color = 1}}, node = Base [13,55], right = Nil {color = 2}}, node = Base [88,123,159,196,235,274,321], right = Nil {color = 3}}
  -- specify "2" $
  --   Tree.removeLines lines (Position 1 0) (Position 1 0)
  --     `shouldBe`
  --   lines
  -- note this is not a valid line tree in terms of color, but that should not affect the test here (it would be an error if it did!)
  let lines = Tree {index = 3, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Tree {index = 0, color = 0, left = Nil {color = 1}, node = Offset 2 [1], right = Nil {color = 1}}}, node = Base [13,55], right = Nil {color = 2}}
      lines' = Tree {index = 2, color = 1, left = Tree {index = 0, color = 1, left = Nil {color = 1}, node = Base [-1,1], right = Nil 1}, node = Base [13,55], right = Nil {color = 2}}
  specify "2" $
    Tree.removeLines lines (Position 2 1) (Position 2 2)
      `shouldBe`
    lines'

skipFromToTests
  :: HasCallStack
  => SpecWith ()
skipFromToTests = describe "skipFromTo" $ do
  let lines = Base [2,3,4,10]
  specify "1" $
    skipFromTo (Position 3 0) (Position 4 0) lines
      `shouldBe`
    (Base [2], Base [4,10], 1)
  specify "2" $
    skipFromTo (Position 0 0) (Position 0 5) lines
      `shouldBe`
    (Base [2,3,4,10], empty, 0)
  specify "3" $
    skipFromTo (Position 4 0) (Position 4 5) lines
      `shouldBe`
    (Base [2,3], Base [10], 1)
  specify "4" $
    skipFromTo (Position 2 0) (Position 2 0) lines
      `shouldBe`
    (Base [2,3,4,10], empty, 0)
  specify "5" $
    skipFromTo (Position 10 0) (Position 11 0) lines
      `shouldBe`
    (Base [2,3,4], empty, 1)
  specify "6" $
    skipFromTo (Position 1 0) (Position 2 0) lines
      `shouldBe`
    (Base [2,3,4,10], empty, 0)
  let lines = Offset 4 [3,4,5,6,9]
  specify "7" $
    skipFromTo (Position 4 0) (Position 4 3) lines
      `shouldBe`
    (Offset 4 [0,1,2,3,6], empty, 0)
  specify "8" $
    skipFromTo (Position 4 0) (Position 4 4) lines
      `shouldBe`
    (Offset 4 [0,1,2,5], empty, 1)
  specify "9" $
    skipFromTo (Position 4 3) (Position 4 4) lines
      `shouldBe`
    (Offset 4 [3,4,5,8], empty, 1)
  specify "10" $
    skipFromTo (Position 4 6) (Position 4 9) lines
      `shouldBe`
    (Offset 4 [3,4,5,6], empty, 1)
  specify "11" $
    skipFromTo (Position 4 6) (Position 4 10) lines
      `shouldBe`
    (Offset 4 [3,4,5], empty, 2)
  specify "12" $
    skipFromTo (Position 4 6) (Position 5 0) lines
      `shouldBe`
    (Offset 4 [3,4,5], empty, 2)
