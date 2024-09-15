{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Model where

import Common
import Filebuffer

import Data.Lines
import Data.Tree
import Data.Vector.Async qualified as Async

import Streaming          qualified as S
import Streaming.Internal qualified as S

import Unsafe.ByteString

import Control.Monad
import System.IO

import Data.ByteString          as ByteString
import Data.ByteString.Char8    as Char8
import Data.ByteString.Internal as Internal

import Data.Knob

import Data.Vector         as Vector
import Data.Vector.Unboxed qualified as Unboxed


import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

-- * General

data Action
  = Move     Int
  | LineMove Int
  | Insert   ByteString
  | Delete   Int
  deriving (Show)


instance Arbitrary Action where
  arbitrary = oneof [ insert, move, lineMove, delete ]
    where insert   = Model.Insert   <$> genPrintableByteString
          delete   = Model.Delete   <$> arbitrary
          move     = Model.Move     <$> arbitrary
          lineMove = Model.LineMove <$> arbitrary
          genPrintableByteString = do
            n     <- arbitrary
            chars <- Control.Monad.replicateM n $ elements $ '\n' : [ 'a' .. 'z' ]
            return $ Char8.pack chars



-- * Filebuffer model

data Model = Model
  { position   :: Int
  , bytestring :: ByteString
  }
  deriving (Show)


getLines
  :: Model
  -> Vector Int
getLines Model{ bytestring } = Vector.fromList $ -1 : ByteString.elemIndices (Internal.c2w '\n') bytestring

lineMoveModel
  :: Int
  -> Model
  -> Model
lineMoveModel count model@Model{ position, bytestring } = model { position = newPosition }
  where linePositions = getLines model
        lineNumber = case (Vector.findIndex (\(n, m) -> n <= position && m >= position) $ Vector.zip linePositions $ Vector.drop 1 linePositions) of
          Just n  -> n
          Nothing -> max 0 $ Vector.length linePositions - 1
        lineStart  = linePositions ! lineNumber
        lineOffset = position - lineStart
        clamp n = max 0 $ min n $ Vector.length linePositions - 1
        targetLineNumber     = clamp $ lineNumber + count
        targetNextLineNumber = clamp $ lineNumber + count + 1
        targetLineStart      = linePositions ! targetLineNumber
        targetNextLineStart  = linePositions ! targetNextLineNumber
        newPosition
          | targetLineStart >= targetNextLineStart = min (ByteString.length bytestring) $ targetLineStart + lineOffset
          | otherwise = min targetNextLineStart $ targetLineStart + lineOffset

performModel
  :: Action
  -> Model
  -> Model
performModel action model@Model{ position, bytestring } = case (action) of
  Model.LineMove n  -> lineMoveModel n model
  Model.Move     n  -> Model
                         { position   = max 0 $ min (position + n) $ ByteString.length bytestring
                         , bytestring = bytestring
                         }
  Model.Insert   bs -> Model
                         { position   = position + ByteString.length bs
                         , bytestring = head <> bs <> tail
                         }
                       where (head, tail) = ByteString.splitAt position bytestring
  Model.Delete   n -> Model
                        { position   = min position $ ByteString.length result
                        , bytestring = result
                        }
                       where (head, tail) = ByteString.splitAt position bytestring
                             result = head <> ByteString.drop n tail

toSubstrings
  :: Model
  -> Vector Int
  -> Vector ByteString
toSubstrings Model{ bytestring } = Vector.map $ \n -> ByteString.drop n bytestring



-- * Filebuffer

collectLines
  :: Tree Lines
  -> IO (Vector Position)
collectLines (Nil _) = return $ Vector.empty
collectLines Tree{ left, node, right } = do
  leftLines  <- collectLines left
  nodeLines  <- calculateLines
  rightLines <- collectLines right
  return $ leftLines <> nodeLines <> rightLines
  where calculateLines = case (node) of
          Base vector        -> Vector.map (\base   -> Position base 0     ) <$> fmap Unboxed.convert (Async.force vector)
          Offset base vector -> Vector.map (\offset -> Position base offset) <$> fmap Unboxed.convert (Async.force vector)

perform
  :: Action
  -> Filebuffer
  -> Filebuffer
perform action filebuffer = case (action) of
  Model.Insert bs -> insert bs $ filebuffer
  Model.Delete n
    | n > 0     -> delete n $ filebuffer
    | otherwise -> filebuffer
  Model.Move n
    | n >= 0    -> move Forward (fromIntegral       n) filebuffer
    | otherwise -> move Back    (fromIntegral $ abs n) filebuffer
  Model.LineMove n
    | n >= 0    -> move Down (fromIntegral       n) filebuffer
    | otherwise -> move Up   (fromIntegral $ abs n) filebuffer

-- | Collects a stream into a bytestring.

toByteString
  :: Filebuffer
  -> Position
  -> IO ByteString
toByteString Filebuffer{ handle, edits } position =
  collectStream
    $ streamWithPatches handle edits position
    $ S.Return ()
  where collectStream :: S.Stream (S.Of Chunk) IO () -> IO ByteString
        collectStream stream = case stream of
          S.Return () -> return $ ByteString.empty
          S.Effect m  -> collectStream =<< m
          S.Step ((Chunk _ _ string) S.:> bs) -> do
            rest <- collectStream bs
            return $ string <> rest

toByteStrings
  :: Filebuffer
  -> Vector Position
  -> IO (Vector ByteString)
toByteStrings filebuffer = Vector.mapM $ toByteString filebuffer

withByteString
  :: ByteString
  -> (Model -> Filebuffer -> IO a)
  -> IO a
withByteString bytestring f = do
  knob <- newKnob bytestring
  withFileHandle knob "test handle" ReadMode
    $ \handle -> withFileHandle knob "test line handle" ReadMode
    $ \asyncHandle -> f model =<< filebuffer handle asyncHandle
  where filebuffer :: Handle -> Handle -> IO Filebuffer
        filebuffer handle asyncHandle = do
          size  <- fromIntegral <$> hFileSize handle
          lines <- scanLines asyncHandle
          return $ Filebuffer
            { cursor = Position 0 0
            , start  = Position 0 0
            , size
            , handle
            , edits  = noEdits
            , lines
            , visualLineCount = 0
            }
        model = Model { position = 0, bytestring }



-- * Utils

-- | Check that a Filebuffer agrees with a model bytestring.
-- |
-- | Note that this function verifies that:
-- | * the filebuffer with its edits produces the same bytestring as the model, and that
-- | * the position in the filebuffer mirrors the position in the model.
-- | The latter check in particular is extremely important,
-- | and means the tests are much more likely to fail early in some otherwise subtle cases.

check
  :: Model
  -> Filebuffer
  -> Expectation
check model@Model{ position, bytestring } filebuffer@Filebuffer{ cursor, lines } = do
  (toByteStrings filebuffer =<< collectLines lines) `shouldReturn` (toSubstrings model $ getLines model)
  filebufferResult `shouldReturn` (bytestring, ByteString.drop position bytestring)
  where filebufferResult = do
          full <- toByteString filebuffer $ Position (-1) 0
          tail <- toByteString filebuffer $ cursor
          return $ (full, tail)

checkActions
  :: Model
  -> Filebuffer
  -> [Action]
  -> Expectation
checkActions model filebuffer [] = check model filebuffer
checkActions model filebuffer (action : actions) = do
  check        model' filebuffer'
  checkActions model' filebuffer' actions
  where filebuffer' = perform      action filebuffer
        model'      = performModel action model

testActions
  :: HasCallStack
  => [Action]
  -> ByteString
  -> Expectation
testActions actions bytestring = withByteString bytestring
                               $ \model filebuffer -> checkActions model filebuffer actions



-- * Unsafe bytestring tests

unsafeInsertByteStringTest
  :: HasCallStack
  => ByteString
  -> Int
  -> ByteString
  -> Expectation
unsafeInsertByteStringTest insert offset' target =
  (head <> tail) `shouldBe` (head' <> insert <> tail')
  where offset = max 0 $ min offset' $ ByteString.length target
        (head,  tail ) = unsafeInsertByteString insert offset target
        (head', tail') = ByteString.splitAt offset target

unsafeRemoveByteStringTest
  :: HasCallStack
  => Int
  -> Int
  -> ByteString
  -> Expectation
unsafeRemoveByteStringTest offset' count' bytestring =
  (result) `shouldBe` (head <> ByteString.drop count tail)
  where offset = max 0 $ min offset' $ ByteString.length bytestring
        count  = max 0 $ min count'  $ ByteString.length bytestring - offset
        result = unsafeRemoveByteString offset count bytestring
        (head, tail) = ByteString.splitAt offset bytestring



-- * Model tests

quickCheckTests
  :: HasCallStack
  => SpecWith ()
quickCheckTests = describe "QuickCheck model tests" $ do
  prop "unsafeInsertByteString" $ \insert offset target ->
    unsafeInsertByteStringTest insert offset target
  prop "unsafeRemoveByteString" $ \offset count bytestring ->
    unsafeRemoveByteStringTest offset count bytestring
  prop "filebuffer actions"     $ \bytestring actions ->
    withMaxSuccess 1000000 $ testActions bytestring actions

-- | This is a small collection of tests
-- | that seems to have good coverage over the tree operations
-- | where there's most likely to be a mistake.

unitTests
  :: HasCallStack
  => SpecWith ()
unitTests = describe "Unit model tests" $ do
  specify "1"  $ testActions actions1  ""
  specify "2"  $ testActions actions2  "?????????"
  specify "3"  $ testActions actions3  "?????????????"
  specify "4"  $ testActions actions4  "????????????"
  specify "5"  $ testActions actions5  "????"
  specify "6"  $ testActions actions6  "????????????????????????????????"
  specify "7"  $ testActions actions7  "????????????????????????????????????????"
  specify "8"  $ testActions actions8  "??????\n??????????????????????????\n"
  specify "9"  $ testActions actions9  "??????????????????????"
  specify "10" $ testActions actions10 "\n??????\n???????????????????"
  specify "11" $ testActions actions11 "?"
  specify "12" $ testActions actions12 ""
  specify "13" $ testActions actions13 ""
  specify "14" $ testActions actions14 ""
  specify "15" $ testActions actions15 "?"
  specify "16" $ testActions actions16 ""
  specify "17" $ testActions actions17 ""
  specify "18" $ testActions actions18 ""
  specify "19" $ testActions actions19 "??????\n?"
  specify "20" $ testActions actions20 "?????????????\n"
  specify "21" $ testActions actions21 ""
  where actions1  = [ Model.Insert "ccujhpjy"
                    , Model.Insert "mqb"
                    , Model.Insert "isqbqxho"
                    , Model.Move   (-14)
                    , Model.Delete 11
                    ]
        actions2  = [ Model.Move   64
                    , Model.Delete 1
                    , Model.Move   (-55)
                    , Model.Move   9
                    , Model.Delete 54
                    ]
        actions3  = [ Model.Insert "wiiyswvfveytfywfyvl"
                    , Model.Move   12
                    , Model.Delete 5
                    , Model.Move   8
                    , Model.Move   (-17)
                    , Model.Delete 2
                    ]
        actions4  = [ Model.Delete 11
                    , Model.Move   0
                    , Model.Delete 45
                    , Model.Insert "gvbsli"
                    ]
        actions5  = [ Model.Move   3
                    , Model.Insert "eaeuzjrmzlauwhdwlenbjcegtdpsmcoycidwgqronee"
                    , Model.Move   43
                    , Model.Delete 22
                    , Model.Move   (-33)
                    , Model.Move   33
                    , Model.Insert "okbwfzrn"
                    ]
        actions6  = [ Model.Move   1
                    , Model.Insert ""
                    , Model.Move   10
                    , Model.Delete 22
                    , Model.Move   (-1)
                    , Model.Move   1
                    , Model.Insert "i"
                    ]
        actions7  = [ Model.Delete 3
                    , Model.Move   16
                    , Model.Delete 5
                    , Model.Move   50
                    , Model.Insert "rmib"
                    , Model.Insert "zrcvpmhgeqvsigrverpvzotqezhmaiansyugezvawjbhuwhqujwcmaqatbtlkz"
                    , Model.Move   (-85)
                    , Model.Move   3
                    , Model.Delete 45
                    , Model.Move   47
                    , Model.Delete 25
                    ]
        actions8  = [ Model.Delete 33 ]
        actions9  = [ Model.Insert "dwdliqouo"
                    , Model.Move   55
                    , Model.Insert "\nkjsh"
                    , Model.Insert "vpujhuqgxlpaogevl\ncyolmq"
                    , Model.Move   (-59)
                    , Model.Delete 45
                    ]
        actions10 = [ Model.Move   37
                    , Model.Insert "stopbekzeojfm\nfbh\nxkuqpbjyfzfealewfxykssfxe\nlnvz"
                    , Model.Move   (-74)
                    , Model.Insert "clslcqcgohqtlepkbmmvyygftwhufibcebxccsdra\nmgzuv\nljmlhnmtdvnwhlcnr\njenzpe\ncmfzb\nfwiue\nb"
                    ]
        actions11 = [ Model.LineMove (-2) ]
        actions12 = [ Model.LineMove 1    ]
        actions13 = [ Model.Delete   6
                    , Model.LineMove 5
                    ]
        actions14 = [ Model.Insert   "exhvx"
                    , Model.LineMove 3
                    ]
        actions15 = [ Model.Move 1
                    , Model.LineMove (-6)
                    ]
        actions16 = [ Model.Insert "tl\nt"
                    , Model.LineMove (-7)
                    ]
        actions17 = [ Model.Insert "hzft\nveiq"
                    , Model.Move   (-7)
                    , Model.LineMove 9
                    ]
        actions18 = [ Model.Insert   "\n"
                    , Model.Move     (-10)
                    , Model.LineMove 14
                    ]
        actions19 = [ Model.Insert   "tirleqjgj\n"
                    , Model.LineMove 14
                    , Model.Move     (-8)
                    , Model.LineMove 29
                    ]
        actions20 = [ Model.Move     13
                    , Model.LineMove 15
                    , Model.Move     (-10)
                    ]
        actions21 = [ Model.Insert   "fbjlxsctafqypzssu\n\nvzkfa"
                    , Model.LineMove (-1)
                    ]



-- * Tests

tests
  :: HasCallStack
  => SpecWith ()
tests = do
  unitTests
  quickCheckTests
