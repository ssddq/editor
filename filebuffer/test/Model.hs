{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}

module Model (modelTests) where

import Common
import Filebuffer

import Unsafe.ByteString

import Data.ByteString       as ByteString
import Data.ByteString.Char8 as Char8

import Data.Knob

import Control.Monad
import System.IO

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()

import Streaming          qualified as S
import Streaming.Internal qualified as S

-- * General

data Action
  = Insert ByteString
  | Move   Int
  | Delete Int
  deriving (Show)


instance Arbitrary Action where
  arbitrary = oneof [ insert, move, delete ]
    where insert = Model.Insert <$> genPrintableByteString
          delete = Model.Delete <$> arbitrary
          move   = Model.Move   <$> arbitrary
          genPrintableByteString = do
            n     <- arbitrary
            chars <- replicateM n $ elements ['a' .. 'z' ]
            return $ Char8.pack chars



-- * Filebuffer model

data Model = Model
  { position   :: Int
  , bytestring :: ByteString
  }
  deriving (Show)


performModel
  :: Action
  -> Model
  -> Model
performModel (Model.Move   n ) Model { position, bytestring }
  = Model
      { position   = max 0 $ min (position + n) $ ByteString.length bytestring
      , bytestring = bytestring
      }
performModel (Model.Insert bs) Model { position, bytestring }
  = Model
      { position   = position + ByteString.length bs
      , bytestring = head <> bs <> tail
      }
  where (head, tail) = ByteString.splitAt position bytestring
performModel (Model.Delete n ) Model { position, bytestring }
  = Model
      { position   = min position $ ByteString.length result
      , bytestring = result
      }
  where (head, tail) = ByteString.splitAt position bytestring
        result = head <> ByteString.drop n tail



-- * Filebuffer

-- | Collects a stream into a bytestring.

collectStream
  :: S.Stream (S.Of Chunk) IO ()
  -> IO ByteString
collectStream stream = case stream of
  S.Return () -> return $ ByteString.empty
  S.Effect m  -> collectStream =<< m
  S.Step ((Chunk _ _ string) S.:> bs) -> do
    rest <- collectStream bs
    return $ string <> rest

perform
  :: Action
  -> Filebuffer
  -> Filebuffer
perform action filebuffer = case (action) of
  Model.Insert bs -> insert bs $ filebuffer
  Model.Delete n  -> if n > 0 then
                       delete n $ filebuffer
                     else
                       filebuffer
  Model.Move   n  -> if (n >= 0) then
                       move Forward (fromIntegral n      ) filebuffer
                     else
                       move Back    (fromIntegral $ abs n) filebuffer

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
check Model{ position, bytestring } filebuffer@Filebuffer{ cursor } =
  filebufferResult `shouldReturn` (bytestring, ByteString.drop position bytestring)
  where toByteString :: Filebuffer -> Position -> IO ByteString
        toByteString Filebuffer{ handle, edits } position =
         collectStream
           $ streamWithPatches handle edits position
           $ S.Return ()
        filebufferResult = do
          full <- toByteString filebuffer $ Position 0 0
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
    withMaxSuccess 10000 $ testActions bytestring actions

-- | This is a small collection of tests
-- | that seems to have good coverage over the tree operations
-- | where there's most likely to be a mistake.

unitTests
  :: HasCallStack
  => SpecWith ()
unitTests = describe "Unit model tests" $ do
  specify "1" $ testActions actions0 ""
  specify "2" $ testActions actions1 "?????????"
  specify "3" $ testActions actions2 "?????????????"
  specify "4" $ testActions actions3 "????????????"
  specify "5" $ testActions actions4 "????"
  specify "6" $ testActions actions5 "????????????????????????????????"
  specify "7" $ testActions actions6 "????????????????????????????????????????"
  where actions0 = [ Model.Insert "ccujhpjy"
                   , Model.Insert "mqb"
                   , Model.Insert "isqbqxho"
                   , Model.Move   (-14)
                   , Model.Delete 11
                   ]
        actions1 = [ Model.Move   64
                   , Model.Delete 1
                   , Model.Move   (-55)
                   , Model.Move   9
                   , Model.Delete 54
                   ]
        actions2 = [ Model.Insert "wiiyswvfveytfywfyvl"
                   , Model.Move   12
                   , Model.Delete 5
                   , Model.Move   8
                   , Model.Move   (-17)
                   , Model.Delete 2
                   ]
        actions3 = [ Model.Delete 11
                   , Model.Move   0
                   , Model.Delete 45
                   , Model.Insert "gvbsli"
                   ]
        actions4 = [ Model.Move   3
                   , Model.Insert "eaeuzjrmzlauwhdwlenbjcegtdpsmcoycidwgqronee"
                   , Model.Move   43
                   , Model.Delete 22
                   , Model.Move   (-33)
                   , Model.Move   33
                   , Model.Insert "okbwfzrn"
                   ]
        actions5 = [ Model.Move   1
                   , Model.Insert ""
                   , Model.Move   10
                   , Model.Delete 22
                   , Model.Move   (-1)
                   , Model.Move   1
                   , Model.Insert "i"
                   ]
        actions6 = [ Model.Delete 3
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



-- * Tests

modelTests
  :: HasCallStack
  => SpecWith ()
modelTests = do
  unitTests
  quickCheckTests
