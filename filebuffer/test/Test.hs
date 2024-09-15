module Test where

import Model
import Unit

import Test.Hspec

main
  :: HasCallStack
  => IO ()
main = do
   hspec $ do
     Unit.tests
     Model.tests
