module Test where

import Unit
import Model

import Test.Hspec

main
  :: HasCallStack
  => IO ()
main = do
   hspec $ do
     unitTests
     modelTests
