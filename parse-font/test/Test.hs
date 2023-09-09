{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell     #-}

module Test where

import Font

import Data.ByteString
import Data.FileEmbed
import Data.Vector     qualified as V

import Test.Hspec

firaCodeTTF :: ByteString
firaCodeTTF = $(makeRelativeToProject "../fonts/FiraCode/FiraCode.ttf" >>= embedFile)

genericTests
  :: HasCallStack
  => Font
  -> SpecWith ()
genericTests font = do
  describe "General tests:" $ do
    specify "Length of glyf.glyphs = maxp.numGlyphs" $ V.length font.glyf.glyphs `shouldBe` fromIntegral font.maxp.numGlyphs


firaCodeTests
  :: HasCallStack
  => SpecWith ()
firaCodeTests = do
  font <- runIO $ parse_Font <$> loadTables firaCodeTTF
  let glyph0 = font.glyf.glyphs V.! 0
  let glyph1 = font.glyf.glyphs V.! 1
  let glyph85 = font.glyf.glyphs V.! 85
  describe "FiraCode:" $ do
    describe "cmap:" $ do
      specify "numTables" $ font.cmap.numTables == 4
    describe "maxp:" $ do
      specify "numGlyphs" $ font.maxp.numGlyphs == 1846
    describe "glyf:" $ do
      specify "xCoordinates in glyph 1" $ glyph1.xCoordinates `shouldBe` ( V.fromList [ 887, 301, 180, 60, 515, 684, 1140, 1008, 344, 845, 596 ])
      specify "endPtsOfContours in glyph 1" $ glyph1.endPtsOfContours `shouldBe` ( V.fromList [7, 10])
      specify "numDraws in glyph 1" $ glyph1.numDraws `shouldBe` 11
      specify "numDraws in glyph 0" $ glyph0.numDraws `shouldBe` 68
      specify "numDraws in glyph 85" $ glyph85.numDraws `shouldBe` (26*2 + 1)
    genericTests font


main :: IO ()
main = do
  hspec $ do
    firaCodeTests
