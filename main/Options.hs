{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module Options where

import Renderer (Area(..), Constants(..), defaultConstants)

import Text.Read

import Control.Applicative
import Options.Applicative

options :: ParserInfo Options
options = info (opts <**> helper)
  ( fullDesc <> header "editor"
             <> progDesc "An experimental Vulkan-based graphical text editor.\n For more information, visit https://github.com/ssddq/editor."
  )
  where opts = Options <$> constants <*> fontFilePath <*> filePath

data Options = Options
  { constants :: Constants
  , fontFile  :: Maybe FilePath
  , file      :: FilePath
  }

constants :: Parser Constants
constants
  = parseMaybe renderUpdate renderParse
  $ parseMaybe fSizeUpdate  fSizeParse
  $ parseMaybe ppiUpdate    ppiParse
  $ pure defaultConstants
  where ppiUpdate ppi c = c { ppi }
        ppiParse = optional $ option auto (long "dpi" <> metavar "FLOAT" <> help "Display DPI")
        fSizeUpdate fSize c = c { fSize }
        fSizeParse = optional $ option auto (long "size" <> metavar "FLOAT" <> help "Font size")
        renderUpdate render c = c { render }
        renderParse = optional $ option readerArea (long "render" <> metavar "INTxINT" <> help "Render size, as <height>x<width>")

readerArea :: ReadM Area
readerArea = maybeReader reader
  where reader str = case (readMaybe heightStr, readMaybe $ drop 1 widthStr) of
          (Just height, Just width) -> Just $ Area height width
          _ -> Nothing
          where (heightStr, widthStr) = span (/= 'x') str

-- | Update accumulator with the parsed value,
-- | if a value was actually parsed.

parseMaybe :: (a -> b -> b) -> Parser (Maybe a) -> Parser b -> Parser b
parseMaybe f = liftA2 $ \maybe b -> case (maybe) of
  Just a  -> f a b
  Nothing -> b

filePath :: Parser FilePath
filePath = argument str (metavar "FILE")

fontFilePath :: Parser (Maybe FilePath)
fontFilePath = optional $ strOption
  ( long "font" <> metavar "FILE" <> help "TTF file for font" )
