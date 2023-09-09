{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Format
  ( module Format
  , module Format.DataDeclaration
  , module Format.TypeSignature
  ) where

import Parse
  ( Block(Block, comments, contents)
  , ByteString
  , IdFormat(identity)
  , Type(Data, DataInstance, Newtype, Signature)
  , formatDataWith
  , formatSigWith
  , formatWith
  )

import Format.DataDeclaration
import Format.TypeSignature

import Data.ByteString.Lazy qualified as BS hiding (replicate)

import System.Environment (getArgs)

filterStdIn :: IO ()
filterStdIn = BS.interact formatFile

format
  :: Block
  -> ByteString
format (Block Signature    _ comment dec) = formatBlock comment (formatSigWith  (BS.intercalate "\n" dec) formatTyDec  )
format (Block Data         _ comment dec) = formatBlock comment (formatDataWith (BS.intercalate "\n" dec) formatDataDec)
format (Block DataInstance _ comment dec) = formatBlock comment (formatDataWith (BS.intercalate "\n" dec) formatDataDec)
format (Block Newtype      _ comment dec) = formatBlock comment (formatDataWith (BS.intercalate "\n" dec) formatDataDec)
format block                              = formatBlock block.comments $ BS.intercalate "\n" block.contents

formatAndPrint
  :: String
  -> IO ()
formatAndPrint f = do
  file <- BS.readFile f
  BS.putStr $ formatFile file

formatBlock
  :: [ByteString]
  -> ByteString
  -> ByteString
formatBlock []       contents = contents
formatBlock comments contents = BS.concat (map (<> "\n") comments) <> "\n" <> contents

formatFile
  :: ByteString
  -> ByteString
formatFile file = formatWith file identity format

main :: IO ()
main = do
  args <- getArgs
  case (args) of
    [ ] -> filterStdIn
    [f] -> formatAndPrint f
    _   -> error "Formatter called with an unexpected number of arguments!"
