{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Wrap where

import Format (format, formatBlock, formatTyDec)
import Parse
  ( Block(Block, comments, contents)
  , ByteString
  , IdFormat(identity)
  , Type(Signature)
  , formatSigWith
  , formatWith
  , hiddenRegionEnd
  , hiddenRegionStart
  )

import Data.ByteString.Lazy qualified as BS

import System.Environment (getArgs)
import System.IO          (hFlush, stdin)
import System.IO.Temp     (withSystemTempFile)
import System.Process     (spawnCommand, waitForProcess)

commentOutSignatures
  :: Block
  -> ByteString
commentOutSignatures (Block Signature _ comment dec)
  = formatBlock comment (formatSigWith  (BS.intercalate "\n" dec) $ \s -> hiddenRegionStart <> formatTyDec s <> hiddenRegionEnd)
commentOutSignatures block = formatBlock block.comments $ BS.intercalate "\n" block.contents

filterStdInWith
  :: String
  -> IO ()
filterStdInWith formatter = do
  input <- BS.hGetContents stdin
  withSystemTempFile "tmp.in" $ \fileIn hdlIn ->
    withSystemTempFile "tmp.out" $ \fileOut hdlOut -> do
      BS.hPut hdlIn $ formatFile input
      hFlush hdlIn
      waitForProcess =<< (spawnCommand $ formatter <> " " <> fileIn <> " > " <> fileOut)
      output <- BS.hGetContents hdlOut
      BS.putStr $ uncomment output

formatFile
  :: ByteString
  -> ByteString
formatFile file = formatWith file identity commentOutSignatures

formatFileWith
  :: String
  -> String
  -> IO ()
formatFileWith formatter file = do
  input <- BS.readFile file
  withSystemTempFile "tmp.in" $ \fileIn hdlIn -> withSystemTempFile "tmp.out" $ \fileOut hdlOut -> do
    BS.hPut hdlIn $ formatFile input
    hFlush hdlIn
    waitForProcess =<< (spawnCommand $ formatter <> " " <> fileIn <> " > " <> fileOut)
    output <- BS.hGetContents hdlOut
    BS.writeFile file $ uncomment output

main :: IO ()
main = do
  args <- getArgs
  case (args) of
    []                -> error "Formatter name required"
    [formatter]       -> filterStdInWith formatter
    [formatter, file] -> formatFileWith formatter file
    _                 -> error "Formatter called with an unexpected number of arguments!"

uncomment
  :: ByteString
  -> ByteString
uncomment file = formatWith file identity format
