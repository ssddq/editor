{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module Preprocessor
  ( generate
  , main
  , mkName
  , nameBase
  , process
  , quantifyExcept
  , scopedTypeVariable
  ) where

import Format (formatBlock, formatQtfyVars, formatTyExp)
import Lib    (generate, quantifyExcept, scopedTypeVariable)
import Parse
  ( Block(Block, comments, contents, position)
  , ByteString
  , Expression(Appl, Unit)
  , Function(..)
  , IdFormat(identity)
  , Type(Signature)
  , TypeSignature(..)
  , Unit(..)
  , Variable(name)
  , formatSigWith
  , processWith
  )

import Data.ByteString.Char8 qualified as B
import Data.ByteString.Lazy  qualified as BS

import Data.Char (isLower)

import Language.Haskell.TH (mkName, nameBase)

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case (args) of
    original : input : output : _ -> do
      src <- BS.readFile input
      BS.writeFile output $ process original src
    _ -> error "Preprocessor called with unexpected number of arguments."

process
  :: String
  -> ByteString
  -> ByteString
process filename file = processWith file identity (processBlock filename)

processBlock
  :: String
  -> Block
  -> ByteString
processBlock filename (Block Signature n comments contents) = formatBlock comments $
  "{-# LINE " <> (BS.fromStrict $ B.pack $ show n) <> " \"" <> (BS.fromStrict $ B.pack filename)  <> "\" #-}\n" <> line <> "\n"
  where line = formatSigWith (BS.intercalate "\n" contents) processDec
processBlock filename block = formatBlock block.comments $
  "{-# LINE " <> (BS.fromStrict $ B.pack $ show n) <> " \"" <> (BS.fromStrict $ B.pack filename)  <> "\" #-}\n" <> contents
  where n = block.position
        contents = BS.intercalate "\n" block.contents

processDec
  :: TypeSignature
  -> ByteString
processDec (TypeSignature name sig) = name <> "\n  :: " <> case (sig) of
  Quantification vars fun -> case (vars) of
    [] -> "$(quantifyExcept [] [t| " <> processFun fun <> " |])\n"
    _  -> "forall " <> formatQtfyVars vars <> ". $(quantifyExcept [ \"" <> BS.intercalate "\", \"" (map (.name) vars)
                      <> "\" ] [t| " <> processFun fun <> " |])\n"
  _ -> "$(quantifyExcept [] [t| " <> processFun sig <> " |])\n"

processEq
  :: (ByteString, Expression)
  -> ByteString
processEq (str, exp) = "(\"" <> str <> "\", \"" <> formatTyExp exp <> "\")"

processExp
  :: Expression
  -> ByteString
processExp (Unit grp)     = processGrp grp
processExp (Appl exp grp) = processExp exp <> " "   <> processGrp grp
processExp exp            = formatTyExp exp

processFun
  :: Function
  -> ByteString
processFun sig = case (sig) of
  Expression     exp      _ -> processExp exp
  Function       exp  fun _ -> processExp exp  <> "\n  -> " <> processFun fun
  Constraint     ctxt fun _ -> processExp ctxt <> "\n  => " <> processFun fun
  Quantification vars fun   -> "forall " <> formatQtfyVars vars <> ". " <> processFun fun

processGrp
  :: Unit
  -> ByteString
processGrp (Atm str) =
  if (BS.null str) then
    str
  else case (isLower . toEnum . fromIntegral $ BS.head str) of
    True  -> "$(scopedTypeVariable \"" <> str <> "\")"
    False -> str
processGrp (Par fun) = "(" <> processFun fun <> ")"
processGrp (Lst fun) = "[" <> processFun fun <> "]"
processGrp (Tpl fun) = "(" <> BS.intercalate ", " (map processFun fun) <> ")"
processGrp (Utp fun) = "(# " <> BS.intercalate ", " (map processFun fun) <> " #)"
processGrp (Tpr str eqs) = "$(replace_" <> str <> " [" <> (BS.intercalate ", " $ map processEq eqs)  <> "])"
