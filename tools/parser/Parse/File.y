{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleContexts #-}

module Parse.File
  ( Block  (..)
  , File   (..)
  , Header (..)
  , parseBlocks
  ) where

import Types

import Lex.Lines
}

%name parseBlocks

%tokentype { Line }
%error     { parseError }

%lexer     { lexer } { Line EOF _ _ }

%monad     { Alex } { >>= } { pure }

%token

   class            { Line Class           _ _ }
   comment          { Line Comment         _ _ }
   data             { Line Data            _ _ }
   data_family      { Line DataFamily      _ _ }
   data_instance    { Line DataInstance    _ _ }
   default          { Line Default         _ _ }
   definition       { Line Definition      _ _ }
   deriving         { Line Deriving        _ _ }
   fixity           { Line Fixity          _ _ }
   foreign          { Line Foreign         _ _ }
   import           { Line Import          _ _ }
   instance         { Line Instance        _ _ }
   module           { Line Module          _ _ }
   nested           { Line Nested          _ _ }
   newtype          { Line Newtype         _ _ }
   pattern          { Line Pattern         _ _ }
   pragma_header    { Line PragmaHeader    _ _ }
   pragma           { Line Pragma          _ _ }
   section          { Line SectionHeader   _ _ }
   signature        { Line Signature       _ _ }
   template_haskell { Line TemplateHaskell _ _ }
   type_family      { Line TypeFamily      _ _ }
   type_instance    { Line TypeInstance    _ _ }
   type             { Line Type            _ _ }

%expect 0

%%

file
  : header sections { File $1 $2 }

sections
  :                         { [] }
  | blocks                  { [Section "" $1]}
  | sections section blocks { $1 <> [Section $2.text $3] }

header
  : header_lines        { Header $1.number $1.text }
  | header header_lines { addLine $1 $2 }

header_lines
  : module        { $1 }
  | import        { $1 }
  | pragma_header { $1 }
  | nested        { $1 }

blocks
  : %shift       { [] }
  | blocks block { $1 <> [$2] }

comments
  : %shift           { [] }
  | comments comment { $1 <> [$2] }

body
  :             { [] }
  | body nested { $1 <> [$2.text] }

block
 : comments class            body { parse Class           $1 $2 $3 }
 | comments data             body { parse Data            $1 $2 $3 }
 | comments data_family      body { parse DataFamily      $1 $2 $3 }
 | comments data_instance    body { parse DataInstance    $1 $2 $3 }
 | comments default          body { parse Default         $1 $2 $3 }
 | comments definition       body { parse Definition      $1 $2 $3 }
 | comments deriving         body { parse Deriving        $1 $2 $3 }
 | comments fixity           body { parse Fixity          $1 $2 $3 }
 | comments foreign          body { parse Foreign         $1 $2 $3 }
 | comments instance         body { parse Instance        $1 $2 $3 }
 | comments newtype          body { parse Newtype         $1 $2 $3 }
 | comments pattern          body { parse Pattern         $1 $2 $3 }
 | comments pragma           body { parse Pragma          $1 $2 $3 }
 | comments signature        body { parse Signature       $1 $2 $3 }
 | comments template_haskell body { parse TemplateHaskell $1 $2 $3 }
 | comments type             body { parse Type            $1 $2 $3 }
 | comments type_family      body { parse TypeFamily      $1 $2 $3 }
 | comments type_instance    body { parse TypeInstance    $1 $2 $3 }

{

type Comment = ByteString

parse
  :: Type
  -> [Line]
  -> Line
  -> [ByteString]
  -> Block
parse blockType comments line body = Block blockType position (map (.text) comments) (line.text : body)
  where position = minimum $ line.number : map (.number) comments

data File = File Header [Section]
  deriving Show

data Header = Header Int ByteString
  deriving Show

addLine :: Header -> Line -> Header
addLine (Header n bs) line = Header n $ bs <> line.text

toHeader :: Line -> Header
toHeader line = Header (line.number) (line.text)

parseError :: Line -> Alex a
parseError t = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column <> "\nLast symbol parsed: " <> show t

lexer :: (Line -> Alex a) -> Alex a
lexer = \f -> alexMonadScan >>= f
}
