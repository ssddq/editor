{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lex.Tokens
  ( Alex
  , AlexPosn (..)
  , alexError
  , alexGetInput
  , alexMonadScan
  , runAlexTokens
  , Token (..)
  ) where

import Data.ByteString.Lazy (ByteString)

import Data.ByteString.Lazy qualified as BS
}

%wrapper "monad-bytestring"

@space = ( \t | \ )
$space = [ \t  \  ]

$letter  = [a-zA-Z]
$numeral = [0-9]


@inlineComment = "--" (~\n)*
@multilineComment = "{-" (. | $white)* "-}"
@comment = (@inlineComment | @multilineComment)

@rest = (~[$space \,])+

@decCmt = "-- |" (~\n)*
@cmt = "--" ($letter | @space)  (~\n)*

@type         = "type "
@class        = "class "
@instance     = "instance "
@import       = "import "
@data         = "data "
@default      = "default "
@newtype      = "newtype "
@typeFamily   = "type family "
@dataFamily   = "data family "
@typeInstance = "type instance "
@dataInstance = "data instance "
@deriving     = "deriving "
@foreign      = "foreign "
@pattern      = "pattern "
@where        = "where "

@pragmaWord = ("INLINE" | "NOINLINE" | "INLINABLE" | "SPECIALIZE" | "SPECIALISE" | "CONLIKE" | "ANN" | "DEPRECATED" | "WARNING" | "OVERLAPPING" | "OVERLAPPABLE" | "OVERLAPS" | "INCOHERENT" )
@stage = "[" "~"* $numeral* "]"

@unpack = "{-# UNPACK #-}"

$symbol = [ \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \: ]
@operator = "(" $symbol+ ")"

@name = (($letter | $numeral | "_" | "'")+ \#{0,1} | "*" | @operator )
@qualifiedName = (@name ".")* @name
@stringLiteral = \" (~[\" \n])* \"

tokens :-

<0> $white+ ;

<0> @class        { symbol Class        }
<0> @cmt          { string Comment      }
<0> @dataFamily   { symbol DataFamily   }
<0> @dataInstance { symbol DataInstance }
<0> @data         { symbol Data         }
<0> @decCmt       { string Comment      }
<0> @default      { symbol Default      }
<0> @deriving     { symbol Deriving     }
<0> @foreign      { symbol Foreign      }
<0> @instance     { symbol Instance     }
<0> @import       { symbol Import       }
<0> @newtype      { symbol Newtype      }
<0> @typeFamily   { symbol TypeFamily   }
<0> @typeInstance { symbol TypeInstance }
<0> @type         { symbol Type         }
<0> @unpack       { symbol Unpack       }
<0> @pattern      { symbol Pattern      }
<0> @where        { symbol Where        }

<0> "{-# " { symbol PragmaStart `andBegin` pragma }

-- Keywords
<0> "forall"       { symbol Forall }

-- Key separators
<0> "."            { symbol Period     }
<0> "=>"           { symbol ConstraintArrow }

-- Separators
<0> "->"           { symbol Arrow }
<0> "<-"           { symbol Bind  }
<0> "="            { symbol Equal }
<0> ","            { symbol Comma }
<0> "|"            { symbol Guard }

<0> "::"           { symbol DoubleColon }
<0> "~"            { symbol Tilde       }

-- Symbols
<0> "*"            { symbol Star }
<0> "@"            { symbol At   }
<0> "!"            { symbol Bang }

-- Delimiters
<0> "("            { symbol OpenParen    }
<0> "("            { symbol OpenParen    }
<0> "(#"           { symbol OpenUnboxedParen    }
<0> "#)"           { symbol CloseUnboxedParen    }
<0> ")"            { symbol CloseParen   }
<0> "["            { symbol OpenBracket  }
<0> "]"            { symbol CloseBracket }
<0> "{"            { symbol OpenBrace    }
<0> "}"            { symbol CloseBrace   }

-- Identifiers
<0> @name          { string Name }
<0> @operator      { string Name }
<0> @qualifiedName { string Name }
<0> @stringLiteral { string Name }

<pragma> ",";
<pragma> $white+;
<pragma> " #-}" { symbol PragmaStop `andBegin` 0 }
<pragma> @pragmaWord;
<pragma> @stage;
<pragma> @type;
<pragma> @name { string Name }
<pragma> @rest;

{

data Token
  = Name    BS.ByteString
  | Comment BS.ByteString
  | Arrow
  | At
  | Bang
  | Bind
  | Class
  | CloseBrace
  | CloseBracket
  | CloseParen
  | CloseUnboxedParen
  | Comma
  | ConstraintArrow
  | Data
  | DataFamily
  | DataInstance
  | Default
  | Deriving
  | DoubleColon
  | Equal
  | Forall
  | Foreign
  | Guard
  | Import
  | Instance
  | Newtype
  | OpenBrace
  | OpenBracket
  | OpenParen
  | OpenUnboxedParen
  | Pattern
  | Period
  | PragmaStart
  | PragmaStop
  | Star
  | Tilde
  | Type
  | TypeFamily
  | TypeInstance
  | Unpack
  | Where
  | EOL
  deriving (Show)

alexEOF :: Alex Token
alexEOF = pure $ EOL

string :: (BS.ByteString -> Token) -> AlexAction Token
string token (_, _, str, _) len = pure $ token (BS.take len str)

symbol :: Token -> AlexAction Token
symbol token _ _ = pure $ token

runAlexTokens :: ByteString -> Alex a -> Either String a
runAlexTokens = runAlex
}
