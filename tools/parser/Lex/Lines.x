{
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Lex.Lines
  ( Alex
  , AlexPosn (..)
  , Line     (..)
  , alexError
  , alexGetInput
  , alexMonadScan
  , hiddenRegionStart
  , hiddenRegionEnd
  , runAlexLines
  ) where

import Types

import Data.ByteString.Lazy qualified as BS

}

%wrapper "monad-bytestring"


$letter  = [a-zA-Z]
$numeral = [0-9]

$symbol = [ \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \: ]

@space = ( \t | \ )
@line  = (~\n)*

@empty    = @space+ \n

@pragma_header = ("{-# OPTIONS_GHC" | "{-# LANGUAGE ") @line (@space* \n)*

@module        = "module "        @line (@space* \n)*
@import        = "import "        @line (@space* \n)*

@class         = "class "         @line

@default       = "default "       @line
@foreign       = "foreign "       @line
@instance      = "instance "      @line
@newtype       = "newtype "       @line
@pattern       = "pattern "       @line
@deriving      = "deriving "      @line

@data          = "data "          @line
@data_family   = "data family "   @line
@data_instance = "data instance " @line

@type_family   = ("type family " | "type ") @line
@type_instance = "type instance "           @line

@type          = "type " (~\n)* "="         @line

@comment = "--" (($letter | @space)  (~\n)* | \n)
@section = "-- *" (~\n)*

@pragma  = "{-# " @line

@identifier = ($letter | $numeral | "_" | "'")+ "#"{0,1}

@name = (@identifier | "(" $symbol+ ")" )

@signature_single_line = (@name  )  "::" @line
@signature_multiline   = (@name  ) $white* @space+ "::" @line

@signature = (@signature_single_line | @signature_multiline)

-- | This is the catch-all line,
-- | since otherwise we'd have to special case on
-- | equality appearing in a different line,
-- | and guards.

@function_definition = (@name )+ @line



@fixity = ("infix " | "infixl " | "infixr ")  @line

@template_haskell = ("$(" | "[") @line




@nested = @space+ (~\n)*

@nested_header = @space+ (~\n)* (\n)*

@hidden_region_start = "{-!!!!!REGION HIDDEN" \n*
@hidden_region_end   = "REGION HIDDEN!!!!!-}"

tokens :-

<0> @hidden_region_start;
<0> @hidden_region_end;

<0> @module              { line Module `andBegin` hdr }

<0> @pragma_header       { line PragmaHeader    }
<0> @section             { line SectionHeader   }
<0> @class               { line Class           }
<0> @comment             { line Comment         }
<0> @data_family         { line DataFamily      }
<0> @data_instance       { line DataInstance    }
<0> @data                { line Data            }
<0> @default             { line Default         }
<0> @deriving            { line Deriving        }
<0> @fixity              { line Fixity          }
<0> @foreign             { line Foreign         }
<0> @instance            { line Instance        }
<0> @nested              { line Nested          }
<0> @newtype             { line Newtype         }
<0> @pattern             { line Pattern         }
<0> @pragma              { line Pragma          }
<0> @signature           { line Signature       }
<0> @template_haskell    { line TemplateHaskell }
<0> @type_family         { line TypeFamily      }
<0> @type_instance       { line TypeInstance    }
<0> @type                { line Type            }
<0> @function_definition { line Definition      }

<0> \n;

<hdr> @hidden_region_start;
<hdr> @hidden_region_end;

<hdr> @import              { line Import          }
<hdr> @nested_header       { line Nested          }

<hdr> @section             { line SectionHeader   `andBegin` 0 }
<hdr> @class               { line Class           `andBegin` 0 }
<hdr> @comment             { line Comment         `andBegin` 0 }
<hdr> @data_family         { line DataFamily      `andBegin` 0 }
<hdr> @data_instance       { line DataInstance    `andBegin` 0 }
<hdr> @data                { line Data            `andBegin` 0 }
<hdr> @default             { line Default         `andBegin` 0 }
<hdr> @deriving            { line Deriving        `andBegin` 0 }
<hdr> @fixity              { line Fixity          `andBegin` 0 }
<hdr> @foreign             { line Foreign         `andBegin` 0 }
<hdr> @instance            { line Instance        `andBegin` 0 }
<hdr> @newtype             { line Newtype         `andBegin` 0 }
<hdr> @pragma              { line Pragma          `andBegin` 0 }
<hdr> @signature           { line Signature       `andBegin` 0 }
<hdr> @template_haskell    { line TemplateHaskell `andBegin` 0 }
<hdr> @type_family         { line TypeFamily      `andBegin` 0 }
<hdr> @type_instance       { line TypeInstance    `andBegin` 0 }
<hdr> @type                { line Type            `andBegin` 0 }
<hdr> @function_definition { line Definition      `andBegin` 0 }

{
hiddenRegionStart :: ByteString
hiddenRegionStart = "{-!!!!!REGION HIDDEN\n"

hiddenRegionEnd :: ByteString
hiddenRegionEnd = "\nREGION HIDDEN!!!!!-}"

alexEOF :: Alex Line
alexEOF = do
  (AlexPn _ n _, _, _, _) <- alexGetInput
  pure $ Line EOF n ""

runAlexLines :: ByteString -> Alex a -> Either String a
runAlexLines = runAlex

line :: Type -> AlexAction Line
line f (AlexPn _ n _, _, str, _) len = pure $ Line f n (BS.take len str)

lexer :: BS.ByteString -> Either String [Line]
lexer file = runAlex file scanner
  where scanner :: Alex [Line]
        scanner = do
          token <- alexMonadScan
          case (token) of
            Line EOF _ _ -> pure $ [token]
            _      -> do rest <- scanner
                         pure $ token : rest
}
