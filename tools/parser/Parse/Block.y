{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}
module Parse.Block
  ( TypeSignature (..)
  , Function (..)
  , Expression (..)
  , Unit (..)
  , Data (..)
  , Constructor (..)
  , RecField (..)
  , Variable (..)
  , parseSignature
  , parseData
  , parseName
  ) where

import Types hiding (Type (..))

import Lex.Tokens

import Parse.File (Block)

import Data.ByteString.Lazy (ByteString)

import Data.ByteString.Lazy qualified as BS

}

%name    parseSignature  type_signature
%name    parseData       data_declaration
%partial parseName       parse_name

%tokentype { Token }
%error     { parseError }

%lexer     { lexer } { EOL }

%monad     { Alex } { >>= } { pure }

%token

  comment { Comment $$ }

  -- Keywords
  class        { Class        }
  default      { Default      }
  instance     { Instance     }
  typeFamily   { TypeFamily   }
  typeInstance { TypeInstance }
  import       { Import       }
  type         { Type         }
  forall       { Forall       }
  newtype      { Newtype      }
  data         { Data         }
  dataFamily   { DataFamily   }
  dataInstance { DataInstance }
  deriving     { Deriving     }
  unpack       { Unpack       }
  foreign      { Foreign      }
  pattern      { Pattern      }

  -- Identifiers
  name    { Name $$ }

  -- Key separators
  '::'    { DoubleColon     }
  '.'     { Period          }
  '=>'    { ConstraintArrow }
  '~'     { Tilde           }

  -- Symbols
  '*'     { Star }
  '@'     { At   }
  '!'     { Bang }

  -- Separators
  '='     { Equal }
  ','     { Comma }
  '->'    { Arrow }
  '<-'    { Bind  }
  '|'     { Guard }

  -- Delimiters
  '(#'    { OpenUnboxedParen  }
  '#)'    { CloseUnboxedParen }
  '('     { OpenParen    }
  ')'     { CloseParen   }
  '['     { OpenBracket  }
  ']'     { CloseBracket }
  '{'     { OpenBrace    }
  '}'     { CloseBrace   }
  '{-#'   { PragmaStart  }
  '#-}'   { PragmaStop   }

%expect 0

%%

parse_name
  : class             function { getName $2 }
  | newtype           function { getName $2 }
  | data              function { getName $2 }
  | instance          function { getName $2 }
  | deriving instance function { getName $3 }
  | instance pragma   function { getName $3 }
  | dataFamily    unit { getNameUnit $2 }
  | type          unit { getNameUnit $2 }
  | typeFamily    unit { getNameUnit $2 }
  | typeInstance  unit { getNameUnit $2 }
  | pattern             name { $2 }
  | data_instance_name       { $1 }
  | foreign import name name { $4 }
  | name { $1 }
  | '{-#' name  { $2 }

pragma
  : '{-#' names '#-}' {}

names
  : {}
  | names name {}

data_instance_name
  : dataInstance name { $2 }
  | data_instance_name name { $1 <> $2 }

phase
  : atom {}
  | '[' atom ']' {}
  | '[' '~' atom ']' {}


data_declaration
  : data         data_definition { $2 DataDec }
  | dataInstance data_definition { $2 InstDec }
  | newtype      data_definition { $2 NtypDec }

data_definition
  : function_no_comment                                                       { \f -> f $1 []                     Nothing }
  | function_no_comment comment_list '=' constructor_list                     { \f -> f $1 (withComments $4 $2)   Nothing }
  | function_no_comment comment_list '=' constructor_list deriving expression { \f -> f $1 (withComments $4 $2) $ Just $6 }

comment_list
  : %shift { [] }
  | comment_list comment { $1 <> [$2] }

constructor_list
  : constructor                                   { [$1] }
  | constructor_list              '|' constructor { $1 <> [$3] }
  | constructor_list comment_list '|' constructor { $1 <> [mergeCmts $4 $2] }

constructor
  : name     fields      { Con $1 $2 [] }
  | name '{' records '}' { Rec $1 $3 [] }

record
  : name '::' unpack_optional strictness function_no_comment comment_optional { RecField $1 $5 $3 $4 $6 }

records
  : record { [$1] }
  | records ',' record { $1 <> [$3] }

field
  : unpack_optional strictness expression comment_optional { RecField "" (Expression $3 "" ) $1 $2 $4 }

fields
  : { [] }
  | fields field { $1 <> [$2] }


unpack_optional
  : { "" }
  | unpack { "{-# UNPACK #-} " }

strictness
  : { "" }
  | '!' { "!" }

comment_optional
  : %shift { "" }
  | comment { $1 }

type_signature
  : name '::' function { TypeSignature $1 $3 }

function_no_comment
  : expression                          { Expression     $1    "" }
  | expression '->' function_no_comment { Function       $1 $3 "" }
  | expression '=>' function_no_comment { Constraint     $1 $3 "" }
  | quantification  function_no_comment { Quantification $1 $2    }

function
  : expression comment_optional               { Expression     $1    $2 }
  | expression comment_optional '->' function { Function       $1 $4 $2 }
  | expression comment_optional '=>' function { Constraint     $1 $4 $2 }
  | quantification                   function { Quantification $1 $2    }

quantification
  : forall quantified_variables '.' { $2 }

quantified_variables
  : { [] }
  | quantified_variables name                         { $1 <> [Var $2   ] }
  | quantified_variables '(' name '::' expression ')' { $1 <> [Knd $3 $5] }

expression
  : unit            { Unit $1    }
  | expression unit { Appl $1 $2 }
  | expression '~' expression %shift{ TyEq $1 $3 }

unit
  : atom                       { Atm $1    }
  | '(' function ')'           { Par $2    }
  | '[' function ']'           { Lst $2    }
  | '(' function_list ')'      { Tpl $2    }
  | '(#' function_list '#)'    { Utp $2    }
  | name '{' tlr_equations '}' { Tpr $1 $3 }

atom
  : name           { $1     }
  | '(' ')'        { "()"   }
  | '[' ']'        { "[]"   }
  | '(' '->' ')'   { "(->)" }
  | '(' commas ')' { "(" <> BS.replicate $2 (fromIntegral $ fromEnum ',') <> ")" }

commas
  : ','        { 1      }
  | commas ',' { 1 + $1 }

function_list
  : function                   %shift { [$1] }
  | function_list ',' function        { $1 <> [$3] }


tlr_equations
  : name '=' expression                   {       [($1, $3)] }
  | tlr_equations ',' name '=' expression { $1 <> [($3, $5)] }

{
parseError :: Token -> Alex a
parseError t = do
  (AlexPn _ line column, _, _, _) <- alexGetInput
  alexError $ "Parse error at line " <> show line <> ", column " <> show column <> "\nLast symbol parsed: " <> show t

lexer :: (Token -> Alex a) -> Alex a
lexer = \f -> alexMonadScan >>= f

getName :: Function -> ByteString
getName (Expression e     _) = getNameExp e
getName (Function   e _   _) = getNameExp e
getName (Constraint _   f _) = getName f
getName (Quantification _ f) = getName f

getNameExp :: Expression -> ByteString
getNameExp (Unit u  ) = getNameUnit u
getNameExp (Appl e u) = getNameExp  e

getNameUnit :: Unit -> ByteString
getNameUnit (Atm a  ) = a
getNameUnit (Par f  ) = getName f
getNameUnit (Lst _  ) = error "unexpected list"
getNameUnit (Tpl _  ) = error "unexpected tuple"
getNameUnit (Tpr _ _) = error "unexpected tlr"


withComments :: [Constructor] -> [ByteString] -> [Constructor]
withComments [] cmts = []
withComments (con : cons) cmts = (mergeCmts con cmts) : cons

mergeCmts :: Constructor -> [ByteString] -> Constructor
mergeCmts (Con name tys cmt) cmts = Con name tys $ cmt <> cmts
mergeCmts (Rec name tys cmt) cmts = Rec name tys $ cmt <> cmts
}
