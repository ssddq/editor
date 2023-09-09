{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Types
  ( ByteString
  , module Types
  ) where

import Data.ByteString.Lazy (ByteString)

data Block = Block
  { block_type :: Type
  , position   :: Int
  , comments   :: [ByteString]
  , contents   :: [ByteString]
  }
  deriving (Show)

data Constructor
  = Con { name     :: ByteString
        , fields   :: [RecField]
        , comments :: [ByteString]
        }
  | Rec { name     :: ByteString
        , fields   :: [RecField]
        , comments :: [ByteString]
        }
  deriving (Show)

-- | Note the function type in the first argument is generous,
-- | but necessary to avoid ambiguities in the grammar
-- | since data declarations can have contexts,
-- | which (I think) cannot be parsed properly in this pass
-- | without significant lookahead, or more complicated tokenization.
-- |
-- | The fundamental problem is that both
-- | (data Monad m => Foo m) AND (data Foo m)
-- | should be accepted, but Happy cannot perform extended look-ahead to know
-- | whether a constraint symbol '=>' appears, and so it has to blindly decide
-- | whether to parse the first symbol Monad/Foo as part of a context or
-- | the type name.

data Data
  = DataDec { head         :: Function
            , constructors :: [Constructor]
            , derived      :: (Maybe Expression)
            }
  | InstDec { head         :: Function
            , constructors :: [Constructor]
            , derived      :: (Maybe Expression)
            }
  | NtypDec { head         :: Function
            , constructors :: [Constructor]
            , derived      :: (Maybe Expression)
            }
  deriving (Show)

data Expression
  = Unit Unit
  | Appl Expression Unit
  | TyEq Expression Expression
  deriving (Show)

data Function
  = Expression     Expression ByteString
  | Function       Expression Function ByteString
  | Constraint     Expression Function ByteString
  | Quantification [Variable] Function
  deriving (Show)

data Line = Line
  { line_type :: Type
  , number    :: Int
  , text      :: ByteString
  }
  deriving (Show)

data RecField = RecField
  { name       :: ByteString
  , field_type :: Function
  , pragma     :: ByteString
  , bang       :: ByteString
  , commments  :: ByteString
  }
  deriving (Show)

data Section = Section
  { header :: ByteString
  , blocks :: [Block]
  }
  deriving (Show)

data Type
  = Class
  | Comment
  | DataFamily
  | DataInstance
  | Data
  | Default
  | Definition
  | Deriving
  | Fixity
  | Foreign
  | Import
  | Instance
  | Module
  | Nested
  | NestedHeader
  | Newtype
  | Pattern
  | PragmaHeader
  | Pragma
  | SectionHeader
  | Signature
  | TemplateHaskell
  | TypeFamily
  | TypeInstance
  | Type
  | EOF
  deriving (Show)

data TypeSignature = TypeSignature ByteString Function
  deriving (Show)

data Unit
  = Atm ByteString
  | Par Function
  | Lst Function
  | Utp [Function]
  | Tpl [Function]
  | Tpr ByteString [(ByteString, Expression)]
  deriving (Show)

data Variable
  = Var { name :: ByteString
        }
  | Knd { name :: ByteString
        , kind :: Expression
        }
  deriving (Show)
