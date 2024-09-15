{-# LANGUAGE TemplateHaskell #-}

module Haskell where

import Common

import Data.Char (isSymbol, isUpperCase)

import FlatParse.Basic


data ParserState
  = DefaultState
  | ImportLine


{-# INLINE initialState #-}
initialState :: ParserState
initialState = DefaultState



-- * Parser

{-# INLINE colorCursor #-}
colorCursor
  :: Mode
  -> Color
colorCursor Normal = Color 235 203 139 255
colorCursor Insert = Color 191 97 106 255

{-# INLINE hs #-}
hs
  :: ParserState
  -> Parser e (Color, ParserState)
hs DefaultState =
      (import_            *> pure (keywordColor           , ImportLine  ))
  <|> (keyword            *> pure (keywordColor           , DefaultState))
  <|> (comment            *> pure (commentColor           , DefaultState))
  <|> (moduleQualifier    *> pure (commentColor           , DefaultState))
  <|> (parentheses        *> pure (parenthesesColor       , DefaultState))
  <|> (typeIdentifier     *> pure (typeIdentifierColor    , DefaultState))
  <|> (stringLiteral      *> pure (stringLiteralColor     , DefaultState))
  <|> (topLevelIdentifier *> pure (topLevelIdentifierColor, DefaultState))
  <|> (guard              *> pure (commentColor           , DefaultState))
  <|> (keySymbol          *> pure (keySymbolColor         , DefaultState))
  <|> (identifier         *> pure (identifierColor        , DefaultState))
  <|> (operator           *> pure (operatorColor          , DefaultState))
  <|> (fallback           *> pure (fallbackColor          , DefaultState))
hs ImportLine =
      (parentheses        *> pure (parenthesesColor       , ImportLine  ))
  <|> ($(char '\n')       *> pure (fallbackColor          , DefaultState))
  <|> (importKeyword      *> pure (commentColor           , ImportLine  ))
  <|> (identifier         *> pure (identifierColor        , ImportLine  ))
  <|> (fallback           *> pure (fallbackColor          , ImportLine  ))

{-# INLINE hsParser #-}
hsParser :: FileParser ParserState e
hsParser = FileParser
  { parser           = hs
  , defaultColor     = fallbackColor
  , defaultState     = DefaultState
  , cursor           = colorCursor
  , requestedContext = 512
  }



-- * Subparsers

{-# INLINE comment #-}
comment :: Parser e ()
comment = (inlineComment <|> multilineComment)

-- | We must require at least 7 characters before we fallback to not coloring the string,
-- | since we may have otherwise hit an initial chunk of a keyword sitting at a boundary.

{-# INLINE fallback #-}
fallback :: Parser e Char
fallback = ensure 7 *> anyChar

{-# INLINE guard #-}
guard :: Parser e ()
guard = $(char '|')
     *> space

{-# INLINE identifier  #-}
identifier :: Parser e [Char]
identifier = some (satisfy (\c -> isDigit c || isLatinLetter c || c == '_'))

{-# INLINE importKeyword #-}
importKeyword :: Parser e ()
importKeyword = space
             *> keywords
             *> lookahead space
  where keywords = $(switch
          [| case _ of
               "qualified" -> pure ()
               "as"        -> pure ()
               "hiding"    -> pure ()
          |] )

{-# INLINE import_ #-}
import_ :: Parser e ()
import_ = many $(char '\n') *> $(string "import")

{-# INLINE inlineComment #-}
inlineComment :: Parser e ()
inlineComment = commentStart
             *> some (satisfy (/= '\n'))
             *> pure ()
  where commentStart = notFollowedBy
                         |- $(string "--")
                         |- symbol

{-# INLINE keySymbol #-}
keySymbol :: Parser e ()
keySymbol = notFollowedBy key symbol
  where key = $(switch
          [| case _ of
               "="  -> pure ()
               "::" -> pure ()
          |] )

-- | Subjectively, I don't like coloring "case .. of"
-- | or "do"

{-# INLINE keyword #-}
keyword :: Parser e ()
keyword = many $(char '\n')
       *> keywords
       *> (space <|> $(char '\n'))
  where keywords = $(switch
          [| case _ of
              "where"    -> pure ()
              "let"      -> pure ()
              "in"       -> pure ()
  --            "case"     -> pure ()
  --            "of"       -> pure ()
              "module"   -> pure ()
              "import"   -> pure ()
              "class"    -> pure ()
              "data"     -> pure ()
              "type"     -> pure ()
              "instance" -> pure ()
  --          "do"       -> pure ()
          |] )

{-# INLINE moduleQualifier #-}
moduleQualifier :: Parser e ()
moduleQualifier = typeIdentifier
               *> $(char '.')

{-# INLINE multilineComment #-}
multilineComment :: Parser e ()
multilineComment = commentStart
                *> some anyUntilEnd
                *> commentEnd
  where commentStart = $(string "{-")
        commentEnd = $(string "-}")
        anyUntilEnd = satisfy (/= '-')
                  <|> notFollowedBy anyChar $(char '}')

-- | Subjectively, I only like coloring some of the single character operators

{-# INLINE operator #-}
operator :: Parser e ()
operator = (symbol *> some symbol *> pure ()) <|> notFollowedBy opChar symbol
  where opChar = $(switch
          [| case _ of
               ">" -> pure ()
               "<" -> pure ()
               "$" -> pure ()
          |] )

{-# INLINE parentheses #-}
parentheses :: Parser e ()
parentheses = $(switch
  [| case _ of
       "(" -> pure ()
       ")" -> pure ()
       "[" -> pure ()
       "]" -> pure ()
       "{" -> pure ()
       "}" -> pure ()
  |] )

space :: Parser e ()
space = $(switch
  [| case _ of
       " "  -> pure ()
       "\t" -> pure ()
  |] )

{-# INLINE stringLiteral #-}
stringLiteral :: Parser e ()
stringLiteral = $(char '\"')
             *> stringContent
             *> $(char '\"')
  where stringContent = many $ $(string "\\\"") <|> (satisfy (/= '\"') *> pure ())

{-# INLINE symbol #-}
symbol :: Parser e ()
symbol = asciiSymbol <|> (satisfy isSymbol *> pure ())
  where asciiSymbol = $(switch
          [| case _ of
               "!"  -> pure ()
               "#"  -> pure ()
               "$"  -> pure ()
               "%"  -> pure ()
               "&"  -> pure ()
               "⋆"  -> pure ()
               "+"  -> pure ()
               "."  -> pure ()
               "/"  -> pure ()
               "<"  -> pure ()
               "="  -> pure ()
               ">"  -> pure ()
               "?"  -> pure ()
               "@"  -> pure ()
               "^"  -> pure ()
               "|"  -> pure ()
               "-"  -> pure ()
               "~"  -> pure ()
               ":"  -> pure ()
               "\\" -> pure ()
          |] )

{-# INLINE topLevelIdentifier #-}
topLevelIdentifier :: Parser e [[Char]]
topLevelIdentifier = $(char '\n') *> many identifier

{-# INLINE typeIdentifier #-}
typeIdentifier :: Parser e [[Char]]
typeIdentifier = satisfy isUpperCase *> many identifier



-- * Colors

{-# INLINE commentColor #-}
commentColor :: Color
commentColor = Color 96 111 132 255

{-# INLINE fallbackColor #-}
fallbackColor :: Color
fallbackColor = Color 236 239 244 255

{-# INLINE identifierColor #-}
identifierColor :: Color
identifierColor = Color 236 239 244 255

{-# INLINE keySymbolColor #-}
keySymbolColor :: Color
keySymbolColor = Color 235 203 139 255

{-# INLINE keywordColor #-}
keywordColor :: Color
keywordColor = Color 191 97 106 255

{-# INLINE operatorColor #-}
operatorColor :: Color
operatorColor = Color 191 97 106 255

{-# INLINE parenthesesColor #-}
parenthesesColor :: Color
parenthesesColor = Color 180 142 173 255

{-# INLINE stringLiteralColor #-}
stringLiteralColor :: Color
stringLiteralColor = Color 191 197 210 255

{-# INLINE topLevelIdentifierColor #-}
topLevelIdentifierColor :: Color
topLevelIdentifierColor = Color 129 161 193 255

{-# INLINE typeIdentifierColor #-}
typeIdentifierColor :: Color
typeIdentifierColor = Color 143 188 187 255
