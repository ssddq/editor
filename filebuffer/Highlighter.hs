{-# LANGUAGE NoFieldSelectors    #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Highlighter where

import Common

import Data.ByteString qualified as Strict
import Data.ByteString.Internal qualified as Strict

import Data.Attoparsec.ByteString     hiding (Parser)
import Data.Attoparsec.Internal.Types (Parser)

import Streaming          qualified as S
import Streaming.Internal qualified as S

data ByteStringColored = ByteStringColored
  { string :: {-# UNPACK #-} !Strict.ByteString
  , color  :: {-# UNPACK #-} !Color
  }

data Visibility = Rendered | Invisible


type ByteStream = S.Stream (S.Of Strict.ByteString) IO ()

type ByteStreamColored = S.Stream (S.Of ByteStringColored) IO ()

type ParserCont = Strict.ByteString -> IResult Strict.ByteString ByteStringColored

type Highlighter = Parser Strict.ByteString ByteStringColored

{-# INLINE defaultColor #-}
defaultColor
  :: Strict.ByteString
  -> ByteStringColored
defaultColor string = ByteStringColored string white

{-# INLINE highlightStream #-}
highlightStream
  :: Highlighter
  -> ByteStream
  -> ByteStreamColored
highlightStream parser = loop
  where
    loop :: ByteStream -> ByteStreamColored
    loop !stream = case (stream) of
      S.Effect m -> S.Effect (fmap loop m)
      S.Return r -> S.Return r
      S.Step (bs S.:> rest) -> case (parse parser bs) of
        Fail _ _ _   -> S.Step (defaultColor bs S.:> loop rest)
        Partial cont -> loopCont cont rest
        Done i r     -> S.Step (r S.:> loop (S.Step $ i S.:> rest))
    loopCont :: ParserCont -> ByteStream -> ByteStreamColored
    loopCont !cont !stream = case (stream) of
      S.Effect m -> S.Effect (fmap (loopCont cont) m)
      S.Return r -> S.Return r
      S.Step (bs S.:> rest) -> case (cont bs) of
        Fail _ _ _    -> S.Step (defaultColor bs S.:> loop rest)
        Partial cont' -> loopCont cont' rest
        Done i r      -> S.Step (r S.:> loop (S.Step $ i S.:> rest))

{-# INLINE white #-}
white :: Color
white = Color maxBound maxBound maxBound maxBound

-- * Haskell highlighter

{-# INLINE hsHighlight #-}
hsHighlight :: Highlighter
hsHighlight = do
  choice [ parentheses
         , keyword
         , identifier
         , highlightFallback
--         , punctuation
--         , topLevelName
--         , name
         ]

{-# INLINE identifier #-}
identifier :: Highlighter
identifier = do
  name <- takeWhile1 $
    \w -> (48 <= w && w <= 57)
       || (65 <= w && w <= 90)
       || (97 <= w && w <= 122)
  return $ defaultColor name

{-# INLINE topLevelIdentifier #-}
topLevelIdentifier :: Highlighter
topLevelIdentifier = do
  name <- takeWhile1 $
    \w -> (48 <= w && w <= 57)
       || (65 <= w && w <= 90)
       || (97 <= w && w <= 122)
  return $ topLevelIdentifierColor name

{-# INLINE highlightFallback #-}
highlightFallback :: Highlighter
highlightFallback = do
  char <- anyWord8
  return $ defaultColor (Strict.singleton char)

{-# INLINE parentheses #-}
parentheses :: Highlighter
parentheses = do
  paren <- satisfy $
    \w -> w == 40  || w == 41  -- '(' || ')'
       || w == 91  || w == 93  -- '[' || ']'
       || w == 123 || w == 125 -- '{' || '}'
  return $ ByteStringColored (Strict.singleton paren) parenthesesColor

{-# INLINE keyword #-}
keyword :: Highlighter
keyword = do
  key <- choice [ string "where"
                , string "let"
                , string "in"
                , string "case"
                , string "of"
                , string "module"
                , string "import"
                , string "class"
                , string "data"
                , string "type"
                , string "instance"
                , string "do"
                ]
  space <- takeWhile1 $ Strict.isSpaceWord8
  return $ ByteStringColored (key <> space) keywordColor

-- * Colors

parenthesesColor :: Color
parenthesesColor = Color 255 0 255 255

keywordColor :: Color
keywordColor = Color 255 0 0 255

topLevelIdentifierColor :: Strict.ByteString -> ByteStringColored
topLevelIdentifierColor bs = ByteStringColored bs color
  where color = Color 0 255 0 255
