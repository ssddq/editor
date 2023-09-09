{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

module Format.DataDeclaration where

import Common ((|-))
import Parse  (ByteString, Constructor(..), Data(..), Expression, Function, RecField(..))

import Format.TypeSignature (formatTyExp, formatTyFun)

import Data.Int (Int64)

import Data.ByteString.Lazy       qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BSC

commentsMulti
  :: Constructor
  -> ByteString
commentsMulti c = BS.concat $ map ("\n  " <>) c.comments

commentsSingle
  :: Constructor
  -> ByteString
commentsSingle c = BS.concat $ map (<> "\n") c.comments

dataDecType
  :: Data
  -> ByteString
dataDecType d = case d of
  DataDec{} -> "data "
  InstDec{} -> "data instance "
  NtypDec{} -> "newtype "

formatDataDec
  :: Data
  -> ByteString
formatDataDec (padConstructors -> dec) =
    initialComments
    <> dataDecType dec
    <> formatTyFun dec.head
    <> body
    <> formatDeriving dec.derived
  where
    body = BS.concat $ zipWith (<>) comments constructors
    comments = (firstConComments :) $ drop 1 $ map (BS.concat . map ("\n  " <>) . (.comments)) dec.constructors
    constructors = zipWith (<>)
                     Common.|- [eq] <> cycle ["\n  | "]
                     Common.|- map (BS.intercalate padding . formatConstructor) dec.constructors
    (eq, initialComments, firstConComments, formatConstructor) = case (dec.constructors) of
      [ ]     -> ( ""      , ""              , ""             , formatMulti )
      [c]     -> ( " = "   , commentsSingle c, ""             , formatSingle )
    --  [c]     -> ( " = "   , commentsSingle c, ""             , formatMulti )
      (c : _) -> ( "\n  = ", ""              , commentsMulti c, formatMulti )
    len = case (dec.constructors) of
            [Con{}] -> 1 + (BS.length $ dataDecType dec <> formatTyFun dec.head <> eq)
            [Rec{}] -> 2
            _       -> 5 -- i.e. BS.length $ "\n  = "
    padding = "\n" <> BSC.replicate len ' '

formatDeriving
  :: Maybe Expression
  -> ByteString
formatDeriving (Just exp) = "\n  deriving " <> formatTyExp exp
formatDeriving Nothing    = ""

formatField
  :: RecField
  -> ByteString
formatField field =
    if (BS.null field.name) then
      field.pragma <> field.bang <> formatTyFun field.field_type <> field.commments
    else
      field.name <> " :: " <> field.pragma <> field.bang <> formatTyFun field.field_type <> field.commments

formatMulti
  :: Constructor
  -> [ByteString]
formatMulti con = case con of
    Con{ fields = [] }     -> [ con.name ]
    Rec{ fields = [] }     -> [ con.name ]
    Con{ fields = f : fs } -> [ con.name <> " "   <> formatField f ] <> map ((padding <>) . formatField) fs
    Rec{ fields = f : fs } -> [ con.name <> " { " <> formatField f ] <> map ((comma   <>) . formatField) fs <> [ padding <> "}" ]
  where
    len     = BS.length con.name
    comma   = padding <> ", "
    padding = BSC.replicate len ' '

formatSingle
  :: Constructor
  -> [ByteString]
formatSingle con = case con of
    Con{ fields = [] }     -> [ con.name ]
    Rec{ fields = [] }     -> [ con.name ]
    Con{ fields = f : fs } -> [ con.name   <> " "    <> formatField f ] <> map ((padding  <>) . formatField) fs
    Rec{ fields = f : fs } -> [ con.name ] <> [ "{ " <> formatField f ] <> map ((", " <>)     . formatField) fs <> [ "}" ]
  where
    padding = BSC.replicate (BS.length con.name) ' '

hasStrictField
  :: Constructor
  -> Bool
hasStrictField con = or $ map ((== "!") . (.bang)) con.fields

hasUnpackedField
  :: Constructor
  -> Bool
hasUnpackedField con = or $ map (not . BS.null . (.pragma)) con.fields

maxCommentLength
  :: [RecField]
  -> Int64
maxCommentLength fields = maximum $ 0 : map recFunLen fields

maxRecordLength
  :: [RecField]
  -> Int64
maxRecordLength []                      = 0
maxRecordLength (RecField{name} : recs) = max (BS.length name) $ maxRecordLength recs

padBangs
  :: Bool
  -> Constructor
  -> Constructor
padBangs False con = con
padBangs True  con = con { fields = map (\r -> r { bang = padStrTo 1 r.bang }) con.fields }

padCmtWithFun
  :: Int64
  -> Function
  -> ByteString
  -> ByteString
padCmtWithFun n fun cmt = padding <> cmt
  where padding = BSC.replicate (n - (BS.length $ formatTyFun fun)) ' '

-- | Record update is ambiguous without an explicit match here, as of GHC 9.6.2.
-- | At some point (future GHC) this should be writeable using a type annotation,
-- | rather than having to match on the constructors.

padConstructorTo
  :: Int64
  -> Constructor
  -> Constructor
padConstructorTo n con = case con of
  Con{..} -> Con{ name = padStrTo n con.name, ..}
  Rec{..} -> Rec{ name = padStrTo n con.name, ..}

padConstructors
  :: Data
  -> Data
padConstructors dec = dec { constructors = map pad dec.constructors }
  where pad = padConstructorTo   l
            . padFieldCommentsTo n
            . padFieldNamesTo    m
            -- . padUNPACK hasUNPACK
            -- . padBangs  hasStrict
        l =     maximum (0 : map (BS.length        . (.name  )) dec.constructors)
        m =     maximum (0 : map (maxRecordLength  . (.fields)) dec.constructors)
        n = 2 + maximum (0 : map (maxCommentLength . (.fields)) dec.constructors)
        -- hasUNPACK = or $ map hasUnpackedField dec.constructors
        -- hasStrict = or $ map hasStrictField   dec.constructors

padFieldCommentsTo
  :: Int64
  -> Constructor
  -> Constructor
padFieldCommentsTo n con = con { fields = map (padRecFieldCmtTo n) con.fields }

padFieldNamesTo
  :: Int64
  -> Constructor
  -> Constructor
padFieldNamesTo _ con@Con{} = con
padFieldNamesTo n rec@Rec{} = rec { fields = map (\RecField{..} -> RecField{ name = padStrTo n name, ..}) rec.fields }

padRecFieldCmtTo
  :: Int64
  -> RecField
  -> RecField
padRecFieldCmtTo n (RecField  name fun pragma bang comms) = RecField  name fun pragma bang (padCmtWithFun n fun comms)

padStrTo
  :: Int64
  -> BS.ByteString
  -> BS.ByteString
padStrTo n str = str <> padding
  where padding = BSC.replicate (n - (BS.length str)) ' '

padUNPACK
  :: Bool
  -> Constructor
  -> Constructor
padUNPACK False con       = con
padUNPACK True  rec@Rec{} = rec { fields = map (\r -> r { pragma = padStrTo 15 r.pragma } ) rec.fields }
padUNPACK True  con@Con{} = case (hasUnpackedField con) of
  False -> con
  True  -> con { fields = map (\r -> r { pragma = padStrTo 15 r.pragma } ) con.fields }

recFunLen
  :: RecField
  -> Int64
recFunLen (RecField  _ fun _ _ _) = BS.length $ formatTyFun fun
