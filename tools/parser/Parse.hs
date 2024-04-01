{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Parse
  ( Block (..)
  , Constructor (..)
  , Data (..)
  , Expression (..)
  , Function (..)
  , Header (..)
  , module Parse
  , RecField (..)
  , TypeSignature (..)
  , module Types
  , Unit (..)
  , Variable (..)
  , hiddenRegionEnd
  , hiddenRegionStart
  ) where

import Types

import Data.Function (on)
import Data.List     (groupBy, sortBy)

import Lex.Lines  (hiddenRegionEnd, hiddenRegionStart, runAlexLines)
import Lex.Tokens (runAlexTokens)

import Parse.Block (parseData, parseName, parseSignature)
import Parse.File  (File(File), Header(..), parseBlocks)

import Data.ByteString.Lazy qualified as BS

data BlockBins = BlockBins
  { classBlocks           :: [Block]
  , dataFamilyBlocks      :: [Block]
  , dataInstanceBlocks    :: [Block]
  , dataBlocks            :: [Block]
  , defaultBlocks         :: [Block]
  , definitionBlocks      :: [Block]
  , derivingBlocks        :: [Block]
  , fixityBlocks          :: [Block]
  , foreignBlocks         :: [Block]
  , instanceBlocks        :: [Block]
  , newtypeBlocks         :: [Block]
  , patternBlocks         :: [Block]
  , pragmaBlocks          :: [Block]
  , signatureBlocks       :: [Block]
  , templateHaskellBlocks :: [Block]
  , typeFamilyBlocks      :: [Block]
  , typeInstanceBlocks    :: [Block]
  , typeBlocks            :: [Block]
  }


class IdFormat a where
  identity :: a -> ByteString

instance IdFormat Block where
  identity block = BS.concat (map (<> "\n") block.comments) <> BS.intercalate "\n" block.contents

instance IdFormat Header where
  identity (Header _ bs) = bs


addToBins
  :: Block
  -> BlockBins
  -> BlockBins
addToBins b bins = case b of
  Block Class           _ _ _ -> bins { classBlocks           = b : bins.classBlocks           }
  Block DataFamily      _ _ _ -> bins { dataFamilyBlocks      = b : bins.dataFamilyBlocks      }
  Block DataInstance    _ _ _ -> bins { dataInstanceBlocks    = b : bins.dataInstanceBlocks    }
  Block Data            _ _ _ -> bins { dataBlocks            = b : bins.dataBlocks            }
  Block Default         _ _ _ -> bins { defaultBlocks         = b : bins.defaultBlocks         }
  Block Definition      _ _ _ -> bins { definitionBlocks      = b : bins.definitionBlocks      }
  Block Deriving        _ _ _ -> bins { derivingBlocks        = b : bins.derivingBlocks        }
  Block Fixity          _ _ _ -> bins { fixityBlocks          = b : bins.fixityBlocks          }
  Block Foreign         _ _ _ -> bins { foreignBlocks         = b : bins.foreignBlocks         }
  Block Instance        _ _ _ -> bins { instanceBlocks        = b : bins.instanceBlocks        }
  Block Newtype         _ _ _ -> bins { newtypeBlocks         = b : bins.newtypeBlocks         }
  Block Pattern         _ _ _ -> bins { patternBlocks         = b : bins.patternBlocks         }
  Block Pragma          _ _ _ -> bins { pragmaBlocks          = b : bins.pragmaBlocks          }
  Block Signature       _ _ _ -> bins { signatureBlocks       = b : bins.signatureBlocks       }
  Block TemplateHaskell _ _ _ -> bins { templateHaskellBlocks = b : bins.templateHaskellBlocks }
  Block TypeFamily      _ _ _ -> bins { typeFamilyBlocks      = b : bins.typeFamilyBlocks      }
  Block TypeInstance    _ _ _ -> bins { typeInstanceBlocks    = b : bins.typeInstanceBlocks    }
  Block Type            _ _ _ -> bins { typeBlocks            = b : bins.typeBlocks            }
  Block{}                     -> error $ "addToBins called on unexpected block " <> show b

binBlocks
  :: [Block]
  -> BlockBins
binBlocks []       = BlockBins [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] [] []
binBlocks (b : bs) = addToBins b $ binBlocks bs

compareBlocks
  :: Block
  -> Block
  -> Ordering
compareBlocks a b = on compare (getNameUnsafe . BS.intercalate "\n" . (.contents)) a b

formatBlockGroupWith
  :: (Block -> ByteString)
  -> [Block]
  -> ByteString
formatBlockGroupWith g blocks = BS.intercalate "\n" $ map g blocks

formatDataWithUnsafe
  :: ByteString
  -> (Data -> ByteString)
  -> ByteString
formatDataWithUnsafe str f = case ( runAlexTokens str parseData ) of
  Left  e -> error e
  Right d -> f d

formatDataWith
  :: ByteString
  -> (Data -> ByteString)
  -> ByteString
formatDataWith str f = case ( runAlexTokens str parseData ) of
  Left  _ -> str
  Right d -> f d

formatSectionWith
  :: (Block -> ByteString)
  -> Section
  -> ByteString
formatSectionWith g (Section header blocks) =
  if (BS.null header) then
    sortBlocks g blocks
  else
    header <> "\n\n" <> (sortBlocks g blocks)

formatSigWithUnsafe
  :: ByteString
  -> (TypeSignature -> ByteString)
  -> ByteString
formatSigWithUnsafe str f = case ( runAlexTokens str parseSignature ) of
  Left  e -> error e
  Right d -> f d

formatSigWith
  :: ByteString
  -> (TypeSignature -> ByteString)
  -> ByteString
formatSigWith str f = case ( runAlexTokens str parseSignature ) of
  Left  _ -> str
  Right d -> f d

formatWith
  :: ByteString
  -> (Header -> ByteString)
  -> (Block -> ByteString)
  -> ByteString
formatWith str f g = case (formatFile f g <$> runAlexLines str parseBlocks) of
  Left  _ -> str
  Right d -> d
  where formatFile f g (File header sections) = f header <> (BS.intercalate "\n\n\n\n" $ map (formatSectionWith g) sections)

formatWithUnsafe
  :: ByteString
  -> (Header -> ByteString)
  -> (Block -> ByteString)
  -> ByteString
formatWithUnsafe str f g = throwFailure $ formatFile f g <$> runAlexLines str parseBlocks
  where formatFile f g (File header sections) = f header <> (BS.intercalate "\n\n\n\n" $ map (formatSectionWith g) sections)

getNameUnsafe
  :: ByteString
  -> ByteString
getNameUnsafe block = throwFailure $ runAlexTokens block parseName

groupByNameUnsafe
  :: [Block]
  -> [[Block]]
groupByNameUnsafe blocks = groupBy (on (==) $ getNameUnsafe . BS.intercalate "\n" . (.contents)) blocks

processWithUnsafe
  :: ByteString
  -> (Header -> ByteString)
  -> (Block -> ByteString)
  -> ByteString
processWithUnsafe str f g = throwFailure $ processFile f g <$> runAlexLines str parseBlocks
  where processFile f g (File header sections) = f header <> (BS.intercalate "\n" $ map (BS.intercalate "\n\n" . map g . (.blocks)) sections)

processWith
  :: ByteString
  -> (Header -> ByteString)
  -> (Block -> ByteString)
  -> ByteString
processWith str f g = case (processFile f g <$> runAlexLines str parseBlocks) of
  Left  _ -> str
  Right d -> d
  where processFile f g (File header sections) = f header <> (BS.intercalate "\n" $ map (BS.intercalate "\n\n" . map g . (.blocks)) sections)



sortBlocks
  :: (Block -> ByteString)
  -> [Block]
  -> ByteString
sortBlocks g bs = BS.intercalate "\n\n\n" $ filter (not . BS.null)
    [ defaults
    , foreignImports
    , derivingInstances
    , datatypes
    , dataFamilies
    , newtypes
    , types
    , typeFamilies
    , typeclasses
    , functions
    , templateHaskell
    ]
  where
    BlockBins{..} = binBlocks bs
    dataFamilyBlocks' = groupByNameUnsafe $ sortBy compareBlocks $  dataFamilyBlocks
                                                           <> dataInstanceBlocks
    typeclassBlocks   = groupByNameUnsafe $ sortBy compareBlocks $  classBlocks
                                                           <> instanceBlocks
    typeFamilyBlocks' = groupByNameUnsafe $ sortBy compareBlocks $  typeFamilyBlocks
                                                           <> typeInstanceBlocks
    functionBlocks    = groupByNameUnsafe $ sortBy compareBlocks $  pragmaBlocks
                                                           <> signatureBlocks
                                                           <> definitionBlocks
                                                           <> patternBlocks
                                                           <> fixityBlocks
    derivingBlocks'   = groupByNameUnsafe $ sortBy compareBlocks derivingBlocks
    derivingInstances = BS.intercalate "\n\n" $ map (BS.intercalate "\n"   . map g) $ derivingBlocks'
    functions         = BS.intercalate "\n\n" $ map (BS.intercalate "\n"   . map g) $ functionBlocks
    typeFamilies      = BS.intercalate "\n\n" $ map (BS.intercalate "\n"   . map g) $ typeFamilyBlocks'
    dataFamilies      = BS.intercalate "\n\n" $ map (BS.intercalate "\n\n" . map g) $ dataFamilyBlocks'
    typeclasses       = BS.intercalate "\n\n" $ map (BS.intercalate "\n\n" . map g) $ typeclassBlocks
    datatypes         = BS.intercalate "\n\n" $ map g $ sortBy compareBlocks dataBlocks
    defaults          = BS.intercalate "\n\n" $ map g $ sortBy compareBlocks defaultBlocks
    foreignImports    = BS.intercalate "\n\n" $ map g $ sortBy compareBlocks foreignBlocks
    newtypes          = BS.intercalate "\n\n" $ map g $ sortBy compareBlocks newtypeBlocks
    types             = BS.intercalate "\n\n" $ map g $ sortBy compareBlocks typeBlocks
    templateHaskell   = BS.intercalate "\n\n" $ map g $ templateHaskellBlocks

throwFailure
  :: Either String a
  -> a
throwFailure (Left  e) = error e
throwFailure (Right a) = a
