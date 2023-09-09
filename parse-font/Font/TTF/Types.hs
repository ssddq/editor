{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Font.TTF.Types where

import Font.Generics

import Prelude hiding (head)

import Data.Bits
import Data.Int
import Data.Serialize.Get
import Data.Word

import GHC.TypeLits

import Numeric.Fixed

import Data.ByteString qualified as Strict
import Data.Vector     qualified as V

--ignoring the "name" table since we won't use it

data Font = Font
  { cmap :: Table "cmap"
  , glyf :: Table "glyf"
  , head :: Table "head"
  , hhea :: Table "hhea"
  , hmtx :: Table "hmtx"
  , loca :: Table "loca"
  , maxp :: Table "maxp"
  , post :: Table "post"
  }
  deriving (Show)

data FontDirectory = FontDirectory
  { numTables      :: {-# UNPACK #-} !Word16
  , tableDirectory :: V.Vector TablePosition
  }
  deriving (Show)

-- functions in Parse probably need strictness annotations to make all the unpacking worth it
-- unlikely for strictness to be worth it for e.g. vectors. I believe the copy cost of the vector from indexing would nullify the gain.

data RawFont = RawFont
  { cmap_r :: (TablePosition, Strict.ByteString)
  , glyf_r :: (TablePosition, Strict.ByteString)
  , head_r :: (TablePosition, Strict.ByteString)
  , hhea_r :: (TablePosition, Strict.ByteString)
  , hmtx_r :: (TablePosition, Strict.ByteString)
  , loca_r :: (TablePosition, Strict.ByteString)
  , maxp_r :: (TablePosition, Strict.ByteString)
  , post_r :: (TablePosition, Strict.ByteString)
  }
  deriving (Show)

data TablePosition = TablePosition
  { tag    :: String
  , offset :: Word32
  , length :: Word32
  }
  deriving (Show)


data family Record (a :: Symbol)

data instance Record "Component" = Component
  { flags          :: {-# UNPACK #-} !Word16
  , glyphIndex     :: {-# UNPACK #-} !Word16
  , argument1      :: {-# UNPACK #-} !Int32
  , argument2      :: {-# UNPACK #-} !Int32
  , transformation :: Record "Transformation"
  }
  deriving (Show)

data instance Record "Encoding" = Encoding
  { platformID     :: {-# UNPACK #-} !Word16
  , encodingID     :: {-# UNPACK #-} !Word16
  , subtableOffset :: {-# UNPACK #-} !Word32
  }
  deriving (Show)

data instance Record "Glyph"
  = Simple    { numberOfContours  :: {-# UNPACK #-} !Int16
              , xMin              :: {-# UNPACK #-} !Int16
              , yMin              :: {-# UNPACK #-} !Int16
              , xMax              :: {-# UNPACK #-} !Int16
              , yMax              :: {-# UNPACK #-} !Int16
              , endPtsOfContours  :: !(V.Vector Word16)
              , instructionLength :: {-# UNPACK #-} !Word16
              , instructions      :: !(V.Vector Word8)
              , flags             :: !(V.Vector Word8)
              , xCoordinates      :: !(V.Vector Int32)
              , yCoordinates      :: !(V.Vector Int32)
              , numDraws          :: {-# UNPACK #-} !Word64
              , numOffCurve       :: {-# UNPACK #-} !Word64
              }
  -- In a simple glyph record, the xCoordinates and yCoordinates entries are arrays of a mixture of Word8, Word16, Int8 and Int16. We use Int32 which is guaranteed to accomodate all of these variations without overflow.
  | Composite { numberOfContours  :: {-# UNPACK #-} !Int16
              , xMin              :: {-# UNPACK #-} !Int16
              , yMin              :: {-# UNPACK #-} !Int16
              , xMax              :: {-# UNPACK #-} !Int16
              , yMax              :: {-# UNPACK #-} !Int16
              , components        :: {-# UNPACK #-} !(V.Vector (Record "Component"))
              , instructionLength :: {-# UNPACK #-} !Word16
              , instructions      :: {-# UNPACK #-} !(V.Vector Word8)
              }
  -- Glyphs for space characters have no contours, and have no direct description in the glyf table
  | Space
  deriving (Show)

data instance Record "MapGroup"
  = SequentialMapGroup { startCharCode :: {-# UNPACK #-} !Word32  --comment
                       , endCharCode   :: {-# UNPACK #-} !Word32
                       , startGlyphID  :: {-# UNPACK #-} !Word32  -- comemtn
                       }
  | ConstantMapGroup   { startCharCode :: {-# UNPACK #-} !Word32
                       , endCharCode   :: {-# UNPACK #-} !Word32
                       , startGlyphID  :: {-# UNPACK #-} !Word32
                       }
  deriving (Show)

data instance Record "SubHeader" = SubHeader
  { firstCode     :: {-# UNPACK #-} !Word16
  , entryCount    :: {-# UNPACK #-} !Word16
  , idDelta       :: {-# UNPACK #-} !Int16
  , idRangeOffset :: {-# UNPACK #-} !Word16
  }
  deriving (Show)

data instance Record "Transformation"
  = SCALE         { scale   :: {-# UNPACK #-} !Float
                  }
  | X_AND_Y_SCALE { xscale  :: {-# UNPACK #-} !Float
                  , yscale  :: {-# UNPACK #-} !Float
                  }
  | TWO_BY_TWO    { xscale  :: {-# UNPACK #-} !Float
                  , scale01 :: {-# UNPACK #-} !Float
                  , scale10 :: {-# UNPACK #-} !Float
                  , yscale  :: {-# UNPACK #-} !Float
                  }
  deriving (Show)

data instance Record "longHorMetric" = LongHorMetric
  { advanceWidth :: {-# UNPACK #-} !Word16
  , lsb          :: {-# UNPACK #-} !Int16
  }
  deriving (Show)

data family Table  (a :: Symbol)

data instance Table "Encoding"
  -- | In Format 0, length is a Word16.
  -- | We use Word32 here for compatibility with the length entry in other formats.
  -- | In Format 0, language is a Word16.
  -- | We use Word32 here for compatibility with the language entry in other formats.
  -- | In Format 0, glyphIdArray is a size 256 array of Word8.
  -- | We use Word16 here for compatibility with the glyphIdArray entry in other formats.
  = Format_0  { format         :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , glyphIdArray   :: !(V.Vector Word16)
              }
  -- | In Format 2, length is a Word16.
  -- | We use Word32 here for compatibility with the length entry in other formats.
  -- | In Format 2, language is a Word16.
  -- | We use Word32 here for compatibility with the language entry in other formats.
  | Format_2  { format         :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , subHeaderKeys  :: !(V.Vector Word16)
              , subHeaders     :: !(V.Vector (Record "SubHeader"))
              , glyphIdArray   :: !(V.Vector Word16)
              }
  -- | In Format 4, length is a Word16.
  -- | We use Word32 here for compatibility with the length entry in other formats.
  -- | In Format 4, language is a Word16.
  -- | We use Word32 here for compatibility with the language entry in other formats.
  | Format_4  { format         :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , segCountX2     :: {-# UNPACK #-} !Word16
              , searchRange    :: {-# UNPACK #-} !Word16
              , entrySelector  :: {-# UNPACK #-} !Word16
              , rangeShift     :: {-# UNPACK #-} !Word16
              , endCode        :: !(V.Vector Word16)
              , reservedPad    :: {-# UNPACK #-} !Word16
              , startCode      :: !(V.Vector Word16)
              , idDelta        :: !(V.Vector Int16)
              , idRangeOffsets :: !(V.Vector Word16)
              , glyphIdArray   :: !(V.Vector Word16)
              }
  -- | In Format 6, length is a Word16.
  -- | We use Word32 here for compatibility with the length entry in other formats.
  -- | In Format 6, language is a Word16.
  -- | We use Word32 here for compatibility with the language entry in other formats.
  | Format_6  { format         :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , firstCode      :: {-# UNPACK #-} !Word16
              , entryCount     :: {-# UNPACK #-} !Word16
              , glyphIdArray   :: !(V.Vector Word16)
              }
  -- | In Format 8, groups is an array of SequentialMapGroup records.
  -- | We use the common MapGroup type here for compatibility with the groups entry in other formats.
  | Format_8  { format         :: {-# UNPACK #-} !Word16
              , reserved       :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , is32           :: !(V.Vector Word8)
              , numGroups      :: {-# UNPACK #-} !Word32
              , groups         :: !(V.Vector (Record "MapGroup"))
              }
  | Format_10 { format         :: {-# UNPACK #-} !Word16
              , reserved       :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , startCharCode  :: {-# UNPACK #-} !Word32
              , numChars       :: {-# UNPACK #-} !Word32
              , glyphIdArray   :: !(V.Vector Word16)
              }
  -- | In Format 12, groups is an array of SequentialMapGroup records.
  -- | We use the common MapGroup type here for compatibility with the groups entry in other formats.
  | Format_12 { format         :: {-# UNPACK #-} !Word16
              , reserved       :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , numGroups      :: {-# UNPACK #-} !Word32
              , groups         :: !(V.Vector (Record "MapGroup"))
              }
  -- In Format 13, groups is an array of ConstantMapGroup records. We use the common MapGroup type here for compatibility with the groups entry in other formats.
  | Format_13 { format         :: {-# UNPACK #-} !Word16
              , reserved       :: {-# UNPACK #-} !Word16
              , length         :: {-# UNPACK #-} !Word32
              , language       :: {-# UNPACK #-} !Word32
              , numGroups      :: {-# UNPACK #-} !Word32
              , groups         :: !(V.Vector (Record "MapGroup"))
              }
  -- Format 14 not yet added; didn't have time to sort through the specification for it.
  | Format_14 ()
  deriving (Show)

data instance Table "cmap" = T_cmap
  { version         :: {-# UNPACK #-} !Word16
  , numTables       :: {-# UNPACK #-} !Word16
  , encodingRecords :: !(V.Vector (Record "Encoding"))
  , subtables       :: !(V.Vector (Table "Encoding"))
  }
  deriving (Show)

-- The numVertices, numDraws, numOffCurve and numSimple fields
-- are not present in the "glyf" table in a TTF file,
-- but we nevertheless calculate them while parsing so that we know
-- for the GPU buffers meant to store the vertex and draw call data.

data instance Table "glyf" = T_glyf
  { glyphs      :: !(V.Vector (Record "Glyph"))
  , numVertices :: {-# UNPACK #-} !Word64
  , numDraws    :: {-# UNPACK #-} !Word64
  , numOffCurve :: {-# UNPACK #-} !Word64
  , numSimple   :: {-# UNPACK #-} !Word64
  }
  deriving (Show)

data instance Table "head" = T_head
  { majorVersion       :: {-# UNPACK #-} !Word16
  , minorVersion       :: {-# UNPACK #-} !Word16
  , fontRevision       :: {-# UNPACK #-} !Fixed
  , checksumAdjustment :: {-# UNPACK #-} !Word32
  , magicNumber        :: {-# UNPACK #-} !Word32
  , flags              :: {-# UNPACK #-} !Word16
  , unitsPerEm         :: {-# UNPACK #-} !Word16
  , created            :: {-# UNPACK #-} !LONGDATETIME
  , modified           :: {-# UNPACK #-} !LONGDATETIME
  , xMin               :: {-# UNPACK #-} !Int16
  , yMin               :: {-# UNPACK #-} !Int16
  , xMax               :: {-# UNPACK #-} !Int16
  , yMax               :: {-# UNPACK #-} !Int16
  , macStyle           :: {-# UNPACK #-} !Word16
  , lowestRecPPEM      :: {-# UNPACK #-} !Word16
  , fontDirectionHint  :: {-# UNPACK #-} !Int16
  , indexToLocFormat   :: {-# UNPACK #-} !Int16
  , glyphDataFormat    :: {-# UNPACK #-} !Int16
  }
  deriving (Show)

data instance Table "hhea" = T_hhea
  { majorVersion        :: {-# UNPACK #-} !Word16
  , minorVersion        :: {-# UNPACK #-} !Word16
  , ascender            :: {-# UNPACK #-} !FWORD
  , descender           :: {-# UNPACK #-} !FWORD
  , lineGap             :: {-# UNPACK #-} !FWORD
  , advanceWidthMax     :: {-# UNPACK #-} !UFWORD
  , minLeftSideBearing  :: {-# UNPACK #-} !FWORD
  , minRightSideBearing :: {-# UNPACK #-} !FWORD
  , xMaxExtent          :: {-# UNPACK #-} !FWORD
  , caretSlopeRise      :: {-# UNPACK #-} !Int16
  , caretSlopeRun       :: {-# UNPACK #-} !Int16
  , caretOffset         :: {-# UNPACK #-} !Int16
  , reserved1           :: {-# UNPACK #-} !Int16
  , reserved2           :: {-# UNPACK #-} !Int16
  , reserved3           :: {-# UNPACK #-} !Int16
  , reserved4           :: {-# UNPACK #-} !Int16
  , metricDataFormat    :: {-# UNPACK #-} !Int16
  , numberOfHMetrics    :: {-# UNPACK #-} !Word16
  }
  deriving (Show)

data instance Table "hmtx" = T_hmtx
  { hMetrics         :: !(V.Vector (Record "longHorMetric"))
  , leftSideBearings :: !(V.Vector Int16)
  }
  deriving (Show)

-- The raw offsets might be stored as (offset/2), but are parsed into Table "loca" as actual byte offsets.

data instance Table "loca" = T_loca
  { offsets :: !(V.Vector Word32)
  }
  deriving (Show)

data instance Table "maxp"
  = T_maxp_0_5 { version               :: {-# UNPACK #-} !Version16Dot16
               , numGlyphs             :: {-# UNPACK #-} !Word16
               }
  | T_maxp_1_0 { version               :: {-# UNPACK #-} !Version16Dot16
               , numGlyphs             :: {-# UNPACK #-} !Word16
               , maxPoints             :: {-# UNPACK #-} !Word16
               , maxContours           :: {-# UNPACK #-} !Word16
               , maxCompositePoints    :: {-# UNPACK #-} !Word16
               , maxCompositeContours  :: {-# UNPACK #-} !Word16
               , maxZones              :: {-# UNPACK #-} !Word16
               , maxTwilightPoints     :: {-# UNPACK #-} !Word16
               , maxStorage            :: {-# UNPACK #-} !Word16
               , maxFunctionDefs       :: {-# UNPACK #-} !Word16
               , maxInstructionDefs    :: {-# UNPACK #-} !Word16
               , maxStackElements      :: {-# UNPACK #-} !Word16
               , maxSizeOfInstructions :: {-# UNPACK #-} !Word16
               , maxComponentElements  :: {-# UNPACK #-} !Word16
               , maxComponentDepth     :: {-# UNPACK #-} !Word16
               }
  deriving (Show)

data instance Table "post"
  = T_post_1_0 { version            :: {-# UNPACK #-} !Version16Dot16
               , italicAngle        :: {-# UNPACK #-} !Fixed
               , underlinePosition  :: {-# UNPACK #-} !FWORD
               , underlineThickness :: {-# UNPACK #-} !FWORD
               , isFixedPitch       :: {-# UNPACK #-} !Word32
               , minMemType42       :: {-# UNPACK #-} !Word32
               , maxMemType42       :: {-# UNPACK #-} !Word32
               , minMemType1        :: {-# UNPACK #-} !Word32
               , maxMemType1        :: {-# UNPACK #-} !Word32
               }
  -- In a version 2.0 'post' table, the stringData table is a Word8 array, that is intended to be parsed as readable strings. We use a String array and concatenate in the parser, since I don't believe there's any reason not to.
  | T_post_2_0 { version            :: {-# UNPACK #-} !Version16Dot16
               , italicAngle        :: {-# UNPACK #-} !Fixed
               , underlinePosition  :: {-# UNPACK #-} !FWORD
               , underlineThickness :: {-# UNPACK #-} !FWORD
               , isFixedPitch       :: {-# UNPACK #-} !Word32
               , minMemType42       :: {-# UNPACK #-} !Word32
               , maxMemType42       :: {-# UNPACK #-} !Word32
               , minMemType1        :: {-# UNPACK #-} !Word32
               , maxMemType1        :: {-# UNPACK #-} !Word32
               , numGlyphs          :: {-# UNPACK #-} !Word16
               , glyphNameIndex     :: !(V.Vector Word16)
               , stringData         :: !(V.Vector String)
               }
  | T_post_2_5 { version            :: {-# UNPACK #-} !Version16Dot16
               , italicAngle        :: {-# UNPACK #-} !Fixed
               , underlinePosition  :: {-# UNPACK #-} !FWORD
               , underlineThickness :: {-# UNPACK #-} !FWORD
               , isFixedPitch       :: {-# UNPACK #-} !Word32
               , minMemType42       :: {-# UNPACK #-} !Word32
               , maxMemType42       :: {-# UNPACK #-} !Word32
               , minMemType1        :: {-# UNPACK #-} !Word32
               , maxMemType1        :: {-# UNPACK #-} !Word32
               , numGlyphs          :: {-# UNPACK #-} !Word16
               , offset             :: !(V.Vector Int8)
               }
  -- | Version 3.0 is identical as a table to version 1.0.
  -- | The difference is that version 3.0 indicates that no table of glyph names is provided,
  -- | whereas 1.0 indicates that defaults should be used.
  | T_post_3_0 { version            :: {-# UNPACK #-} !Version16Dot16
               , italicAngle        :: {-# UNPACK #-} !Fixed
               , underlinePosition  :: {-# UNPACK #-} !FWORD
               , underlineThickness :: {-# UNPACK #-} !FWORD
               , isFixedPitch       :: {-# UNPACK #-} !Word32
               , minMemType42       :: {-# UNPACK #-} !Word32
               , maxMemType42       :: {-# UNPACK #-} !Word32
               , minMemType1        :: {-# UNPACK #-} !Word32
               , maxMemType1        :: {-# UNPACK #-} !Word32
               }
  deriving (Show)


-- | As far as I understand, every F2DOT14 should be representable (losslessly)
-- | as a 32-bit Float, but I can't find anything to corroborate this,
-- | so I could be missing something.

type F2DOT14 = Float

-- Signed 16-bit integer representing font design units

type FWORD = Int16

-- | Signed 64-bit integer representing Unix time,
-- | i.e. the number of seconds since 00:00 on January 1, 1904, UTC.

type LONGDATETIME = Int64

-- Unsigned 16-bit integer representing font design units

type UFWORD = Word16


f2dot14
  :: Word16
  -> Float
f2dot14 n = -2*sgn + int + dec/16384
  where sgn = fromIntegral $ (shiftR n 15)                        -- extract bit 0
        int = fromIntegral $ (shiftR n 14) .&. 0b0000000000000001 -- extract bit 1
        dec = fromIntegral $         n     .&. 0b0011111111111111 -- extract bits 2-14

getF2DOT14 :: Get F2DOT14
getF2DOT14 = f2dot14 <$> getWord16be

getVersion16Dot16 :: Get Version16Dot16
getVersion16Dot16 = generic
