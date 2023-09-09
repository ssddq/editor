{-# LANGUAGE PatternSynonyms #-}

module Font.TTF.Patterns where

import Data.Bits
import Data.Word

test
  :: Bits a
  => a
  -> Bool
test a = a /= zeroBits



-- * Table "cmap"

pattern PlatformID_Custom :: Word16
pattern PlatformID_Custom = 4

pattern PlatformID_ISO :: Word16
pattern PlatformID_ISO = 2

pattern PlatformID_Macintosh :: Word16
pattern PlatformID_Macintosh = 1

pattern PlatformID_Unicode :: Word16
pattern PlatformID_Unicode = 0

pattern PlatformID_Windows :: Word16
pattern PlatformID_Windows = 3



-- * Table "cmap": Unicode encodingIDs

pattern EncodingID_Unicode_1_0 :: Word16
pattern EncodingID_Unicode_1_0 = 0

pattern EncodingID_Unicode_1_1 :: Word16
pattern EncodingID_Unicode_1_1 = 1

pattern EncodingID_Unicode_2_0 :: Word16
pattern EncodingID_Unicode_2_0 = 4

pattern EncodingID_Unicode_2_0_BMP :: Word16
pattern EncodingID_Unicode_2_0_BMP = 3

pattern EncodingID_Unicode_Full :: Word16
pattern EncodingID_Unicode_Full = 6

pattern EncodingID_Unicode_ISO_10646 :: Word16
pattern EncodingID_Unicode_ISO_10646 = 2

pattern EncodingID_Unicode_Variation :: Word16
pattern EncodingID_Unicode_Variation = 5



-- * Table "cmap": ISO encodingIDs

pattern EncodingID_ISO_10646 :: Word16
pattern EncodingID_ISO_10646 = 1

pattern EncodingID_ISO_8859_1 :: Word16
pattern EncodingID_ISO_8859_1 = 2

pattern EncodingID_ISO_ASCII :: Word16
pattern EncodingID_ISO_ASCII = 0



-- * Table "cmap": Windows encodingIDs

pattern EncodingID_Windows_Big5 :: Word16
pattern EncodingID_Windows_Big5 = 4

pattern EncodingID_Windows_Johab :: Word16
pattern EncodingID_Windows_Johab = 6

pattern EncodingID_Windows_PRC :: Word16
pattern EncodingID_Windows_PRC = 3

pattern EncodingID_Windows_ShiftJIS :: Word16
pattern EncodingID_Windows_ShiftJIS = 2

pattern EncodingID_Windows_Symbol :: Word16
pattern EncodingID_Windows_Symbol = 0

pattern EncodingID_Windows_Unicode_BMP :: Word16
pattern EncodingID_Windows_Unicode_BMP = 1

pattern EncodingID_Windows_Unicode_Full :: Word16
pattern EncodingID_Windows_Unicode_Full = 10

pattern EncodingID_Windows_Wansung :: Word16
pattern EncodingID_Windows_Wansung = 5



-- * Table "glyf": simple glyph flags

pattern ON_CURVE_POINT :: Word8
pattern ON_CURVE_POINT = 0x01

pattern OVERLAP_SIMPLE :: Word8
pattern OVERLAP_SIMPLE = 0x40

pattern REPEAT_FLAG :: Word8
pattern REPEAT_FLAG = 0x08

pattern X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR :: Word8
pattern X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR = 0x10

pattern X_SHORT_VECTOR :: Word8
pattern X_SHORT_VECTOR = 0x02

pattern Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR :: Word8
pattern Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR = 0x20

pattern Y_SHORT_VECTOR :: Word8
pattern Y_SHORT_VECTOR = 0x04



-- * Table "glyf": component glyph flags

pattern ARGS_ARE_XY_VALUES :: Word16
pattern ARGS_ARE_XY_VALUES = 0x0002

pattern ARG_1_AND_2_ARE_WORDS :: Word16
pattern ARG_1_AND_2_ARE_WORDS = 0x0001

pattern MORE_COMPONENTS :: Word16
pattern MORE_COMPONENTS = 0x0020

pattern OVERLAP_COMPOUND :: Word16
pattern OVERLAP_COMPOUND = 0x0400

pattern ROUND_XY_TO_GRID :: Word16
pattern ROUND_XY_TO_GRID = 0x0004

pattern SCALED_COMPONENT_OFFSET :: Word16
pattern SCALED_COMPONENT_OFFSET = 0x0800

pattern UNSCALED_COMPONENT_OFFSET :: Word16
pattern UNSCALED_COMPONENT_OFFSET = 0x1000

pattern USE_MY_METRICS :: Word16
pattern USE_MY_METRICS = 0x0200

pattern WE_HAVE_AN_X_AND_Y_SCALE :: Word16
pattern WE_HAVE_AN_X_AND_Y_SCALE = 0x0040

pattern WE_HAVE_A_SCALE :: Word16
pattern WE_HAVE_A_SCALE = 0x0008

pattern WE_HAVE_A_TWO_BY_TWO :: Word16
pattern WE_HAVE_A_TWO_BY_TWO = 0x0080

pattern WE_HAVE_INSTRUCTIONS :: Word16
pattern WE_HAVE_INSTRUCTIONS = 0x0100
