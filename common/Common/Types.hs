{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoFieldSelectors     #-}
{-# LANGUAGE OverloadedRecordDot  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.Types where

import Data.Int
import Data.Word
import FlatParse.Basic

import Data.Vector qualified as V

import Foreign.Ptr
import Foreign.Storable

data Color = Color
  { red   :: {-# UNPACK #-} !Word8
  , green :: {-# UNPACK #-} !Word8
  , blue  :: {-# UNPACK #-} !Word8
  , alpha :: {-# UNPACK #-} !Word8
  }

data FileParser a e = FileParser
  { parser           :: a -> Parser e (Color, a)
  , defaultColor     :: {-# UNPACK #-} !Color
  , defaultState     :: a
  , cursor           :: Mode -> Color
  , requestedContext :: {-# UNPACK #-} !Int
  }

-- | Font data that we want to avoid recalculating per-frame or per-glyph.

data FontData = FontData
  { unitsPerEmX2 :: {-# UNPACK #-} !Word32
  , ascender     :: {-# UNPACK #-} !Int16
  , descender    :: {-# UNPACK #-} !Int16
  , lineGap      :: {-# UNPACK #-} !Int16
  , lookup       :: Word32 -> (Int, Int, Int, Int)
  }

data GlyphDrawInfo
  = SimpleGlyph    { drawIndices  :: {-# UNPACK #-} !IndexRange
                   , advanceWidth :: {-# UNPACK #-} !Word16
                   , lsb          :: {-# UNPACK #-} !Int16
                   }
  | SpaceGlyph     { advanceWidth :: {-# UNPACK #-} !Word16
                   , lsb          :: {-# UNPACK #-} !Int16
                   }
  | CompositeGlyph [(Int, Transformation)]
  deriving (Read, Show)

data GlyphIndexRanges = GlyphIndexRanges (V.Vector GlyphDrawInfo)
  deriving (Read, Show)

data IndexRange = IndexRange
  { firstr :: {-# UNPACK #-} !Int32
  , lastr  :: {-# UNPACK #-} !Int32
  }
  deriving (Read, Show)

data Mode
  = Normal
  | Insert

data Position = Position
  { base   :: {-# UNPACK #-} !Int
  , offset :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

data Symbol
  = Char        {-# UNPACK #-} !Word32
  | ColorChange {-# UNPACK #-} !Color
  | Cursor      (Mode -> Color)

-- | Standardized data for the affine transformation
-- | required to draw a glyph component of a composite glyph.

data Transformation = Transformation
  { xOffset :: {-# UNPACK #-} !Int32
  , yOffset :: {-# UNPACK #-} !Int32
  , xx      :: {-# UNPACK #-} !Float
  , xy      :: {-# UNPACK #-} !Float
  , yx      :: {-# UNPACK #-} !Float
  , yy      :: {-# UNPACK #-} !Float
  }
  deriving (Read, Show)

data Vertex = Vertex
  { x     :: {-# UNPACK #-} !Int32
  , y     :: {-# UNPACK #-} !Int32
  , flags :: {-# UNPACK #-} !Word32
  }


type Index = Word32


instance Ord Position where
  compare (Position p1 o1) (Position p2 o2) = case (compare p1 p2) of
    LT -> LT
    GT -> GT
    EQ -> compare o1 o2

class (Size a) where
  size :: Int

instance (Storable a) => Size a where
  size = sizeOf @a (error "argument of sizeOf should not be used")

instance Storable Color where
  sizeOf    _ = 4
  alignment _ = 4
  peek ptr = do
    red   <- peek $ castPtr ptr
    green <- peekByteOff (castPtr ptr) 1
    blue  <- peekByteOff (castPtr ptr) 2
    alpha <- peekByteOff (castPtr ptr) 3
    return $ Color
      { red
      , green
      , blue
      , alpha
      }
  poke ptr color = do
    poke        (castPtr ptr)   color.red
    pokeByteOff (castPtr ptr) 1 color.green
    pokeByteOff (castPtr ptr) 2 color.blue
    pokeByteOff (castPtr ptr) 3 color.alpha

instance Storable Vertex where
  sizeOf    _ = 12
  alignment _ = 12
  peek ptr = do
    x     <- peek        (castPtr ptr)
    y     <- peekByteOff (castPtr ptr) 4
    flags <- peekByteOff (castPtr ptr) 8
    return $ Vertex { x, y, flags }
  poke ptr Vertex { x, y, flags } = do
    poke        (castPtr ptr)   x
    pokeByteOff (castPtr ptr) 4 y
    pokeByteOff (castPtr ptr) 8 flags
