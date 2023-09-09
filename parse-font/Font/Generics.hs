{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Font.Generics where

import Data.Serialize.Get
import Data.Coerce
import Data.Int
import Data.Word

import Foreign.C.Types

import Numeric.Fixed

data Version16Dot16 = Version16Dot16
  { major :: {-# UNPACK #-} !Word16
  , minor :: {-# UNPACK #-} !Word16
  }
  deriving (Show)


type family Target a where
  Target (a -> b) = Target b
  Target b = b


class From b where
  into :: b -> Get (Target b)

instance {-# OVERLAPPING #-} (Generic a, From (b -> c)) => From (a -> (b -> c)) where
  {-# INLINE into #-}
  into f = f <$> generic @a >>= into

instance {-# OVERLAPPABLE #-} (Generic a, Target b ~ b) => From (a -> b) where
  {-# INLINE into #-}
  into f = f <$> generic @a

class Generic b where
  generic :: Get b

instance Generic Word8 where
  {-# INLINE generic #-}
  generic = getWord8

instance Generic Word16 where
  {-# INLINE generic #-}
  generic = getWord16be

instance Generic Word32 where
  {-# INLINE generic #-}
  generic = getWord32be

instance Generic Int8 where
  {-# INLINE generic #-}
  generic = getInt8

instance Generic Int16 where
  {-# INLINE generic #-}
  generic = getInt16be

instance Generic Int32 where
  {-# INLINE generic #-}
  generic = getInt32be

instance Generic Int64 where
  {-# INLINE generic #-}
  generic = getInt64be

instance Generic Fixed where
  {-# INLINE generic #-}
  generic = getFixed16_16be

instance Generic Version16Dot16 where
  {-# INLINE generic #-}
  generic = do
    major <- getWord16be
    minor <- getWord16be
    return $ Version16Dot16 { major, minor }


-- Signed fixed point (16.16) decimal numbers, using Int32 as the primitive underlying type.

{-# INLINE getFixed16_16be #-}
getFixed16_16be :: Get Fixed
getFixed16_16be = coerce <$> CInt <$> getInt32be
