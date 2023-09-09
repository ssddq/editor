{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Font.Load where

import Data.ByteString qualified as Strict
import Data.Int

import System.IO.MMap

class Slice a where
  slice :: a
        -> (Int64, Int64)
        -> IO Strict.ByteString

-- | Not sure if the implementation of System.IO.MMap is reliable.
-- | The lazy version of mmapFileByteString didn't seem to work,
-- | though I didn't investigate thoroughly.
instance Slice FilePath where
  slice a (n,m) = mmapFileByteString a (Just (n, fromIntegral m))

instance Slice Strict.ByteString where
  slice a (n,m) = pure $ Strict.take (m') $ Strict.drop n' a
    where n' = fromIntegral n
          m' = fromIntegral m
