{-# LANGUAGE TemplateHaskell #-}

module Shaders where

import Data.ByteString
import Data.FileEmbed


vertShader0 :: ByteString
vertShader0 = $(makeRelativeToProject "data/vert0.spv" >>= embedFile)

fragShader0 :: ByteString
fragShader0 = $(makeRelativeToProject "data/frag0.spv" >>= embedFile)

vertShader1 :: ByteString
vertShader1 = $(makeRelativeToProject "data/vert1.spv" >>= embedFile)

fragShader1 :: ByteString
fragShader1 = $(makeRelativeToProject "data/frag1.spv" >>= embedFile)

vertShader2 :: ByteString
vertShader2 = $(makeRelativeToProject "data/vert2.spv" >>= embedFile)

fragShader2 :: ByteString
fragShader2 = $(makeRelativeToProject "data/frag2.spv" >>= embedFile)
