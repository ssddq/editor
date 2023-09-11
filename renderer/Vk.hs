{-# OPTIONS_GHC -F -pgmF=tpr-pp#-}

{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedRecordDot       #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE UndecidableInstances      #-}

module Vk
  ( module Common
  , module Control.Monad
  , module Debug.Trace
  , module Foreign.Marshal.Alloc
  , module Foreign.Marshal.Array
  , module Foreign.Marshal.Utils
  , module Foreign.Ptr
  , module Foreign.Storable
  , module Graphics.Vulkan
  , module Graphics.Vulkan.Core_1_0
  , module Graphics.Vulkan.Marshal.Create
  , module Preprocessor
  , module Types
  , module Utils
  , module VMA
  , module Vk
  ) where

import Common
import Preprocessor hiding (main)
import Types
import Utils
import VMA

import Control.Monad

import Debug.Trace

import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Marshal.Create

defaultConstants :: Constants
defaultConstants = Constants
  { name       = "Editor"
  , imageCount = 3
  , ppi        = 201.0
  , render     = Area 5120 2880
  , present    = Area 1000 1000
  , fSize      = 20.0
  }


generate [d|
  data Vk commandPool
          renderPipeline
          fullscreenBuffer signals
          stream   drawBuffers      vulkan font
     = Vk  { constants        :: Constants
           , mode             :: Mode
           , vulkan           :: Field vulkan              Vulkan
           , signals          :: Field signals             Signals
           , font             :: Field font                Font
           , drawBuffers      :: Field drawBuffers         DrawBuffers
           , stream           :: Field stream              StreamBuffer
           , commandPool      :: Field commandPool         VkCommandPool
           , renderPipeline   :: Field renderPipeline      RenderPipeline
           , fullscreenBuffer :: Field fullscreenBuffer    FullscreenBuffer
           }
  |]

emptyVk :: Null Vk
emptyVk = createEmpty (Vk defaultConstants Normal)
