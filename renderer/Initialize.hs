{-# OPTIONS_GHC -F -pgmF=tpr-pp#-}

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds         #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Initialize
  ( module Initialize
  , recreateSwapchain
  ) where
import Vk

import Graphics.Vulkan.Ext.VK_EXT_debug_utils

import Initialize.Allocator
import Initialize.Device
import Initialize.Instance
import Initialize.Swapchain
import Initialize.Window

initializeVk
  :: Vk    { vulkan = X }
  -> IO Vk { vulkan = I }
initializeVk vk = do
  window    <- createWindow
                 |- constants
  instance_ <- createInstance
                 |- instanceLayers
                 |- instanceExtensions
                 |- window
  physicalDevice   <- selectPhysicalDevice
                        |- instance_
  queueFamilyIndex <- selectGraphicsQueueFamily
                        |- physicalDevice
  device  <- createDevice
               |- deviceLayers
               |- deviceExtensions
               |- physicalDevice
               |- queueFamilyIndex
  surface <- createSurface
               |- instance_
               |- window
  allocator <- createAllocator
                 |- physicalDevice
                 |- device
                 |- instance_
  ( newConstants, swapchain, imageViews ) <- createSwapchain
                                               |- constants
                                               |- device
                                               |- surface
                                               |- window
                                               |- physicalDevice
                                               |- queueFamilyIndex
                                               |- VK_NULL
  let vulkan = Vulkan { window
                      , instance_
                      , physicalDevice
                      , queueFamilyIndex
                      , device
                      , surface
                      , allocator
                      , swapchain
                      , imageViews
                      }
  return $ vk { vulkan
              , constants = newConstants
              }
  where constants = vk.constants
        instanceLayers = [ "VK_LAYER_KHRONOS_validation" -- validation layers
                         -- , "VK_LAYER_MESA_overlay"       -- GPU name + FPS overlay, use with VK_PRESENT_MODE_IMMEDIATE_KHR
                         -- , "VK_LAYER_LUNARG_api_dump"    -- print API calls to stdout
                         ]
        instanceExtensions = [ VK_EXT_DEBUG_UTILS_EXTENSION_NAME
                             ]
        deviceLayers     = []
        deviceExtensions = []
