{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeApplications      #-}

module Initialize.Swapchain.Utils where

import SDL qualified as SDL
import Vk

import Data.Ord
import Data.Vector qualified as V

import Graphics.Vulkan.Ext.VK_KHR_surface
import Graphics.Vulkan.Ext.VK_KHR_swapchain

import SDL.Video.Vulkan qualified as SDL

-- | Get presentable images on a given swapchain, returned as a vector.
retrieveImages
  :: VkDevice
  -> VkSwapchainKHR
  -> IO (V.Vector VkImage)
retrieveImages device swapchain = do
  images <- performEnumerate $ vkGetSwapchainImagesKHR
                                 |- device
                                 |- swapchain
  return $ V.fromList images

-- | Check physical device capabilities for the created physical device and surface,
-- | and use the result to clamp vk.programConstants.
-- |
-- | This is (probably) necessary even immediately after window creation, since e.g.
-- | on X11 a window manager (XMonad) can draw borders over the window,
-- | which reduce the surface imageExtent and cause errors in the validation layers
-- | if not recalculated.
updateConstants
  :: SDL.Window
  -> VkPhysicalDevice
  -> VkSurfaceKHR
  -> Constants
  -> IO Constants
updateConstants window physicalDevice surface constants = do
  capabilities <- perform $ vkGetPhysicalDeviceSurfaceCapabilitiesKHR
                              |- physicalDevice
                              |- surface
  SDL.V2 widthUnclamped heightUnclamped <- SDL.vkGetDrawableSize window
  let minImages      = getField @"minImageCount"  capabilities
      maxImages      = getField @"maxImageCount"  capabilities
      minImageExtent = getField @"minImageExtent" capabilities
      maxImageExtent = getField @"maxImageExtent" capabilities
      minWidth       = getField @"width"  minImageExtent
      minHeight      = getField @"height" minImageExtent
      maxWidth       = getField @"width"  maxImageExtent
      maxHeight      = getField @"height" maxImageExtent
      width  = clamp
                 |- (minWidth , maxWidth )
                 |- fromIntegral widthUnclamped
      height = clamp
                 |- (minHeight, maxHeight)
                 |- fromIntegral heightUnclamped
      imageCount = clamp
                     |- (minImages, maxImages)
                     |- imageCountUnclamped
  return $ case maxImages of
    0 -> constants { present = Area { width, height } }
    _ -> constants { imageCount
                   , present = Area { width, height }
                   }
  where imageCountUnclamped = constants.imageCount
