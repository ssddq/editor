{-# OPTIONS_GHC -F -pgmF=tpr-pp#-}

{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell     #-}

module Initialize.Swapchain where

import SDL qualified as SDL
import Vk

import Graphics.Vulkan.Ext.VK_KHR_swapchain

import Initialize.Swapchain.Info
import Initialize.Swapchain.Utils

-- | Initial swapchain creation, with checks to update (clamp) program constants.

createSwapchain
  :: Constants
  -> VkDevice
  -> VkSurfaceKHR
  -> SDL.Window
  -> VkPhysicalDevice
  -> Word32
  -> VkSwapchainKHR
  -> IO (Constants, VkSwapchainKHR, ImageViews)
createSwapchain oldConstants device surface window physicalDevice queueFamilyIndex oldSwapchain = do
  newConstants <- updateConstants
                    |- window
                    |- physicalDevice
                    |- surface
                    |- oldConstants
  let Constants {..} = newConstants
      swapchainCreateInfo = mkSwapchainCreateInfo
                              |- surface
                              |- imageCount
                              |- present.width
                              |- present.height
                              |- queueFamilyIndex
                              |- oldSwapchain
  swapchain <- perform $ vkCreateSwapchainKHR
                           |- device
                           |- p swapchainCreateInfo
                           |- VK_NULL
  imageList <- retrieveImages
                 |- device
                 |- swapchain
  imageViews <- mapM
                  |- createImageView
                  |- imageList
  return $ (newConstants, swapchain, imageViews)
  where createImageView image = do
          let imageViewCreateInfo = mkImageViewCreateInfo image
          perform $ vkCreateImageView
                      |- device
                      |- p imageViewCreateInfo
                      |- VK_NULL

-- | Recreate the swapchain. This uses existing program constants,
-- | e.g. width and height, which should be updated separately prior
-- | to recreation. Presentation is first-in-first-out, and only a
-- | single graphics queue family is used.

recreateSwapchain
  :: Vk    { vulkan = I }
  -> IO Vk { vulkan = I }
recreateSwapchain vk = do
  vkDeviceWaitIdle device
  ( newConstants, newSwapchain, newImageViews ) <- createSwapchain
                                                     |- constants
                                                     |- device
                                                     |- surface
                                                     |- window
                                                     |- physicalDevice
                                                     |- queueFamilyIndex
                                                     |- swapchain
  -- | To ensure that cleanup is not performed while
  -- | swapchain imageViews are in use, wait until
  -- | the device is idle.
  mapM
    |- destroyImageView
    |- imageViews
  vkDestroySwapchainKHR
    |- device
    |- swapchain
    |- VK_NULL
  return $ vk { vulkan = vulkan { swapchain  = newSwapchain
                                , imageViews = newImageViews
                                }
              , constants = newConstants
              }
  where Vk     {..} = vk
        Vulkan {..} = vulkan
        destroyImageView imageView = vkDestroyImageView
                                       |- device
                                       |- imageView
                                       |- VK_NULL
