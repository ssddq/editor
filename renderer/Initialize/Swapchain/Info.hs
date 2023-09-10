{-# LANGUAGE DataKinds #-}

module Initialize.Swapchain.Info where

import Vk

import Graphics.Vulkan.Ext.VK_KHR_swapchain

-- | Swapchain create info for a swapchain with a given
-- | minImageCount, width, heigth and (graphics) queue family.
-- | Presentation is first-in-first-out.
-- |
-- | Note: for measuring frame times, use VK_PRESENT_MODE_IMMEDIATE_KHR
-- | to prevent the framerate being capped at the refresh rate of the display.
mkSwapchainCreateInfo
  :: VkSurfaceKHR
  -> Word32                    -- minImageCount
  -> Word32                    -- imageExtent width
  -> Word32                    -- imageExtent height
  -> Word32                    -- (graphics) queueFamilyIndex
  -> VkSwapchainKHR
  -> VkSwapchainCreateInfoKHR
mkSwapchainCreateInfo surface imageCount width height queueFamilyIndex oldSwapchain =
  createVk @VkSwapchainCreateInfoKHR
    $ set                @"sType"                 |* VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
   &* set                @"pNext"                 |* VK_NULL
   &* set                @"flags"                 |* VK_ZERO_FLAGS
   &* set                @"surface"               |* surface
   &* set                @"minImageCount"         |* imageCount
   &* set                @"imageFormat"           |* VK_FORMAT_B8G8R8A8_UNORM
   &* set                @"imageColorSpace"       |* VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
   &* set                @"imageExtent"           |* imageExtent width height
   &* set                @"imageArrayLayers"      |* 1
   &* set                @"imageUsage"            |* VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
   &* set                @"imageSharingMode"      |* VK_SHARING_MODE_EXCLUSIVE
   &* setListCountAndRef @"queueFamilyIndexCount"
                         @"pQueueFamilyIndices"   |* [queueFamilyIndex]
   &* set                @"preTransform"          |* VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR
   &* set                @"compositeAlpha"        |* VK_COMPOSITE_ALPHA_OPAQUE_BIT_KHR
   &* set                @"presentMode"           |* VK_PRESENT_MODE_FIFO_KHR
   &* set                @"clipped"               |* VK_FALSE
   &* set                @"oldSwapchain"          |* oldSwapchain
  where imageExtent width height = createVk @VkExtent2D
          $ set @"width"  |* width
         &* set @"height" |* height

-- | ImageView create info for a given image.
mkImageViewCreateInfo
  :: VkImage
  -> VkImageViewCreateInfo
mkImageViewCreateInfo image = createVk @VkImageViewCreateInfo
    $ set             @"sType"            |* VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
   &* set             @"pNext"            |* VK_NULL
   &* set             @"flags"            |* VK_ZERO_FLAGS
   &* set             @"image"            |* image
   &* set             @"viewType"         |* VK_IMAGE_VIEW_TYPE_2D
   &* set             @"format"           |* VK_FORMAT_B8G8R8A8_UNORM
   &* set             @"components"       |* components
   &* set             @"subresourceRange" |* subresourceRange
  where components = createVk @VkComponentMapping
          $ set @"r"              |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"g"              |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"b"              |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"a"              |* VK_COMPONENT_SWIZZLE_IDENTITY
        subresourceRange = createVk @VkImageSubresourceRange
          $ set @"aspectMask"     |* VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"baseMipLevel"   |* 0
         &* set @"levelCount"     |* 1
         &* set @"baseArrayLayer" |* 0
         &* set @"layerCount"     |* 1
