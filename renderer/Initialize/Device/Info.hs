{-# LANGUAGE DataKinds #-}

module Initialize.Device.Info where

import Vk

import Graphics.Vulkan.Ext.VK_KHR_swapchain

-- | Device create info with given (graphics) queue family index, layers and extensions.
-- | Extensions are CStrings, since vulkan-api has CString pattern synonyms for these,
-- | and the required window extensions returned by SDL.vkGetInstanceExtensions are CStrings.
-- | Since vulkan-api provides no pattern synonyms for instance layers,
-- | we write them as regular strings.
mkDeviceCreateInfo
  :: Word32              -- queueFamilyIndex
  -> [String]            -- layers
  -> [CString]           -- extensions
  -> VkDeviceCreateInfo
mkDeviceCreateInfo queueFamilyIndex layers extensions = createVk @VkDeviceCreateInfo
   $ set                   @"sType"                      |* VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  &* set                   @"pNext"                      |* VK_NULL
  &* set                   @"flags"                      |* VK_ZERO_FLAGS
  &* setListCountAndRef    @"queueCreateInfoCount"    -- |*
                           @"pQueueCreateInfos"          |* [queueCreateInfo]
  &* setStrListCountAndRef @"enabledLayerCount"       -- |*
                           @"ppEnabledLayerNames"        |* layers
  &* setListCountAndRef    @"enabledExtensionCount"   -- |*
                           @"ppEnabledExtensionNames"    |* VK_KHR_SWAPCHAIN_EXTENSION_NAME : extensions
  &* setVkRef              @"pEnabledFeatures"           |* enabledFeatures
  where enabledFeatures = createVk @VkPhysicalDeviceFeatures
           $ set                 @"sampleRateShading"   |* VK_TRUE
          &* set                 @"samplerAnisotropy"   |* VK_TRUE
          &* set                 @"multiDrawIndirect"   |* VK_TRUE
        queueCreateInfo = createVk @VkDeviceQueueCreateInfo
           $ set                 @"sType"               |* VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
          &* set                 @"pNext"               |* VK_NULL
          &* set                 @"flags"               |* VK_ZERO_FLAGS
          &* set                 @"queueFamilyIndex"    |* queueFamilyIndex
          &* setListCountAndRef  @"queueCount"       -- |*
                                 @"pQueuePriorities"    |* [1.0]
