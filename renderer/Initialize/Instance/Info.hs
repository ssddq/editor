{-# LANGUAGE DataKinds #-}

module Initialize.Instance.Info where

import Vk

-- | Set applicationInfo. Both applicationName and engineName seem to have no use,
-- | other than to (possibly) allow for application-specific optimizations by the driver.
applicationInfo :: VkApplicationInfo
applicationInfo = createVk @VkApplicationInfo
   $ set                   @"sType"                    |* VK_STRUCTURE_TYPE_APPLICATION_INFO
  &* set                   @"pNext"                    |* VK_NULL
  &* setStrRef             @"pApplicationName"         |* ""
  &* set                   @"applicationVersion"       |* _VK_MAKE_VERSION 1 0 0
  &* setStrRef             @"pEngineName"              |* ""
  &* set                   @"engineVersion"            |* _VK_MAKE_VERSION 1 0 0
  &* set                   @"apiVersion"               |* _VK_MAKE_VERSION 1 0 68

-- | Instance create info with given instance layers and extensions.
-- | Extensions are CStrings, since vulkan-api has CString pattern synonyms for these,
-- | and the required window extensions returned by SDL.vkGetInstanceExtensions are CStrings.
-- | Since vulkan-api provides no pattern synonyms for instance layers,
-- | we write them as regular strings.
mkInstanceCreateInfo
  :: [String]              -- layers
  -> [CString]             -- extensions
  -> VkInstanceCreateInfo
mkInstanceCreateInfo layers extensions = createVk @VkInstanceCreateInfo
   $ set                   @"sType"                    |* VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  &* set                   @"pNext"                    |* VK_NULL
  &* setVkRef              @"pApplicationInfo"         |* applicationInfo
  &* setStrListCountAndRef @"enabledLayerCount"     -- |*
                           @"ppEnabledLayerNames"      |* layers
  &* setListCountAndRef    @"enabledExtensionCount" -- |*
                           @"ppEnabledExtensionNames"  |* extensions
