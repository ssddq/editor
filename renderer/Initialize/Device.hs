{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Initialize.Device where

import Vk

import Initialize.Device.Info
import Initialize.Device.Utils

-- | Selects a physical device given a Vulkan instance. This function currently
-- | either selects the first physical device enumerated by vkEnumeratePhysicalDevices,
-- | or returns an error if no physical devices could be found.
selectPhysicalDevice
  :: VkInstance
  -> IO VkPhysicalDevice
selectPhysicalDevice instance_ = do
  physicalDevices <- performEnumerate $ vkEnumeratePhysicalDevices
                                          |- instance_
  let physicalDevice = case (physicalDevices) of
                         ( dev : _ ) -> dev
                         []          -> error "Cannot find a valid physical device for Vulkan."
  return $ physicalDevice

-- | Selects a graphics queue family given a physical device. This function currently
-- | either selects the first graphics queue family enumerated by getQueueFamilyProperties,
-- | or returns an error if no queue family with graphics capabilities could be found.
selectGraphicsQueueFamily
  :: VkPhysicalDevice
  -> IO Word32
selectGraphicsQueueFamily physicalDevice = do
  queueFamilyProperties <- getQueueFamilyProperties
                             |- physicalDevice
  return $ case (graphicsQueueIndex queueFamilyProperties) of
             Just queueFamilyIndex -> queueFamilyIndex
             Nothing               -> error "Could not find graphics queue family."

-- | Creates a logical device with the given device layers and extensions.
-- | The logical device is created along with a single graphics queue,
-- | and has sample shading, sampler anisotropy, indirect multi-draw and independen blending features enabled.
createDevice
  :: [String]
  -> [CString]
  -> VkPhysicalDevice
  -> Word32
  -> IO VkDevice
createDevice layers extensions physicalDevice queueFamilyIndex = do
  let deviceCreateInfo = mkDeviceCreateInfo
                           |- queueFamilyIndex
                           |- layers
                           |- extensions
  device <- perform $ vkCreateDevice
                        |- physicalDevice
                        |- p deviceCreateInfo
                        |- VK_NULL
  return $ device
