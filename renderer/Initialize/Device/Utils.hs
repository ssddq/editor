{-# LANGUAGE DataKinds #-}

module Initialize.Device.Utils where

import Vk

import Data.Bits
import Data.List

-- | Find a suitable memory type.

findHostMemoryIndex
  :: VkMemoryRequirements
  -> VkPhysicalDeviceMemoryProperties
  -> Word32
findHostMemoryIndex memReqs memProps = case (suitableMemoryTypes) of
  (a : _) -> a
  []      -> error "No suitable memory type found on host device."
  where suitableMemoryTypes = filter (supportsHostMemory memReqs memProps) [0..memTypeCnt]
        memTypeCnt = getField @"memoryTypeCount" memProps - 1

-- | Retrieve queue family properties

getQueueFamilyProperties
  :: VkPhysicalDevice
  -> IO [VkQueueFamilyProperties]
getQueueFamilyProperties physicalDevice = do
  performEnumerate $ vkGetPhysicalDeviceQueueFamilyProperties
                       |- physicalDevice

-- | Given a list of VkQueueFamilyProperties, graphicsQueueIndex
-- | either finds the queueFamilyIndex of the first queue family with graphics capabilities,
-- | or returns Nothing.

graphicsQueueIndex
  :: [VkQueueFamilyProperties]
  -> Maybe Word32
graphicsQueueIndex queueFamilyProperties =
  fmap
    |* fromIntegral
    |* findIndex
         |- hasBit   VK_QUEUE_GRAPHICS_BIT
                   . getField @"queueFlags"
         |- queueFamilyProperties

-- | Test whether a queue has a given set of flags

hasBit
  :: VkQueueBitmask FlagMask
  -> VkQueueFlags
  -> Bool
hasBit bitmask flags = flags .&. bitmask /= zeroBits

-- | Retrieve memory property flags for a given memory type index.

memPropFlags
  :: Word32
  -> VkPhysicalDeviceMemoryProperties
  -> VkMemoryPropertyFlags
memPropFlags n memProps = propFlag
  where propFlag = getField            @"propertyFlags" |* memType
        memType  = getFieldArrayUnsafe @"memoryTypes"   |* fromIntegral n
                                                        |* memProps

-- | Test that a memory type meets given requirements,
-- | and is both host-visible and host-coherent.

supportsHostMemory
  :: VkMemoryRequirements
  -> VkPhysicalDeviceMemoryProperties
  -> Word32
  -> Bool
supportsHostMemory memReqs memProps n = satisfiesRequirements && isSuitable
  where satisfiesRequirements = testBit
                                  |- memTypeBit
                                  |- fromIntegral n
        isSuitable = memPropFlags n memProps
                       .&. VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
                       .|. VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
                  /= zeroBits
        memTypeBit = getField @"memoryTypeBits"  memReqs
