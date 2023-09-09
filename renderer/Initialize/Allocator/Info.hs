{-# LANGUAGE DataKinds #-}

module Initialize.Allocator.Info where

import Vk

-- | Allocator create info for a VMA allocator.

mkAllocatorCreateInfo
  :: VkPhysicalDevice
  -> VkDevice
  -> VkInstance
  -> VmaAllocatorCreateInfo
mkAllocatorCreateInfo physicalDevice device instance_ = createVk @VmaAllocatorCreateInfo
   $ set       @"flags"                          |* VK_ZERO_FLAGS
  &* set       @"physicalDevice"                 |* physicalDevice
  &* set       @"device"                         |* device
  &* set       @"preferredLargeHeapBlockSize"    |* 0
  &* set       @"pAllocationCallbacks"           |* VK_NULL
  &* set       @"pDeviceMemoryCallbacks"         |* VK_NULL
  &* set       @"pHeapSizeLimit"                 |* VK_NULL
  &* setVkRef  @"pVulkanFunctions"               |* vulkanFunctions
  &* set       @"instance"                       |* instance_
  &* set       @"vulkanApiVersion"               |* _VK_MAKE_VERSION 1 0 68
  &* set       @"pTypeExternalMemoryHandleTypes" |* VK_NULL

-- | Pointers to Vulkan functions used by the VMA library.

vulkanFunctions :: VmaVulkanFunctions
vulkanFunctions = createVk @VmaVulkanFunctions
   $ set  @"vkGetInstanceProcAddr"                   |* pfn_vkGetInstanceProcAddr
  &* set  @"vkGetDeviceProcAddr"                     |* pfn_vkGetDeviceProcAddr
  &* set  @"vkGetPhysicalDeviceProperties"           |* pfn_vkGetPhysicalDeviceProperties
  &* set  @"vkGetPhysicalDeviceMemoryProperties"     |* pfn_vkGetPhysicalDeviceMemoryProperties
  &* set  @"vkAllocateMemory"                        |* pfn_vkAllocateMemory
  &* set  @"vkFreeMemory"                            |* pfn_vkFreeMemory
  &* set  @"vkMapMemory"                             |* pfn_vkMapMemory
  &* set  @"vkUnmapMemory"                           |* pfn_vkUnmapMemory
  &* set  @"vkFlushMappedMemoryRanges"               |* pfn_vkFlushMappedMemoryRanges
  &* set  @"vkInvalidateMappedMemoryRanges"          |* pfn_vkInvalidateMappedMemoryRanges
  &* set  @"vkBindBufferMemory"                      |* pfn_vkBindBufferMemory
  &* set  @"vkBindImageMemory"                       |* pfn_vkBindImageMemory
  &* set  @"vkGetBufferMemoryRequirements"           |* pfn_vkGetBufferMemoryRequirements
  &* set  @"vkGetImageMemoryRequirements"            |* pfn_vkGetImageMemoryRequirements
  &* set  @"vkCreateBuffer"                          |* pfn_vkCreateBuffer
  &* set  @"vkDestroyBuffer"                         |* pfn_vkDestroyBuffer
  &* set  @"vkCreateImage"                           |* pfn_vkCreateImage
  &* set  @"vkDestroyImage"                          |* pfn_vkDestroyImage
  &* set  @"vkCmdCopyBuffer"                         |* pfn_vkCmdCopyBuffer
  &* set  @"vkGetBufferMemoryRequirements2KHR"       |* pfn_vkGetBufferMemoryRequirements2KHR
  &* set  @"vkGetImageMemoryRequirements2KHR"        |* pfn_vkGetImageMemoryRequirements2KHR
  &* set  @"vkBindBufferMemory2KHR"                  |* pfn_vkBindBufferMemory2KHR
  &* set  @"vkBindImageMemory2KHR"                   |* pfn_vkBindImageMemory2KHR
  &* set  @"vkGetPhysicalDeviceMemoryProperties2KHR" |* pfn_vkGetPhysicalDeviceMemoryProperties2KHR
  &* set  @"vkGetDeviceBufferMemoryRequirements"     |* pfn_vkGetDeviceBufferMemoryRequirements
  &* set  @"vkGetDeviceImageMemoryRequirements"      |* pfn_vkGetDeviceImageMemoryRequirements
