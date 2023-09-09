#include "vk_mem_alloc.h"
#include "vulkan/vulkan.h"
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module VkExtra where

import Prelude hiding (sinh)
import Foreign.Ptr

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import Graphics.Vulkan.Marshal.Internal

import Graphics.Vulkan.Ext.VK_KHR_bind_memory2
import Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2

foreign import ccall unsafe "vkGetDeviceBufferMemoryRequirements"
  vkGetDeviceBufferMemoryRequirements :: VkDevice
                                      -> (Ptr VkDeviceBufferMemoryRequirements)
                                      -> (Ptr VkMemoryRequirements2)
                                      -> IO ()

type HS_vkGetDeviceBufferMemoryRequirements = VkDevice
                                      -> (Ptr VkDeviceBufferMemoryRequirements)
                                      -> (Ptr VkMemoryRequirements2)
                                      -> IO ()

type PFN_vkGetDeviceBufferMemoryRequirements = FunPtr HS_vkGetDeviceBufferMemoryRequirements

foreign import ccall unsafe "vkGetDeviceImageMemoryRequirements"
  vkGetDeviceImageMemoryRequirements :: VkDevice
                                     -> (Ptr VkDeviceImageMemoryRequirements)
                                     -> (Ptr VkMemoryRequirements2)
                                     -> IO ()

type HS_vkGetDeviceImageMemoryRequirements = VkDevice
                                     -> (Ptr VkDeviceImageMemoryRequirements)
                                     -> (Ptr VkMemoryRequirements2)
                                     -> IO ()

type PFN_vkGetDeviceImageMemoryRequirements = FunPtr HS_vkGetDeviceImageMemoryRequirements

data VkDeviceBufferMemoryRequirements'
type VkDeviceBufferMemoryRequirements = VkStruct VkDeviceBufferMemoryRequirements'

instance VulkanMarshal VkDeviceBufferMemoryRequirements where
  type StructRep VkDeviceBufferMemoryRequirements = 'StructMeta "VkDeviceBufferMemoryRequirements" VkDeviceBufferMemoryRequirements #{size VkDeviceBufferMemoryRequirements } #{alignment VkDeviceBufferMemoryRequirements}
    '[ ('FieldMeta "sType" VkStructureType 'False #{offset VkDeviceBufferMemoryRequirements, sType} 1 'True 'True)
     , ('FieldMeta "pNext" (Ptr Void) 'False #{offset VkDeviceBufferMemoryRequirements, pNext} 1 'True 'True)
     , ('FieldMeta "pCreateInfo" (Ptr VkBufferCreateInfo) 'False #{offset VkDeviceBufferMemoryRequirements, pCreateInfo} 1 'True 'True)
     ] 'False 'False '[]

data VkDeviceImageMemoryRequirements'
type VkDeviceImageMemoryRequirements = VkStruct VkDeviceImageMemoryRequirements'

instance VulkanMarshal VkDeviceImageMemoryRequirements where
  type StructRep VkDeviceImageMemoryRequirements = 'StructMeta "VkDeviceImageMemoryRequirements" VkDeviceImageMemoryRequirements #{size VkDeviceImageMemoryRequirements } #{alignment VkDeviceImageMemoryRequirements}
    '[ ('FieldMeta "sType" VkStructureType 'False #{offset VkDeviceImageMemoryRequirements, sType} 1 'True 'True)
     , ('FieldMeta "pNext" (Ptr Void) 'False #{offset VkDeviceImageMemoryRequirements, pNext} 1 'True 'True)
     , ('FieldMeta "pCreateInfo" (Ptr VkImageCreateInfo) 'False #{offset VkDeviceImageMemoryRequirements, pCreateInfo} 1 'True 'True)
     , ('FieldMeta "planeAspect" VkImageAspectFlagBits 'False #{offset VkDeviceImageMemoryRequirements, planeAspect} 1 'True 'True)
     ] 'False 'False '[]

foreign import ccall unsafe "&vkGetInstanceProcAddr"
  pfn_vkGetInstanceProcAddr :: FunPtr HS_vkGetInstanceProcAddr

foreign import ccall unsafe "&vkGetDeviceProcAddr"
  pfn_vkGetDeviceProcAddr :: FunPtr HS_vkGetDeviceProcAddr

foreign import ccall unsafe "&vkGetPhysicalDeviceProperties"
  pfn_vkGetPhysicalDeviceProperties :: FunPtr HS_vkGetPhysicalDeviceProperties

foreign import ccall unsafe "&vkGetPhysicalDeviceMemoryProperties"
  pfn_vkGetPhysicalDeviceMemoryProperties :: FunPtr HS_vkGetPhysicalDeviceMemoryProperties

foreign import ccall unsafe "&vkAllocateMemory"
  pfn_vkAllocateMemory :: FunPtr HS_vkAllocateMemory

foreign import ccall unsafe "&vkFreeMemory"
  pfn_vkFreeMemory :: FunPtr HS_vkFreeMemory

foreign import ccall unsafe "&vkMapMemory"
  pfn_vkMapMemory :: FunPtr HS_vkMapMemory

foreign import ccall unsafe "&vkUnmapMemory"
  pfn_vkUnmapMemory :: FunPtr HS_vkUnmapMemory

foreign import ccall unsafe "&vkFlushMappedMemoryRanges"
  pfn_vkFlushMappedMemoryRanges :: FunPtr HS_vkFlushMappedMemoryRanges

foreign import ccall unsafe "&vkInvalidateMappedMemoryRanges"
  pfn_vkInvalidateMappedMemoryRanges :: FunPtr HS_vkInvalidateMappedMemoryRanges

foreign import ccall unsafe "&vkBindBufferMemory"
  pfn_vkBindBufferMemory :: FunPtr HS_vkBindBufferMemory

foreign import ccall unsafe "&vkBindImageMemory"
  pfn_vkBindImageMemory :: FunPtr HS_vkBindImageMemory

foreign import ccall unsafe "&vkGetBufferMemoryRequirements"
  pfn_vkGetBufferMemoryRequirements :: FunPtr HS_vkGetBufferMemoryRequirements

foreign import ccall unsafe "&vkGetImageMemoryRequirements"
  pfn_vkGetImageMemoryRequirements :: FunPtr HS_vkGetImageMemoryRequirements

foreign import ccall unsafe "&vkCreateBuffer"
  pfn_vkCreateBuffer :: FunPtr HS_vkCreateBuffer

foreign import ccall unsafe "&vkDestroyBuffer"
  pfn_vkDestroyBuffer :: FunPtr HS_vkDestroyBuffer

foreign import ccall unsafe "&vkCreateImage"
  pfn_vkCreateImage :: FunPtr HS_vkCreateImage

foreign import ccall unsafe "&vkDestroyImage"
  pfn_vkDestroyImage :: FunPtr HS_vkDestroyImage

foreign import ccall unsafe "&vkCmdCopyBuffer"
  pfn_vkCmdCopyBuffer :: FunPtr HS_vkCmdCopyBuffer

foreign import ccall unsafe "&vkGetBufferMemoryRequirements2"
  pfn_vkGetBufferMemoryRequirements2KHR :: FunPtr HS_vkGetBufferMemoryRequirements2KHR

foreign import ccall unsafe "&vkGetImageMemoryRequirements2"
  pfn_vkGetImageMemoryRequirements2KHR :: FunPtr HS_vkGetImageMemoryRequirements2KHR

foreign import ccall unsafe "&vkBindBufferMemory2"
  pfn_vkBindBufferMemory2KHR :: FunPtr HS_vkBindBufferMemory2KHR

foreign import ccall unsafe "&vkBindImageMemory2"
  pfn_vkBindImageMemory2KHR :: FunPtr HS_vkBindImageMemory2KHR

foreign import ccall unsafe "&vkGetPhysicalDeviceMemoryProperties2"
  pfn_vkGetPhysicalDeviceMemoryProperties2KHR :: FunPtr HS_vkGetPhysicalDeviceMemoryProperties2KHR

foreign import ccall unsafe "&vkGetDeviceBufferMemoryRequirements"
  pfn_vkGetDeviceBufferMemoryRequirements :: FunPtr HS_vkGetDeviceBufferMemoryRequirements

foreign import ccall unsafe "&vkGetDeviceImageMemoryRequirements"
  pfn_vkGetDeviceImageMemoryRequirements :: FunPtr HS_vkGetDeviceImageMemoryRequirements

