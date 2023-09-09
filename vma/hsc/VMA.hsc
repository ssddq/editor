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

module VMA (module VMA, module VkExtra) where

import Prelude 
import Foreign.Ptr
import Foreign.Storable
import Data.Bits
import Data.Word

import VkExtra 

import Graphics.Vulkan
import Graphics.Vulkan.Core_1_0

import Graphics.Vulkan.Marshal.Internal

import Graphics.Vulkan.Ext.VK_KHR_bind_memory2
import Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2
import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2

type VmaAllocatorCreateFlagBits = VmaAllocatorCreateBitmask FlagBit
pattern VmaAllocatorCreateFlagBits :: VkFlags -> VmaAllocatorCreateBitmask FlagBit
pattern VmaAllocatorCreateFlagBits n = VmaAllocatorCreateBitmask n

pattern VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT }

pattern VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT }

pattern VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT }

pattern VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT }

pattern VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT }

pattern VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT }

pattern VMA_ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT }

pattern VMA_ALLOCATOR_CREATE_FLAG_BITS_MAX_ENUM :: VmaAllocatorCreateBitmask a
pattern VMA_ALLOCATOR_CREATE_FLAG_BITS_MAX_ENUM = VmaAllocatorCreateBitmask #{const VMA_ALLOCATOR_CREATE_FLAG_BITS_MAX_ENUM }

instance Show (VmaAllocatorCreateBitmask a) where
  showsPrec _ VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT = showString "VMA_ALLOCATOR_CREATE_EXTERNALLY_SYNCHRONIZED_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT = showString "VMA_ALLOCATOR_CREATE_KHR_DEDICATED_ALLOCATION_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT = showString "VMA_ALLOCATOR_CREATE_KHR_BIND_MEMORY2_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT = showString "VMA_ALLOCATOR_CREATE_EXT_MEMORY_BUDGET_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT = showString "VMA_ALLOCATOR_CREATE_AMD_DEVICE_COHERENT_MEMORY_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT = showString "VMA_ALLOCATOR_CREATE_BUFFER_DEVICE_ADDRESS_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT = showString "VMA_ALLOCATOR_CREATE_EXT_MEMORY_PRIORITY_BIT"
  showsPrec _ VMA_ALLOCATOR_CREATE_FLAG_BITS_MAX_ENUM = showString "VMA_ALLOCATOR_CREATE_FLAG_BITS_MAX_ENUM"


newtype VmaAllocatorCreateBitmask (a :: FlagType) = VmaAllocatorCreateBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaAllocatorCreateFlags = VmaAllocatorCreateBitmask FlagMask
pattern VmaAllocatorCreateFlags :: VkFlags -> VmaAllocatorCreateBitmask FlagMask
pattern VmaAllocatorCreateFlags n = VmaAllocatorCreateBitmask n
deriving instance Bits (VmaAllocatorCreateBitmask FlagMask)
deriving instance FiniteBits (VmaAllocatorCreateBitmask FlagMask)

newtype VmaMemoryUsage = VmaMemoryUsage Word32
  deriving (Eq, Ord, Enum, Storable)
pattern VMA_MEMORY_USAGE_UNKNOWN :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_UNKNOWN = VmaMemoryUsage #{const VMA_MEMORY_USAGE_UNKNOWN }

pattern VMA_MEMORY_USAGE_GPU_ONLY :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_GPU_ONLY = VmaMemoryUsage #{const VMA_MEMORY_USAGE_GPU_ONLY }

pattern VMA_MEMORY_USAGE_CPU_ONLY :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_CPU_ONLY = VmaMemoryUsage #{const VMA_MEMORY_USAGE_CPU_ONLY }

pattern VMA_MEMORY_USAGE_CPU_TO_GPU :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_CPU_TO_GPU = VmaMemoryUsage #{const VMA_MEMORY_USAGE_CPU_TO_GPU }

pattern VMA_MEMORY_USAGE_GPU_TO_CPU :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_GPU_TO_CPU = VmaMemoryUsage #{const VMA_MEMORY_USAGE_GPU_TO_CPU }

pattern VMA_MEMORY_USAGE_CPU_COPY :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_CPU_COPY = VmaMemoryUsage #{const VMA_MEMORY_USAGE_CPU_COPY }

pattern VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED = VmaMemoryUsage #{const VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED }

pattern VMA_MEMORY_USAGE_AUTO :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_AUTO = VmaMemoryUsage #{const VMA_MEMORY_USAGE_AUTO }

pattern VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE = VmaMemoryUsage #{const VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE }

pattern VMA_MEMORY_USAGE_AUTO_PREFER_HOST :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_AUTO_PREFER_HOST = VmaMemoryUsage #{const VMA_MEMORY_USAGE_AUTO_PREFER_HOST }

pattern VMA_MEMORY_USAGE_MAX_ENUM :: VmaMemoryUsage 
pattern VMA_MEMORY_USAGE_MAX_ENUM = VmaMemoryUsage #{const VMA_MEMORY_USAGE_MAX_ENUM }

instance Show VmaMemoryUsage where
  showsPrec _ VMA_MEMORY_USAGE_UNKNOWN = showString "VMA_MEMORY_USAGE_UNKNOWN"
  showsPrec _ VMA_MEMORY_USAGE_GPU_ONLY = showString "VMA_MEMORY_USAGE_GPU_ONLY"
  showsPrec _ VMA_MEMORY_USAGE_CPU_ONLY = showString "VMA_MEMORY_USAGE_CPU_ONLY"
  showsPrec _ VMA_MEMORY_USAGE_CPU_TO_GPU = showString "VMA_MEMORY_USAGE_CPU_TO_GPU"
  showsPrec _ VMA_MEMORY_USAGE_GPU_TO_CPU = showString "VMA_MEMORY_USAGE_GPU_TO_CPU"
  showsPrec _ VMA_MEMORY_USAGE_CPU_COPY = showString "VMA_MEMORY_USAGE_CPU_COPY"
  showsPrec _ VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED = showString "VMA_MEMORY_USAGE_GPU_LAZILY_ALLOCATED"
  showsPrec _ VMA_MEMORY_USAGE_AUTO = showString "VMA_MEMORY_USAGE_AUTO"
  showsPrec _ VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE = showString "VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE"
  showsPrec _ VMA_MEMORY_USAGE_AUTO_PREFER_HOST = showString "VMA_MEMORY_USAGE_AUTO_PREFER_HOST"
  showsPrec _ VMA_MEMORY_USAGE_MAX_ENUM = showString "VMA_MEMORY_USAGE_MAX_ENUM"


type VmaAllocationCreateFlagBits = VmaAllocationCreateBitmask FlagBit
pattern VmaAllocationCreateFlagBits :: VkFlags -> VmaAllocationCreateBitmask FlagBit
pattern VmaAllocationCreateFlagBits n = VmaAllocationCreateBitmask n

pattern VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT }

pattern VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT }

pattern VMA_ALLOCATION_CREATE_MAPPED_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_MAPPED_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_MAPPED_BIT }

pattern VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT }

pattern VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT }

pattern VMA_ALLOCATION_CREATE_DONT_BIND_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_DONT_BIND_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_DONT_BIND_BIT }

pattern VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT }

pattern VMA_ALLOCATION_CREATE_CAN_ALIAS_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_CAN_ALIAS_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_CAN_ALIAS_BIT }

pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT }

pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT }

pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT }

pattern VMA_ALLOCATION_CREATE_STRATEGY_MASK :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_STRATEGY_MASK = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_STRATEGY_MASK }

pattern VMA_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM :: VmaAllocationCreateBitmask a
pattern VMA_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM = VmaAllocationCreateBitmask #{const VMA_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM }

instance Show (VmaAllocationCreateBitmask a) where
  showsPrec _ VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT = showString "VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT = showString "VMA_ALLOCATION_CREATE_NEVER_ALLOCATE_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_MAPPED_BIT = showString "VMA_ALLOCATION_CREATE_MAPPED_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT = showString "VMA_ALLOCATION_CREATE_USER_DATA_COPY_STRING_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT = showString "VMA_ALLOCATION_CREATE_UPPER_ADDRESS_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_DONT_BIND_BIT = showString "VMA_ALLOCATION_CREATE_DONT_BIND_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT = showString "VMA_ALLOCATION_CREATE_WITHIN_BUDGET_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_CAN_ALIAS_BIT = showString "VMA_ALLOCATION_CREATE_CAN_ALIAS_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT = showString "VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT = showString "VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT = showString "VMA_ALLOCATION_CREATE_HOST_ACCESS_ALLOW_TRANSFER_INSTEAD_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = showString "VMA_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = showString "VMA_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = showString "VMA_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT = showString "VMA_ALLOCATION_CREATE_STRATEGY_BEST_FIT_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT = showString "VMA_ALLOCATION_CREATE_STRATEGY_FIRST_FIT_BIT"
  showsPrec _ VMA_ALLOCATION_CREATE_STRATEGY_MASK = showString "VMA_ALLOCATION_CREATE_STRATEGY_MASK"
  showsPrec _ VMA_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM = showString "VMA_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM"


newtype VmaAllocationCreateBitmask (a :: FlagType) = VmaAllocationCreateBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaAllocationCreateFlags = VmaAllocationCreateBitmask FlagMask
pattern VmaAllocationCreateFlags :: VkFlags -> VmaAllocationCreateBitmask FlagMask
pattern VmaAllocationCreateFlags n = VmaAllocationCreateBitmask n
deriving instance Bits (VmaAllocationCreateBitmask FlagMask)
deriving instance FiniteBits (VmaAllocationCreateBitmask FlagMask)

type VmaPoolCreateFlagBits = VmaPoolCreateBitmask FlagBit
pattern VmaPoolCreateFlagBits :: VkFlags -> VmaPoolCreateBitmask FlagBit
pattern VmaPoolCreateFlagBits n = VmaPoolCreateBitmask n

pattern VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT :: VmaPoolCreateBitmask a
pattern VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT = VmaPoolCreateBitmask #{const VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT }

pattern VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT :: VmaPoolCreateBitmask a
pattern VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT = VmaPoolCreateBitmask #{const VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT }

pattern VMA_POOL_CREATE_ALGORITHM_MASK :: VmaPoolCreateBitmask a
pattern VMA_POOL_CREATE_ALGORITHM_MASK = VmaPoolCreateBitmask #{const VMA_POOL_CREATE_ALGORITHM_MASK }

pattern VMA_POOL_CREATE_FLAG_BITS_MAX_ENUM :: VmaPoolCreateBitmask a
pattern VMA_POOL_CREATE_FLAG_BITS_MAX_ENUM = VmaPoolCreateBitmask #{const VMA_POOL_CREATE_FLAG_BITS_MAX_ENUM }

instance Show (VmaPoolCreateBitmask a) where
  showsPrec _ VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT = showString "VMA_POOL_CREATE_IGNORE_BUFFER_IMAGE_GRANULARITY_BIT"
  showsPrec _ VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT = showString "VMA_POOL_CREATE_LINEAR_ALGORITHM_BIT"
  showsPrec _ VMA_POOL_CREATE_ALGORITHM_MASK = showString "VMA_POOL_CREATE_ALGORITHM_MASK"
  showsPrec _ VMA_POOL_CREATE_FLAG_BITS_MAX_ENUM = showString "VMA_POOL_CREATE_FLAG_BITS_MAX_ENUM"


newtype VmaPoolCreateBitmask (a :: FlagType) = VmaPoolCreateBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaPoolCreateFlags = VmaPoolCreateBitmask FlagMask
pattern VmaPoolCreateFlags :: VkFlags -> VmaPoolCreateBitmask FlagMask
pattern VmaPoolCreateFlags n = VmaPoolCreateBitmask n
deriving instance Bits (VmaPoolCreateBitmask FlagMask)
deriving instance FiniteBits (VmaPoolCreateBitmask FlagMask)

type VmaDefragmentationFlagBits = VmaDefragmentationBitmask FlagBit
pattern VmaDefragmentationFlagBits :: VkFlags -> VmaDefragmentationBitmask FlagBit
pattern VmaDefragmentationFlagBits n = VmaDefragmentationBitmask n

pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT }

pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT }

pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT }

pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT }

pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_MASK :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_ALGORITHM_MASK = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_ALGORITHM_MASK }

pattern VMA_DEFRAGMENTATION_FLAG_BITS_MAX_ENUM :: VmaDefragmentationBitmask a
pattern VMA_DEFRAGMENTATION_FLAG_BITS_MAX_ENUM = VmaDefragmentationBitmask #{const VMA_DEFRAGMENTATION_FLAG_BITS_MAX_ENUM }

instance Show (VmaDefragmentationBitmask a) where
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT = showString "VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FAST_BIT"
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT = showString "VMA_DEFRAGMENTATION_FLAG_ALGORITHM_BALANCED_BIT"
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT = showString "VMA_DEFRAGMENTATION_FLAG_ALGORITHM_FULL_BIT"
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT = showString "VMA_DEFRAGMENTATION_FLAG_ALGORITHM_EXTENSIVE_BIT"
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_ALGORITHM_MASK = showString "VMA_DEFRAGMENTATION_FLAG_ALGORITHM_MASK"
  showsPrec _ VMA_DEFRAGMENTATION_FLAG_BITS_MAX_ENUM = showString "VMA_DEFRAGMENTATION_FLAG_BITS_MAX_ENUM"


newtype VmaDefragmentationBitmask (a :: FlagType) = VmaDefragmentationBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaDefragmentationFlags = VmaDefragmentationBitmask FlagMask
pattern VmaDefragmentationFlags :: VkFlags -> VmaDefragmentationBitmask FlagMask
pattern VmaDefragmentationFlags n = VmaDefragmentationBitmask n
deriving instance Bits (VmaDefragmentationBitmask FlagMask)
deriving instance FiniteBits (VmaDefragmentationBitmask FlagMask)

newtype VmaDefragmentationMoveOperation = VmaDefragmentationMoveOperation Word32
  deriving (Eq, Ord, Enum, Storable)
pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_COPY :: VmaDefragmentationMoveOperation 
pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_COPY = VmaDefragmentationMoveOperation #{const VMA_DEFRAGMENTATION_MOVE_OPERATION_COPY }

pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_IGNORE :: VmaDefragmentationMoveOperation 
pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_IGNORE = VmaDefragmentationMoveOperation #{const VMA_DEFRAGMENTATION_MOVE_OPERATION_IGNORE }

pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_DESTROY :: VmaDefragmentationMoveOperation 
pattern VMA_DEFRAGMENTATION_MOVE_OPERATION_DESTROY = VmaDefragmentationMoveOperation #{const VMA_DEFRAGMENTATION_MOVE_OPERATION_DESTROY }

instance Show VmaDefragmentationMoveOperation where
  showsPrec _ VMA_DEFRAGMENTATION_MOVE_OPERATION_COPY = showString "VMA_DEFRAGMENTATION_MOVE_OPERATION_COPY"
  showsPrec _ VMA_DEFRAGMENTATION_MOVE_OPERATION_IGNORE = showString "VMA_DEFRAGMENTATION_MOVE_OPERATION_IGNORE"
  showsPrec _ VMA_DEFRAGMENTATION_MOVE_OPERATION_DESTROY = showString "VMA_DEFRAGMENTATION_MOVE_OPERATION_DESTROY"


type VmaVirtualBlockCreateFlagBits = VmaVirtualBlockCreateBitmask FlagBit
pattern VmaVirtualBlockCreateFlagBits :: VkFlags -> VmaVirtualBlockCreateBitmask FlagBit
pattern VmaVirtualBlockCreateFlagBits n = VmaVirtualBlockCreateBitmask n

pattern VMA_VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT :: VmaVirtualBlockCreateBitmask a
pattern VMA_VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT = VmaVirtualBlockCreateBitmask #{const VMA_VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT }

pattern VMA_VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK :: VmaVirtualBlockCreateBitmask a
pattern VMA_VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK = VmaVirtualBlockCreateBitmask #{const VMA_VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK }

pattern VMA_VIRTUAL_BLOCK_CREATE_FLAG_BITS_MAX_ENUM :: VmaVirtualBlockCreateBitmask a
pattern VMA_VIRTUAL_BLOCK_CREATE_FLAG_BITS_MAX_ENUM = VmaVirtualBlockCreateBitmask #{const VMA_VIRTUAL_BLOCK_CREATE_FLAG_BITS_MAX_ENUM }

instance Show (VmaVirtualBlockCreateBitmask a) where
  showsPrec _ VMA_VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT = showString "VMA_VIRTUAL_BLOCK_CREATE_LINEAR_ALGORITHM_BIT"
  showsPrec _ VMA_VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK = showString "VMA_VIRTUAL_BLOCK_CREATE_ALGORITHM_MASK"
  showsPrec _ VMA_VIRTUAL_BLOCK_CREATE_FLAG_BITS_MAX_ENUM = showString "VMA_VIRTUAL_BLOCK_CREATE_FLAG_BITS_MAX_ENUM"


newtype VmaVirtualBlockCreateBitmask (a :: FlagType) = VmaVirtualBlockCreateBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaVirtualBlockCreateFlags = VmaVirtualBlockCreateBitmask FlagMask
pattern VmaVirtualBlockCreateFlags :: VkFlags -> VmaVirtualBlockCreateBitmask FlagMask
pattern VmaVirtualBlockCreateFlags n = VmaVirtualBlockCreateBitmask n
deriving instance Bits (VmaVirtualBlockCreateBitmask FlagMask)
deriving instance FiniteBits (VmaVirtualBlockCreateBitmask FlagMask)

type VmaVirtualAllocationCreateFlagBits = VmaVirtualAllocationCreateBitmask FlagBit
pattern VmaVirtualAllocationCreateFlagBits :: VkFlags -> VmaVirtualAllocationCreateBitmask FlagBit
pattern VmaVirtualAllocationCreateFlagBits n = VmaVirtualAllocationCreateBitmask n

pattern VMA_VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT }

pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT }

pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT }

pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT }

pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK }

pattern VMA_VIRTUAL_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM :: VmaVirtualAllocationCreateBitmask a
pattern VMA_VIRTUAL_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM = VmaVirtualAllocationCreateBitmask #{const VMA_VIRTUAL_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM }

instance Show (VmaVirtualAllocationCreateBitmask a) where
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT = showString "VMA_VIRTUAL_ALLOCATION_CREATE_UPPER_ADDRESS_BIT"
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT = showString "VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_MEMORY_BIT"
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT = showString "VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_TIME_BIT"
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT = showString "VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MIN_OFFSET_BIT"
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK = showString "VMA_VIRTUAL_ALLOCATION_CREATE_STRATEGY_MASK"
  showsPrec _ VMA_VIRTUAL_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM = showString "VMA_VIRTUAL_ALLOCATION_CREATE_FLAG_BITS_MAX_ENUM"


newtype VmaVirtualAllocationCreateBitmask (a :: FlagType) = VmaVirtualAllocationCreateBitmask VkFlags
  deriving (Eq, Ord, Storable)

type VmaVirtualAllocationCreateFlags = VmaVirtualAllocationCreateBitmask FlagMask
pattern VmaVirtualAllocationCreateFlags :: VkFlags -> VmaVirtualAllocationCreateBitmask FlagMask
pattern VmaVirtualAllocationCreateFlags n = VmaVirtualAllocationCreateBitmask n
deriving instance Bits (VmaVirtualAllocationCreateBitmask FlagMask)
deriving instance FiniteBits (VmaVirtualAllocationCreateBitmask FlagMask)

data VmaAllocator_T
type VmaAllocator = Ptr VmaAllocator_T

data VmaPool_T
type VmaPool = Ptr VmaPool_T

data VmaAllocation_T
type VmaAllocation = Ptr VmaAllocation_T

data VmaDefragmentationContext_T
type VmaDefragmentationContext = Ptr VmaDefragmentationContext_T

data VmaVirtualAllocation_T
type VmaVirtualAllocation = VkPtr VmaVirtualAllocation_T

data VmaVirtualBlock_T
type VmaVirtualBlock = Ptr VmaVirtualBlock_T

type HS_vmaAllocateDeviceMemoryFunction = VmaAllocator
                                           -> Word32
                                           -> VkDeviceMemory
                                           -> VkDeviceSize
                                           -> (Ptr Void)
                                           -> IO ()

type PFN_vmaAllocateDeviceMemoryFunction = FunPtr HS_vmaAllocateDeviceMemoryFunction

type HS_vmaFreeDeviceMemoryFunction = VmaAllocator
                                       -> Word32
                                       -> VkDeviceMemory
                                       -> VkDeviceSize
                                       -> (Ptr Void)
                                       -> IO ()

type PFN_vmaFreeDeviceMemoryFunction = FunPtr HS_vmaFreeDeviceMemoryFunction

data VmaDeviceMemoryCallbacks'
type VmaDeviceMemoryCallbacks = VkStruct VmaDeviceMemoryCallbacks'

instance VulkanMarshal VmaDeviceMemoryCallbacks where
  type StructRep VmaDeviceMemoryCallbacks = 'StructMeta "VmaDeviceMemoryCallbacks" VmaDeviceMemoryCallbacks #{size VmaDeviceMemoryCallbacks } #{alignment VmaDeviceMemoryCallbacks}
    '[ ('FieldMeta "pfnAllocate" PFN_vmaAllocateDeviceMemoryFunction 'False #{offset VmaDeviceMemoryCallbacks, pfnAllocate} 1 'True 'True)
     , ('FieldMeta "pfnFree" PFN_vmaFreeDeviceMemoryFunction 'False #{offset VmaDeviceMemoryCallbacks, pfnFree} 1 'True 'True)
     , ('FieldMeta "pUserData" (Ptr Void) 'False #{offset VmaDeviceMemoryCallbacks, pUserData} 1 'True 'True)
     ] 'False 'False '[]

data VmaVulkanFunctions'
type VmaVulkanFunctions = VkStruct VmaVulkanFunctions'

instance VulkanMarshal VmaVulkanFunctions where
  type StructRep VmaVulkanFunctions = 'StructMeta "VmaVulkanFunctions" VmaVulkanFunctions #{size VmaVulkanFunctions } #{alignment VmaVulkanFunctions}
    '[ ('FieldMeta "vkGetInstanceProcAddr" PFN_vkGetInstanceProcAddr 'False #{offset VmaVulkanFunctions, vkGetInstanceProcAddr} 1 'True 'True)
     , ('FieldMeta "vkGetDeviceProcAddr" PFN_vkGetDeviceProcAddr 'False #{offset VmaVulkanFunctions, vkGetDeviceProcAddr} 1 'True 'True)
     , ('FieldMeta "vkGetPhysicalDeviceProperties" PFN_vkGetPhysicalDeviceProperties 'False #{offset VmaVulkanFunctions, vkGetPhysicalDeviceProperties} 1 'True 'True)
     , ('FieldMeta "vkGetPhysicalDeviceMemoryProperties" PFN_vkGetPhysicalDeviceMemoryProperties 'False #{offset VmaVulkanFunctions, vkGetPhysicalDeviceMemoryProperties} 1 'True 'True)
     , ('FieldMeta "vkAllocateMemory" PFN_vkAllocateMemory 'False #{offset VmaVulkanFunctions, vkAllocateMemory} 1 'True 'True)
     , ('FieldMeta "vkFreeMemory" PFN_vkFreeMemory 'False #{offset VmaVulkanFunctions, vkFreeMemory} 1 'True 'True)
     , ('FieldMeta "vkMapMemory" PFN_vkMapMemory 'False #{offset VmaVulkanFunctions, vkMapMemory} 1 'True 'True)
     , ('FieldMeta "vkUnmapMemory" PFN_vkUnmapMemory 'False #{offset VmaVulkanFunctions, vkUnmapMemory} 1 'True 'True)
     , ('FieldMeta "vkFlushMappedMemoryRanges" PFN_vkFlushMappedMemoryRanges 'False #{offset VmaVulkanFunctions, vkFlushMappedMemoryRanges} 1 'True 'True)
     , ('FieldMeta "vkInvalidateMappedMemoryRanges" PFN_vkInvalidateMappedMemoryRanges 'False #{offset VmaVulkanFunctions, vkInvalidateMappedMemoryRanges} 1 'True 'True)
     , ('FieldMeta "vkBindBufferMemory" PFN_vkBindBufferMemory 'False #{offset VmaVulkanFunctions, vkBindBufferMemory} 1 'True 'True)
     , ('FieldMeta "vkBindImageMemory" PFN_vkBindImageMemory 'False #{offset VmaVulkanFunctions, vkBindImageMemory} 1 'True 'True)
     , ('FieldMeta "vkGetBufferMemoryRequirements" PFN_vkGetBufferMemoryRequirements 'False #{offset VmaVulkanFunctions, vkGetBufferMemoryRequirements} 1 'True 'True)
     , ('FieldMeta "vkGetImageMemoryRequirements" PFN_vkGetImageMemoryRequirements 'False #{offset VmaVulkanFunctions, vkGetImageMemoryRequirements} 1 'True 'True)
     , ('FieldMeta "vkCreateBuffer" PFN_vkCreateBuffer 'False #{offset VmaVulkanFunctions, vkCreateBuffer} 1 'True 'True)
     , ('FieldMeta "vkDestroyBuffer" PFN_vkDestroyBuffer 'False #{offset VmaVulkanFunctions, vkDestroyBuffer} 1 'True 'True)
     , ('FieldMeta "vkCreateImage" PFN_vkCreateImage 'False #{offset VmaVulkanFunctions, vkCreateImage} 1 'True 'True)
     , ('FieldMeta "vkDestroyImage" PFN_vkDestroyImage 'False #{offset VmaVulkanFunctions, vkDestroyImage} 1 'True 'True)
     , ('FieldMeta "vkCmdCopyBuffer" PFN_vkCmdCopyBuffer 'False #{offset VmaVulkanFunctions, vkCmdCopyBuffer} 1 'True 'True)
#if VMA_DEDICATED_ALLOCATION || VMA_VULKAN_VERSION >= 1001000
     , ('FieldMeta "vkGetBufferMemoryRequirements2KHR" PFN_vkGetBufferMemoryRequirements2KHR 'False #{offset VmaVulkanFunctions, vkGetBufferMemoryRequirements2KHR} 1 'True 'True)
     , ('FieldMeta "vkGetImageMemoryRequirements2KHR" PFN_vkGetImageMemoryRequirements2KHR 'False #{offset VmaVulkanFunctions, vkGetImageMemoryRequirements2KHR} 1 'True 'True)
#endif
#if VMA_BIND_MEMORY2 || VMA_VULKAN_VERSION >= 1001000
     , ('FieldMeta "vkBindBufferMemory2KHR" PFN_vkBindBufferMemory2KHR 'False #{offset VmaVulkanFunctions, vkBindBufferMemory2KHR} 1 'True 'True)
     , ('FieldMeta "vkBindImageMemory2KHR" PFN_vkBindImageMemory2KHR 'False #{offset VmaVulkanFunctions, vkBindImageMemory2KHR} 1 'True 'True)
#endif
#if VMA_MEMORY_BUDGET || VMA_VULKAN_VERSION >= 1001000
     , ('FieldMeta "vkGetPhysicalDeviceMemoryProperties2KHR" PFN_vkGetPhysicalDeviceMemoryProperties2KHR 'False #{offset VmaVulkanFunctions, vkGetPhysicalDeviceMemoryProperties2KHR} 1 'True 'True)
#endif
#if VMA_VULKAN_VERSION >= 1003000
     , ('FieldMeta "vkGetDeviceBufferMemoryRequirements" PFN_vkGetDeviceBufferMemoryRequirements 'False #{offset VmaVulkanFunctions, vkGetDeviceBufferMemoryRequirements} 1 'True 'True)
     , ('FieldMeta "vkGetDeviceImageMemoryRequirements" PFN_vkGetDeviceImageMemoryRequirements 'False #{offset VmaVulkanFunctions, vkGetDeviceImageMemoryRequirements} 1 'True 'True)
#endif
     ] 'False 'False '[]

data VmaAllocatorCreateInfo'
type VmaAllocatorCreateInfo = VkStruct VmaAllocatorCreateInfo'

instance VulkanMarshal VmaAllocatorCreateInfo where
  type StructRep VmaAllocatorCreateInfo = 'StructMeta "VmaAllocatorCreateInfo" VmaAllocatorCreateInfo #{size VmaAllocatorCreateInfo } #{alignment VmaAllocatorCreateInfo}
    '[ ('FieldMeta "flags" VmaAllocatorCreateFlags 'False #{offset VmaAllocatorCreateInfo, flags} 1 'True 'True)
     , ('FieldMeta "physicalDevice" VkPhysicalDevice 'False #{offset VmaAllocatorCreateInfo, physicalDevice} 1 'True 'True)
     , ('FieldMeta "device" VkDevice 'False #{offset VmaAllocatorCreateInfo, device} 1 'True 'True)
     , ('FieldMeta "preferredLargeHeapBlockSize" VkDeviceSize 'False #{offset VmaAllocatorCreateInfo, preferredLargeHeapBlockSize} 1 'True 'True)
     , ('FieldMeta "pAllocationCallbacks" (Ptr VkAllocationCallbacks) 'False #{offset VmaAllocatorCreateInfo, pAllocationCallbacks} 1 'True 'True)
     , ('FieldMeta "pDeviceMemoryCallbacks" (Ptr VmaDeviceMemoryCallbacks) 'False #{offset VmaAllocatorCreateInfo, pDeviceMemoryCallbacks} 1 'True 'True)
     , ('FieldMeta "pHeapSizeLimit" (Ptr VkDeviceSize) 'False #{offset VmaAllocatorCreateInfo, pHeapSizeLimit} 1 'True 'True)
     , ('FieldMeta "pVulkanFunctions" (Ptr VmaVulkanFunctions) 'False #{offset VmaAllocatorCreateInfo, pVulkanFunctions} 1 'True 'True)
     , ('FieldMeta "instance" VkInstance 'False #{offset VmaAllocatorCreateInfo, instance} 1 'True 'True)
     , ('FieldMeta "vulkanApiVersion" Word32 'False #{offset VmaAllocatorCreateInfo, vulkanApiVersion} 1 'True 'True)
#if VMA_EXTERNAL_MEMORY
     , ('FieldMeta "pTypeExternalMemoryHandleTypes" (Ptr VkExternalMemoryHandleTypeFlagsKHR) 'False #{offset VmaAllocatorCreateInfo, pTypeExternalMemoryHandleTypes} 1 'True 'True)
#endif // #if VMA_EXTERNAL_MEMORY
     ] 'False 'False '[]

data VmaAllocatorInfo'
type VmaAllocatorInfo = VkStruct VmaAllocatorInfo'

instance VulkanMarshal VmaAllocatorInfo where
  type StructRep VmaAllocatorInfo = 'StructMeta "VmaAllocatorInfo" VmaAllocatorInfo #{size VmaAllocatorInfo } #{alignment VmaAllocatorInfo}
    '[ ('FieldMeta "instance" VkInstance 'False #{offset VmaAllocatorInfo, instance} 1 'True 'True)
     , ('FieldMeta "physicalDevice" VkPhysicalDevice 'False #{offset VmaAllocatorInfo, physicalDevice} 1 'True 'True)
     , ('FieldMeta "device" VkDevice 'False #{offset VmaAllocatorInfo, device} 1 'True 'True)
     ] 'False 'False '[]

data VmaStatistics'
type VmaStatistics = VkStruct VmaStatistics'

instance VulkanMarshal VmaStatistics where
  type StructRep VmaStatistics = 'StructMeta "VmaStatistics" VmaStatistics #{size VmaStatistics } #{alignment VmaStatistics}
    '[ ('FieldMeta "blockCount" Word32 'False #{offset VmaStatistics, blockCount} 1 'True 'True)
     , ('FieldMeta "allocationCount" Word32 'False #{offset VmaStatistics, allocationCount} 1 'True 'True)
     , ('FieldMeta "blockBytes" VkDeviceSize 'False #{offset VmaStatistics, blockBytes} 1 'True 'True)
     , ('FieldMeta "allocationBytes" VkDeviceSize 'False #{offset VmaStatistics, allocationBytes} 1 'True 'True)
     ] 'False 'False '[]

data VmaDetailedStatistics'
type VmaDetailedStatistics = VkStruct VmaDetailedStatistics'

instance VulkanMarshal VmaDetailedStatistics where
  type StructRep VmaDetailedStatistics = 'StructMeta "VmaDetailedStatistics" VmaDetailedStatistics #{size VmaDetailedStatistics } #{alignment VmaDetailedStatistics}
    '[ ('FieldMeta "statistics" VmaStatistics 'False #{offset VmaDetailedStatistics, statistics} 1 'True 'True)
     , ('FieldMeta "unusedRangeCount" Word32 'False #{offset VmaDetailedStatistics, unusedRangeCount} 1 'True 'True)
     , ('FieldMeta "allocationSizeMin" VkDeviceSize 'False #{offset VmaDetailedStatistics, allocationSizeMin} 1 'True 'True)
     , ('FieldMeta "allocationSizeMax" VkDeviceSize 'False #{offset VmaDetailedStatistics, allocationSizeMax} 1 'True 'True)
     , ('FieldMeta "unusedRangeSizeMin" VkDeviceSize 'False #{offset VmaDetailedStatistics, unusedRangeSizeMin} 1 'True 'True)
     , ('FieldMeta "unusedRangeSizeMax" VkDeviceSize 'False #{offset VmaDetailedStatistics, unusedRangeSizeMax} 1 'True 'True)
     ] 'False 'False '[]

data VmaTotalStatistics'
type VmaTotalStatistics = VkStruct VmaTotalStatistics'

instance VulkanMarshal VmaTotalStatistics where
  type StructRep VmaTotalStatistics = 'StructMeta "VmaTotalStatistics" VmaTotalStatistics #{size VmaTotalStatistics } #{alignment VmaTotalStatistics}
    '[ ('FieldMeta "memoryType" VmaDetailedStatistics 'False #{offset VmaTotalStatistics, memoryType} #{const VK_MAX_MEMORY_TYPES} 'True 'True)
     , ('FieldMeta "memoryHeap" VmaDetailedStatistics 'False #{offset VmaTotalStatistics, memoryHeap} #{const VK_MAX_MEMORY_HEAPS} 'True 'True)
     , ('FieldMeta "total" VmaDetailedStatistics 'False #{offset VmaTotalStatistics, total} 1 'True 'True)
     ] 'False 'False '[]

data VmaBudget'
type VmaBudget = VkStruct VmaBudget'

instance VulkanMarshal VmaBudget where
  type StructRep VmaBudget = 'StructMeta "VmaBudget" VmaBudget #{size VmaBudget } #{alignment VmaBudget}
    '[ ('FieldMeta "statistics" VmaStatistics 'False #{offset VmaBudget, statistics} 1 'True 'True)
     , ('FieldMeta "usage" VkDeviceSize 'False #{offset VmaBudget, usage} 1 'True 'True)
     , ('FieldMeta "budget" VkDeviceSize 'False #{offset VmaBudget, budget} 1 'True 'True)
     ] 'False 'False '[]

data VmaAllocationCreateInfo'
type VmaAllocationCreateInfo = VkStruct VmaAllocationCreateInfo'

instance VulkanMarshal VmaAllocationCreateInfo where
  type StructRep VmaAllocationCreateInfo = 'StructMeta "VmaAllocationCreateInfo" VmaAllocationCreateInfo #{size VmaAllocationCreateInfo } #{alignment VmaAllocationCreateInfo}
    '[ ('FieldMeta "flags" VmaAllocationCreateFlags 'False #{offset VmaAllocationCreateInfo, flags} 1 'True 'True)
     , ('FieldMeta "usage" VmaMemoryUsage 'False #{offset VmaAllocationCreateInfo, usage} 1 'True 'True)
     , ('FieldMeta "requiredFlags" VkMemoryPropertyFlags 'False #{offset VmaAllocationCreateInfo, requiredFlags} 1 'True 'True)
     , ('FieldMeta "preferredFlags" VkMemoryPropertyFlags 'False #{offset VmaAllocationCreateInfo, preferredFlags} 1 'True 'True)
     , ('FieldMeta "memoryTypeBits" Word32 'False #{offset VmaAllocationCreateInfo, memoryTypeBits} 1 'True 'True)
     , ('FieldMeta "pool" VmaPool 'False #{offset VmaAllocationCreateInfo, pool} 1 'True 'True)
     , ('FieldMeta "pUserData" (Ptr Void) 'False #{offset VmaAllocationCreateInfo, pUserData} 1 'True 'True)
     , ('FieldMeta "priority" Float 'False #{offset VmaAllocationCreateInfo, priority} 1 'True 'True)
     ] 'False 'False '[]

data VmaPoolCreateInfo'
type VmaPoolCreateInfo = VkStruct VmaPoolCreateInfo'

instance VulkanMarshal VmaPoolCreateInfo where
  type StructRep VmaPoolCreateInfo = 'StructMeta "VmaPoolCreateInfo" VmaPoolCreateInfo #{size VmaPoolCreateInfo } #{alignment VmaPoolCreateInfo}
    '[ ('FieldMeta "memoryTypeIndex" Word32 'False #{offset VmaPoolCreateInfo, memoryTypeIndex} 1 'True 'True)
     , ('FieldMeta "flags" VmaPoolCreateFlags 'False #{offset VmaPoolCreateInfo, flags} 1 'True 'True)
     , ('FieldMeta "blockSize" VkDeviceSize 'False #{offset VmaPoolCreateInfo, blockSize} 1 'True 'True)
     , ('FieldMeta "minBlockCount" CSize 'False #{offset VmaPoolCreateInfo, minBlockCount} 1 'True 'True)
     , ('FieldMeta "maxBlockCount" CSize 'False #{offset VmaPoolCreateInfo, maxBlockCount} 1 'True 'True)
     , ('FieldMeta "priority" Float 'False #{offset VmaPoolCreateInfo, priority} 1 'True 'True)
     , ('FieldMeta "minAllocationAlignment" VkDeviceSize 'False #{offset VmaPoolCreateInfo, minAllocationAlignment} 1 'True 'True)
     , ('FieldMeta "pMemoryAllocateNext" (Ptr Void) 'False #{offset VmaPoolCreateInfo, pMemoryAllocateNext} 1 'True 'True)
     ] 'False 'False '[]

data VmaAllocationInfo'
type VmaAllocationInfo = VkStruct VmaAllocationInfo'

instance VulkanMarshal VmaAllocationInfo where
  type StructRep VmaAllocationInfo = 'StructMeta "VmaAllocationInfo" VmaAllocationInfo #{size VmaAllocationInfo } #{alignment VmaAllocationInfo}
    '[ ('FieldMeta "memoryType" Word32 'False #{offset VmaAllocationInfo, memoryType} 1 'True 'True)
     , ('FieldMeta "deviceMemory" VkDeviceMemory 'False #{offset VmaAllocationInfo, deviceMemory} 1 'True 'True)
     , ('FieldMeta "offset" VkDeviceSize 'False #{offset VmaAllocationInfo, offset} 1 'True 'True)
     , ('FieldMeta "size" VkDeviceSize 'False #{offset VmaAllocationInfo, size} 1 'True 'True)
     , ('FieldMeta "pMappedData" (Ptr Void) 'False #{offset VmaAllocationInfo, pMappedData} 1 'True 'True)
     , ('FieldMeta "pUserData" (Ptr Void) 'False #{offset VmaAllocationInfo, pUserData} 1 'True 'True)
     , ('FieldMeta "pName" CString 'False #{offset VmaAllocationInfo, pName} 1 'True 'True)
     ] 'False 'False '[]

data VmaDefragmentationInfo'
type VmaDefragmentationInfo = VkStruct VmaDefragmentationInfo'

instance VulkanMarshal VmaDefragmentationInfo where
  type StructRep VmaDefragmentationInfo = 'StructMeta "VmaDefragmentationInfo" VmaDefragmentationInfo #{size VmaDefragmentationInfo } #{alignment VmaDefragmentationInfo}
    '[ ('FieldMeta "flags" VmaDefragmentationFlags 'False #{offset VmaDefragmentationInfo, flags} 1 'True 'True)
     , ('FieldMeta "pool" VmaPool 'False #{offset VmaDefragmentationInfo, pool} 1 'True 'True)
     , ('FieldMeta "maxBytesPerPass" VkDeviceSize 'False #{offset VmaDefragmentationInfo, maxBytesPerPass} 1 'True 'True)
     , ('FieldMeta "maxAllocationsPerPass" Word32 'False #{offset VmaDefragmentationInfo, maxAllocationsPerPass} 1 'True 'True)
     ] 'False 'False '[]

data VmaDefragmentationMove'
type VmaDefragmentationMove = VkStruct VmaDefragmentationMove'

instance VulkanMarshal VmaDefragmentationMove where
  type StructRep VmaDefragmentationMove = 'StructMeta "VmaDefragmentationMove" VmaDefragmentationMove #{size VmaDefragmentationMove } #{alignment VmaDefragmentationMove}
    '[ ('FieldMeta "operation" VmaDefragmentationMoveOperation 'False #{offset VmaDefragmentationMove, operation} 1 'True 'True)
     , ('FieldMeta "srcAllocation" VmaAllocation 'False #{offset VmaDefragmentationMove, srcAllocation} 1 'True 'True)
     , ('FieldMeta "dstTmpAllocation" VmaAllocation 'False #{offset VmaDefragmentationMove, dstTmpAllocation} 1 'True 'True)
     ] 'False 'False '[]

data VmaDefragmentationPassMoveInfo'
type VmaDefragmentationPassMoveInfo = VkStruct VmaDefragmentationPassMoveInfo'

instance VulkanMarshal VmaDefragmentationPassMoveInfo where
  type StructRep VmaDefragmentationPassMoveInfo = 'StructMeta "VmaDefragmentationPassMoveInfo" VmaDefragmentationPassMoveInfo #{size VmaDefragmentationPassMoveInfo } #{alignment VmaDefragmentationPassMoveInfo}
    '[ ('FieldMeta "moveCount" Word32 'False #{offset VmaDefragmentationPassMoveInfo, moveCount} 1 'True 'True)
     , ('FieldMeta "pMoves" (Ptr VmaDefragmentationMove) 'False #{offset VmaDefragmentationPassMoveInfo, pMoves} 1 'True 'True)
     ] 'False 'False '[]

data VmaDefragmentationStats'
type VmaDefragmentationStats = VkStruct VmaDefragmentationStats'

instance VulkanMarshal VmaDefragmentationStats where
  type StructRep VmaDefragmentationStats = 'StructMeta "VmaDefragmentationStats" VmaDefragmentationStats #{size VmaDefragmentationStats } #{alignment VmaDefragmentationStats}
    '[ ('FieldMeta "bytesMoved" VkDeviceSize 'False #{offset VmaDefragmentationStats, bytesMoved} 1 'True 'True)
     , ('FieldMeta "bytesFreed" VkDeviceSize 'False #{offset VmaDefragmentationStats, bytesFreed} 1 'True 'True)
     , ('FieldMeta "allocationsMoved" Word32 'False #{offset VmaDefragmentationStats, allocationsMoved} 1 'True 'True)
     , ('FieldMeta "deviceMemoryBlocksFreed" Word32 'False #{offset VmaDefragmentationStats, deviceMemoryBlocksFreed} 1 'True 'True)
     ] 'False 'False '[]

data VmaVirtualBlockCreateInfo'
type VmaVirtualBlockCreateInfo = VkStruct VmaVirtualBlockCreateInfo'

instance VulkanMarshal VmaVirtualBlockCreateInfo where
  type StructRep VmaVirtualBlockCreateInfo = 'StructMeta "VmaVirtualBlockCreateInfo" VmaVirtualBlockCreateInfo #{size VmaVirtualBlockCreateInfo } #{alignment VmaVirtualBlockCreateInfo}
    '[ ('FieldMeta "size" VkDeviceSize 'False #{offset VmaVirtualBlockCreateInfo, size} 1 'True 'True)
     , ('FieldMeta "flags" VmaVirtualBlockCreateFlags 'False #{offset VmaVirtualBlockCreateInfo, flags} 1 'True 'True)
     , ('FieldMeta "pAllocationCallbacks" (Ptr VkAllocationCallbacks) 'False #{offset VmaVirtualBlockCreateInfo, pAllocationCallbacks} 1 'True 'True)
     ] 'False 'False '[]

data VmaVirtualAllocationCreateInfo'
type VmaVirtualAllocationCreateInfo = VkStruct VmaVirtualAllocationCreateInfo'

instance VulkanMarshal VmaVirtualAllocationCreateInfo where
  type StructRep VmaVirtualAllocationCreateInfo = 'StructMeta "VmaVirtualAllocationCreateInfo" VmaVirtualAllocationCreateInfo #{size VmaVirtualAllocationCreateInfo } #{alignment VmaVirtualAllocationCreateInfo}
    '[ ('FieldMeta "size" VkDeviceSize 'False #{offset VmaVirtualAllocationCreateInfo, size} 1 'True 'True)
     , ('FieldMeta "alignment" VkDeviceSize 'False #{offset VmaVirtualAllocationCreateInfo, alignment} 1 'True 'True)
     , ('FieldMeta "flags" VmaVirtualAllocationCreateFlags 'False #{offset VmaVirtualAllocationCreateInfo, flags} 1 'True 'True)
     , ('FieldMeta "pUserData" (Ptr Void) 'False #{offset VmaVirtualAllocationCreateInfo, pUserData} 1 'True 'True)
     ] 'False 'False '[]

data VmaVirtualAllocationInfo'
type VmaVirtualAllocationInfo = VkStruct VmaVirtualAllocationInfo'

instance VulkanMarshal VmaVirtualAllocationInfo where
  type StructRep VmaVirtualAllocationInfo = 'StructMeta "VmaVirtualAllocationInfo" VmaVirtualAllocationInfo #{size VmaVirtualAllocationInfo } #{alignment VmaVirtualAllocationInfo}
    '[ ('FieldMeta "offset" VkDeviceSize 'False #{offset VmaVirtualAllocationInfo, offset} 1 'True 'True)
     , ('FieldMeta "size" VkDeviceSize 'False #{offset VmaVirtualAllocationInfo, size} 1 'True 'True)
     , ('FieldMeta "pUserData" (Ptr Void) 'False #{offset VmaVirtualAllocationInfo, pUserData} 1 'True 'True)
     ] 'False 'False '[]

foreign import ccall unsafe "vmaCreateAllocator"
  vmaCreateAllocator :: (Ptr VmaAllocatorCreateInfo)
                     -> (Ptr VmaAllocator)
                     -> IO VkResult

type HS_vmaCreateAllocator = (Ptr VmaAllocatorCreateInfo)
                     -> (Ptr VmaAllocator)
                     -> IO VkResult

type PFN_vmaCreateAllocator = FunPtr HS_vmaCreateAllocator

foreign import ccall unsafe "vmaDestroyAllocator"
  vmaDestroyAllocator :: VmaAllocator
                      -> IO ()

type HS_vmaDestroyAllocator = VmaAllocator
                      -> IO ()

type PFN_vmaDestroyAllocator = FunPtr HS_vmaDestroyAllocator

foreign import ccall unsafe "vmaGetAllocatorInfo"
  vmaGetAllocatorInfo :: VmaAllocator
                      -> (Ptr VmaAllocatorInfo)
                      -> IO ()

type HS_vmaGetAllocatorInfo = VmaAllocator
                      -> (Ptr VmaAllocatorInfo)
                      -> IO ()

type PFN_vmaGetAllocatorInfo = FunPtr HS_vmaGetAllocatorInfo

foreign import ccall unsafe "vmaGetPhysicalDeviceProperties"
  vmaGetPhysicalDeviceProperties :: VmaAllocator
                                 -> (Ptr VkPhysicalDeviceProperties)
                                 -> IO ()

type HS_vmaGetPhysicalDeviceProperties = VmaAllocator
                                 -> (Ptr VkPhysicalDeviceProperties)
                                 -> IO ()

type PFN_vmaGetPhysicalDeviceProperties = FunPtr HS_vmaGetPhysicalDeviceProperties

foreign import ccall unsafe "vmaGetMemoryProperties"
  vmaGetMemoryProperties :: VmaAllocator
                         -> (Ptr VkPhysicalDeviceMemoryProperties)
                         -> IO ()

type HS_vmaGetMemoryProperties = VmaAllocator
                         -> (Ptr VkPhysicalDeviceMemoryProperties)
                         -> IO ()

type PFN_vmaGetMemoryProperties = FunPtr HS_vmaGetMemoryProperties

foreign import ccall unsafe "vmaGetMemoryTypeProperties"
  vmaGetMemoryTypeProperties :: VmaAllocator
                             -> Word32
                             -> (Ptr VkMemoryPropertyFlags)
                             -> IO ()

type HS_vmaGetMemoryTypeProperties = VmaAllocator
                             -> Word32
                             -> (Ptr VkMemoryPropertyFlags)
                             -> IO ()

type PFN_vmaGetMemoryTypeProperties = FunPtr HS_vmaGetMemoryTypeProperties

foreign import ccall unsafe "vmaSetCurrentFrameIndex"
  vmaSetCurrentFrameIndex :: VmaAllocator
                          -> Word32
                          -> IO ()

type HS_vmaSetCurrentFrameIndex = VmaAllocator
                          -> Word32
                          -> IO ()

type PFN_vmaSetCurrentFrameIndex = FunPtr HS_vmaSetCurrentFrameIndex

foreign import ccall unsafe "vmaCalculateStatistics"
  vmaCalculateStatistics :: VmaAllocator
                         -> (Ptr VmaTotalStatistics)
                         -> IO ()

type HS_vmaCalculateStatistics = VmaAllocator
                         -> (Ptr VmaTotalStatistics)
                         -> IO ()

type PFN_vmaCalculateStatistics = FunPtr HS_vmaCalculateStatistics

foreign import ccall unsafe "vmaGetHeapBudgets"
  vmaGetHeapBudgets :: VmaAllocator
                    -> (Ptr VmaBudget)
                    -> IO ()

type HS_vmaGetHeapBudgets = VmaAllocator
                    -> (Ptr VmaBudget)
                    -> IO ()

type PFN_vmaGetHeapBudgets = FunPtr HS_vmaGetHeapBudgets

foreign import ccall unsafe "vmaFindMemoryTypeIndex"
  vmaFindMemoryTypeIndex :: VmaAllocator
                         -> Word32
                         -> (Ptr VmaAllocationCreateInfo)
                         -> Word32
                         -> IO VkResult

type HS_vmaFindMemoryTypeIndex = VmaAllocator
                         -> Word32
                         -> (Ptr VmaAllocationCreateInfo)
                         -> Word32
                         -> IO VkResult

type PFN_vmaFindMemoryTypeIndex = FunPtr HS_vmaFindMemoryTypeIndex

foreign import ccall unsafe "vmaFindMemoryTypeIndexForBufferInfo"
  vmaFindMemoryTypeIndexForBufferInfo :: VmaAllocator
                                      -> (Ptr VkBufferCreateInfo)
                                      -> (Ptr VmaAllocationCreateInfo)
                                      -> Word32
                                      -> IO VkResult

type HS_vmaFindMemoryTypeIndexForBufferInfo = VmaAllocator
                                      -> (Ptr VkBufferCreateInfo)
                                      -> (Ptr VmaAllocationCreateInfo)
                                      -> Word32
                                      -> IO VkResult

type PFN_vmaFindMemoryTypeIndexForBufferInfo = FunPtr HS_vmaFindMemoryTypeIndexForBufferInfo

foreign import ccall unsafe "vmaFindMemoryTypeIndexForImageInfo"
  vmaFindMemoryTypeIndexForImageInfo :: VmaAllocator
                                     -> (Ptr VkImageCreateInfo)
                                     -> (Ptr VmaAllocationCreateInfo)
                                     -> Word32
                                     -> IO VkResult

type HS_vmaFindMemoryTypeIndexForImageInfo = VmaAllocator
                                     -> (Ptr VkImageCreateInfo)
                                     -> (Ptr VmaAllocationCreateInfo)
                                     -> Word32
                                     -> IO VkResult

type PFN_vmaFindMemoryTypeIndexForImageInfo = FunPtr HS_vmaFindMemoryTypeIndexForImageInfo

foreign import ccall unsafe "vmaCreatePool"
  vmaCreatePool :: VmaAllocator
                -> (Ptr VmaPoolCreateInfo)
                -> (Ptr VmaPool)
                -> IO VkResult

type HS_vmaCreatePool = VmaAllocator
                -> (Ptr VmaPoolCreateInfo)
                -> (Ptr VmaPool)
                -> IO VkResult

type PFN_vmaCreatePool = FunPtr HS_vmaCreatePool

foreign import ccall unsafe "vmaDestroyPool"
  vmaDestroyPool :: VmaAllocator
                 -> VmaPool
                 -> IO ()

type HS_vmaDestroyPool = VmaAllocator
                 -> VmaPool
                 -> IO ()

type PFN_vmaDestroyPool = FunPtr HS_vmaDestroyPool

foreign import ccall unsafe "vmaGetPoolStatistics"
  vmaGetPoolStatistics :: VmaAllocator
                       -> VmaPool
                       -> (Ptr VmaStatistics)
                       -> IO ()

type HS_vmaGetPoolStatistics = VmaAllocator
                       -> VmaPool
                       -> (Ptr VmaStatistics)
                       -> IO ()

type PFN_vmaGetPoolStatistics = FunPtr HS_vmaGetPoolStatistics

foreign import ccall unsafe "vmaCalculatePoolStatistics"
  vmaCalculatePoolStatistics :: VmaAllocator
                             -> VmaPool
                             -> (Ptr VmaDetailedStatistics)
                             -> IO ()

type HS_vmaCalculatePoolStatistics = VmaAllocator
                             -> VmaPool
                             -> (Ptr VmaDetailedStatistics)
                             -> IO ()

type PFN_vmaCalculatePoolStatistics = FunPtr HS_vmaCalculatePoolStatistics

foreign import ccall unsafe "vmaCheckPoolCorruption"
  vmaCheckPoolCorruption :: VmaAllocator
                         -> VmaPool
                         -> IO VkResult

type HS_vmaCheckPoolCorruption = VmaAllocator
                         -> VmaPool
                         -> IO VkResult

type PFN_vmaCheckPoolCorruption = FunPtr HS_vmaCheckPoolCorruption

foreign import ccall unsafe "vmaGetPoolName"
  vmaGetPoolName :: VmaAllocator
                 -> VmaPool
                 -> CString
                 -> IO ()

type HS_vmaGetPoolName = VmaAllocator
                 -> VmaPool
                 -> CString
                 -> IO ()

type PFN_vmaGetPoolName = FunPtr HS_vmaGetPoolName

foreign import ccall unsafe "vmaSetPoolName"
  vmaSetPoolName :: VmaAllocator
                 -> VmaPool
                 -> CString
                 -> IO ()

type HS_vmaSetPoolName = VmaAllocator
                 -> VmaPool
                 -> CString
                 -> IO ()

type PFN_vmaSetPoolName = FunPtr HS_vmaSetPoolName

foreign import ccall unsafe "vmaAllocateMemory"
  vmaAllocateMemory :: VmaAllocator
                    -> (Ptr VkMemoryRequirements)
                    -> (Ptr VmaAllocationCreateInfo)
                    -> (Ptr VmaAllocation)
                    -> (Ptr VmaAllocationInfo)
                    -> IO VkResult

type HS_vmaAllocateMemory = VmaAllocator
                    -> (Ptr VkMemoryRequirements)
                    -> (Ptr VmaAllocationCreateInfo)
                    -> (Ptr VmaAllocation)
                    -> (Ptr VmaAllocationInfo)
                    -> IO VkResult

type PFN_vmaAllocateMemory = FunPtr HS_vmaAllocateMemory

foreign import ccall unsafe "vmaAllocateMemoryPages"
  vmaAllocateMemoryPages :: VmaAllocator
                         -> (Ptr VkMemoryRequirements)
                         -> IO VkResult

type HS_vmaAllocateMemoryPages = VmaAllocator
                         -> (Ptr VkMemoryRequirements)
                         -> IO VkResult

type PFN_vmaAllocateMemoryPages = FunPtr HS_vmaAllocateMemoryPages

foreign import ccall unsafe "vmaAllocateMemoryForBuffer"
  vmaAllocateMemoryForBuffer :: VmaAllocator
                             -> VkBuffer
                             -> (Ptr VmaAllocationCreateInfo)
                             -> (Ptr VmaAllocation)
                             -> (Ptr VmaAllocationInfo)
                             -> IO VkResult

type HS_vmaAllocateMemoryForBuffer = VmaAllocator
                             -> VkBuffer
                             -> (Ptr VmaAllocationCreateInfo)
                             -> (Ptr VmaAllocation)
                             -> (Ptr VmaAllocationInfo)
                             -> IO VkResult

type PFN_vmaAllocateMemoryForBuffer = FunPtr HS_vmaAllocateMemoryForBuffer

foreign import ccall unsafe "vmaAllocateMemoryForImage"
  vmaAllocateMemoryForImage :: VmaAllocator
                            -> VkImage
                            -> (Ptr VmaAllocationCreateInfo)
                            -> (Ptr VmaAllocation)
                            -> (Ptr VmaAllocationInfo)
                            -> IO VkResult

type HS_vmaAllocateMemoryForImage = VmaAllocator
                            -> VkImage
                            -> (Ptr VmaAllocationCreateInfo)
                            -> (Ptr VmaAllocation)
                            -> (Ptr VmaAllocationInfo)
                            -> IO VkResult

type PFN_vmaAllocateMemoryForImage = FunPtr HS_vmaAllocateMemoryForImage

foreign import ccall unsafe "vmaFreeMemory"
  vmaFreeMemory :: VmaAllocator
                -> VmaAllocation
                -> IO ()

type HS_vmaFreeMemory = VmaAllocator
                -> VmaAllocation
                -> IO ()

type PFN_vmaFreeMemory = FunPtr HS_vmaFreeMemory

foreign import ccall unsafe "vmaFreeMemoryPages"
  vmaFreeMemoryPages :: VmaAllocator
                     -> CSize
                     -> (Ptr VmaAllocation)
                     -> IO ()

type HS_vmaFreeMemoryPages = VmaAllocator
                     -> CSize
                     -> (Ptr VmaAllocation)
                     -> IO ()

type PFN_vmaFreeMemoryPages = FunPtr HS_vmaFreeMemoryPages

foreign import ccall unsafe "vmaGetAllocationInfo"
  vmaGetAllocationInfo :: VmaAllocator
                       -> VmaAllocation
                       -> (Ptr VmaAllocationInfo)
                       -> IO ()

type HS_vmaGetAllocationInfo = VmaAllocator
                       -> VmaAllocation
                       -> (Ptr VmaAllocationInfo)
                       -> IO ()

type PFN_vmaGetAllocationInfo = FunPtr HS_vmaGetAllocationInfo

foreign import ccall unsafe "vmaSetAllocationUserData"
  vmaSetAllocationUserData :: VmaAllocator
                           -> VmaAllocation
                           -> (Ptr Void)
                           -> IO ()

type HS_vmaSetAllocationUserData = VmaAllocator
                           -> VmaAllocation
                           -> (Ptr Void)
                           -> IO ()

type PFN_vmaSetAllocationUserData = FunPtr HS_vmaSetAllocationUserData

foreign import ccall unsafe "vmaSetAllocationName"
  vmaSetAllocationName :: VmaAllocator
                       -> VmaAllocation
                       -> CString
                       -> IO ()

type HS_vmaSetAllocationName = VmaAllocator
                       -> VmaAllocation
                       -> CString
                       -> IO ()

type PFN_vmaSetAllocationName = FunPtr HS_vmaSetAllocationName

foreign import ccall unsafe "vmaGetAllocationMemoryProperties"
  vmaGetAllocationMemoryProperties :: VmaAllocator
                                   -> VmaAllocation
                                   -> (Ptr VkMemoryPropertyFlags)
                                   -> IO ()

type HS_vmaGetAllocationMemoryProperties = VmaAllocator
                                   -> VmaAllocation
                                   -> (Ptr VkMemoryPropertyFlags)
                                   -> IO ()

type PFN_vmaGetAllocationMemoryProperties = FunPtr HS_vmaGetAllocationMemoryProperties

foreign import ccall unsafe "vmaMapMemory"
  vmaMapMemory :: VmaAllocator
               -> VmaAllocation
               -> (Ptr (Ptr Void))
               -> IO VkResult

type HS_vmaMapMemory = VmaAllocator
               -> VmaAllocation
               -> (Ptr (Ptr Void))
               -> IO VkResult

type PFN_vmaMapMemory = FunPtr HS_vmaMapMemory

foreign import ccall unsafe "vmaUnmapMemory"
  vmaUnmapMemory :: VmaAllocator
                 -> VmaAllocation
                 -> IO ()

type HS_vmaUnmapMemory = VmaAllocator
                 -> VmaAllocation
                 -> IO ()

type PFN_vmaUnmapMemory = FunPtr HS_vmaUnmapMemory

foreign import ccall unsafe "vmaFlushAllocation"
  vmaFlushAllocation :: VmaAllocator
                     -> VmaAllocation
                     -> VkDeviceSize
                     -> VkDeviceSize
                     -> IO VkResult

type HS_vmaFlushAllocation = VmaAllocator
                     -> VmaAllocation
                     -> VkDeviceSize
                     -> VkDeviceSize
                     -> IO VkResult

type PFN_vmaFlushAllocation = FunPtr HS_vmaFlushAllocation

foreign import ccall unsafe "vmaInvalidateAllocation"
  vmaInvalidateAllocation :: VmaAllocator
                          -> VmaAllocation
                          -> VkDeviceSize
                          -> VkDeviceSize
                          -> IO VkResult

type HS_vmaInvalidateAllocation = VmaAllocator
                          -> VmaAllocation
                          -> VkDeviceSize
                          -> VkDeviceSize
                          -> IO VkResult

type PFN_vmaInvalidateAllocation = FunPtr HS_vmaInvalidateAllocation

foreign import ccall unsafe "vmaFlushAllocations"
  vmaFlushAllocations :: VmaAllocator
                      -> Word32
                      -> (Ptr VmaAllocation)
                      -> IO VkResult

type HS_vmaFlushAllocations = VmaAllocator
                      -> Word32
                      -> (Ptr VmaAllocation)
                      -> IO VkResult

type PFN_vmaFlushAllocations = FunPtr HS_vmaFlushAllocations

foreign import ccall unsafe "vmaInvalidateAllocations"
  vmaInvalidateAllocations :: VmaAllocator
                           -> Word32
                           -> (Ptr VmaAllocation)
                           -> IO VkResult

type HS_vmaInvalidateAllocations = VmaAllocator
                           -> Word32
                           -> (Ptr VmaAllocation)
                           -> IO VkResult

type PFN_vmaInvalidateAllocations = FunPtr HS_vmaInvalidateAllocations

foreign import ccall unsafe "vmaCheckCorruption"
  vmaCheckCorruption :: VmaAllocator
                     -> Word32
                     -> IO VkResult

type HS_vmaCheckCorruption = VmaAllocator
                     -> Word32
                     -> IO VkResult

type PFN_vmaCheckCorruption = FunPtr HS_vmaCheckCorruption

foreign import ccall unsafe "vmaBeginDefragmentation"
  vmaBeginDefragmentation :: VmaAllocator
                          -> (Ptr VmaDefragmentationInfo)
                          -> (Ptr VmaDefragmentationContext)
                          -> IO VkResult

type HS_vmaBeginDefragmentation = VmaAllocator
                          -> (Ptr VmaDefragmentationInfo)
                          -> (Ptr VmaDefragmentationContext)
                          -> IO VkResult

type PFN_vmaBeginDefragmentation = FunPtr HS_vmaBeginDefragmentation

foreign import ccall unsafe "vmaEndDefragmentation"
  vmaEndDefragmentation :: VmaAllocator
                        -> VmaDefragmentationContext
                        -> (Ptr VmaDefragmentationStats)
                        -> IO ()

type HS_vmaEndDefragmentation = VmaAllocator
                        -> VmaDefragmentationContext
                        -> (Ptr VmaDefragmentationStats)
                        -> IO ()

type PFN_vmaEndDefragmentation = FunPtr HS_vmaEndDefragmentation

foreign import ccall unsafe "vmaBeginDefragmentationPass"
  vmaBeginDefragmentationPass :: VmaAllocator
                              -> VmaDefragmentationContext
                              -> (Ptr VmaDefragmentationPassMoveInfo)
                              -> IO VkResult

type HS_vmaBeginDefragmentationPass = VmaAllocator
                              -> VmaDefragmentationContext
                              -> (Ptr VmaDefragmentationPassMoveInfo)
                              -> IO VkResult

type PFN_vmaBeginDefragmentationPass = FunPtr HS_vmaBeginDefragmentationPass

foreign import ccall unsafe "vmaEndDefragmentationPass"
  vmaEndDefragmentationPass :: VmaAllocator
                            -> VmaDefragmentationContext
                            -> (Ptr VmaDefragmentationPassMoveInfo)
                            -> IO VkResult

type HS_vmaEndDefragmentationPass = VmaAllocator
                            -> VmaDefragmentationContext
                            -> (Ptr VmaDefragmentationPassMoveInfo)
                            -> IO VkResult

type PFN_vmaEndDefragmentationPass = FunPtr HS_vmaEndDefragmentationPass

foreign import ccall unsafe "vmaBindBufferMemory"
  vmaBindBufferMemory :: VmaAllocator
                      -> VmaAllocation
                      -> VkBuffer
                      -> IO VkResult

type HS_vmaBindBufferMemory = VmaAllocator
                      -> VmaAllocation
                      -> VkBuffer
                      -> IO VkResult

type PFN_vmaBindBufferMemory = FunPtr HS_vmaBindBufferMemory

foreign import ccall unsafe "vmaBindBufferMemory2"
  vmaBindBufferMemory2 :: VmaAllocator
                       -> VmaAllocation
                       -> VkDeviceSize
                       -> VkBuffer
                       -> (Ptr Void)
                       -> IO VkResult

type HS_vmaBindBufferMemory2 = VmaAllocator
                       -> VmaAllocation
                       -> VkDeviceSize
                       -> VkBuffer
                       -> (Ptr Void)
                       -> IO VkResult

type PFN_vmaBindBufferMemory2 = FunPtr HS_vmaBindBufferMemory2

foreign import ccall unsafe "vmaBindImageMemory"
  vmaBindImageMemory :: VmaAllocator
                     -> VmaAllocation
                     -> VkImage
                     -> IO VkResult

type HS_vmaBindImageMemory = VmaAllocator
                     -> VmaAllocation
                     -> VkImage
                     -> IO VkResult

type PFN_vmaBindImageMemory = FunPtr HS_vmaBindImageMemory

foreign import ccall unsafe "vmaBindImageMemory2"
  vmaBindImageMemory2 :: VmaAllocator
                      -> VmaAllocation
                      -> VkDeviceSize
                      -> VkImage
                      -> (Ptr Void)
                      -> IO VkResult

type HS_vmaBindImageMemory2 = VmaAllocator
                      -> VmaAllocation
                      -> VkDeviceSize
                      -> VkImage
                      -> (Ptr Void)
                      -> IO VkResult

type PFN_vmaBindImageMemory2 = FunPtr HS_vmaBindImageMemory2

foreign import ccall unsafe "vmaCreateBuffer"
  vmaCreateBuffer :: VmaAllocator
                  -> (Ptr VkBufferCreateInfo)
                  -> (Ptr VmaAllocationCreateInfo)
                  -> (Ptr VkBuffer)
                  -> (Ptr VmaAllocation)
                  -> (Ptr VmaAllocationInfo)
                  -> IO VkResult

type HS_vmaCreateBuffer = VmaAllocator
                  -> (Ptr VkBufferCreateInfo)
                  -> (Ptr VmaAllocationCreateInfo)
                  -> (Ptr VkBuffer)
                  -> (Ptr VmaAllocation)
                  -> (Ptr VmaAllocationInfo)
                  -> IO VkResult

type PFN_vmaCreateBuffer = FunPtr HS_vmaCreateBuffer

foreign import ccall unsafe "vmaCreateBufferWithAlignment"
  vmaCreateBufferWithAlignment :: VmaAllocator
                               -> (Ptr VkBufferCreateInfo)
                               -> (Ptr VmaAllocationCreateInfo)
                               -> VkDeviceSize
                               -> (Ptr VkBuffer)
                               -> (Ptr VmaAllocation)
                               -> (Ptr VmaAllocationInfo)
                               -> IO VkResult

type HS_vmaCreateBufferWithAlignment = VmaAllocator
                               -> (Ptr VkBufferCreateInfo)
                               -> (Ptr VmaAllocationCreateInfo)
                               -> VkDeviceSize
                               -> (Ptr VkBuffer)
                               -> (Ptr VmaAllocation)
                               -> (Ptr VmaAllocationInfo)
                               -> IO VkResult

type PFN_vmaCreateBufferWithAlignment = FunPtr HS_vmaCreateBufferWithAlignment

foreign import ccall unsafe "vmaCreateAliasingBuffer"
  vmaCreateAliasingBuffer :: VmaAllocator
                          -> VmaAllocation
                          -> (Ptr VkBufferCreateInfo)
                          -> (Ptr VkBuffer)
                          -> IO VkResult

type HS_vmaCreateAliasingBuffer = VmaAllocator
                          -> VmaAllocation
                          -> (Ptr VkBufferCreateInfo)
                          -> (Ptr VkBuffer)
                          -> IO VkResult

type PFN_vmaCreateAliasingBuffer = FunPtr HS_vmaCreateAliasingBuffer

foreign import ccall unsafe "vmaDestroyBuffer"
  vmaDestroyBuffer :: VmaAllocator
                   -> VkBuffer
                   -> VmaAllocation
                   -> IO ()

type HS_vmaDestroyBuffer = VmaAllocator
                   -> VkBuffer
                   -> VmaAllocation
                   -> IO ()

type PFN_vmaDestroyBuffer = FunPtr HS_vmaDestroyBuffer

foreign import ccall unsafe "vmaCreateImage"
  vmaCreateImage :: VmaAllocator
                 -> (Ptr VkImageCreateInfo)
                 -> (Ptr VmaAllocationCreateInfo)
                 -> (Ptr VkImage)
                 -> (Ptr VmaAllocation)
                 -> (Ptr VmaAllocationInfo)
                 -> IO VkResult

type HS_vmaCreateImage = VmaAllocator
                 -> (Ptr VkImageCreateInfo)
                 -> (Ptr VmaAllocationCreateInfo)
                 -> (Ptr VkImage)
                 -> (Ptr VmaAllocation)
                 -> (Ptr VmaAllocationInfo)
                 -> IO VkResult

type PFN_vmaCreateImage = FunPtr HS_vmaCreateImage

foreign import ccall unsafe "vmaCreateAliasingImage"
  vmaCreateAliasingImage :: VmaAllocator
                         -> VmaAllocation
                         -> (Ptr VkImageCreateInfo)
                         -> (Ptr VkImage)
                         -> IO VkResult

type HS_vmaCreateAliasingImage = VmaAllocator
                         -> VmaAllocation
                         -> (Ptr VkImageCreateInfo)
                         -> (Ptr VkImage)
                         -> IO VkResult

type PFN_vmaCreateAliasingImage = FunPtr HS_vmaCreateAliasingImage

foreign import ccall unsafe "vmaDestroyImage"
  vmaDestroyImage :: VmaAllocator
                  -> VkImage
                  -> VmaAllocation
                  -> IO ()

type HS_vmaDestroyImage = VmaAllocator
                  -> VkImage
                  -> VmaAllocation
                  -> IO ()

type PFN_vmaDestroyImage = FunPtr HS_vmaDestroyImage

foreign import ccall unsafe "vmaCreateVirtualBlock"
  vmaCreateVirtualBlock :: (Ptr VmaVirtualBlockCreateInfo)
                        -> (Ptr VmaVirtualBlock)
                        -> IO VkResult

type HS_vmaCreateVirtualBlock = (Ptr VmaVirtualBlockCreateInfo)
                        -> (Ptr VmaVirtualBlock)
                        -> IO VkResult

type PFN_vmaCreateVirtualBlock = FunPtr HS_vmaCreateVirtualBlock

foreign import ccall unsafe "vmaDestroyVirtualBlock"
  vmaDestroyVirtualBlock :: VmaVirtualBlock
                         -> IO ()

type HS_vmaDestroyVirtualBlock = VmaVirtualBlock
                         -> IO ()

type PFN_vmaDestroyVirtualBlock = FunPtr HS_vmaDestroyVirtualBlock

foreign import ccall unsafe "vmaGetVirtualAllocationInfo"
  vmaGetVirtualAllocationInfo :: VmaVirtualBlock
                              -> VmaVirtualAllocation
                              -> IO ()

type HS_vmaGetVirtualAllocationInfo = VmaVirtualBlock
                              -> VmaVirtualAllocation
                              -> IO ()

type PFN_vmaGetVirtualAllocationInfo = FunPtr HS_vmaGetVirtualAllocationInfo

foreign import ccall unsafe "vmaVirtualAllocate"
  vmaVirtualAllocate :: VmaVirtualBlock
                     -> (Ptr VmaVirtualAllocationCreateInfo)
                     -> (Ptr VmaVirtualAllocation)
                     -> (Ptr VkDeviceSize)
                     -> IO VkResult

type HS_vmaVirtualAllocate = VmaVirtualBlock
                     -> (Ptr VmaVirtualAllocationCreateInfo)
                     -> (Ptr VmaVirtualAllocation)
                     -> (Ptr VkDeviceSize)
                     -> IO VkResult

type PFN_vmaVirtualAllocate = FunPtr HS_vmaVirtualAllocate

foreign import ccall unsafe "vmaVirtualFree"
  vmaVirtualFree :: VmaVirtualBlock
                 -> VmaVirtualAllocation
                 -> IO ()

type HS_vmaVirtualFree = VmaVirtualBlock
                 -> VmaVirtualAllocation
                 -> IO ()

type PFN_vmaVirtualFree = FunPtr HS_vmaVirtualFree

foreign import ccall unsafe "vmaClearVirtualBlock"
  vmaClearVirtualBlock :: VmaVirtualBlock
                       -> IO ()

type HS_vmaClearVirtualBlock = VmaVirtualBlock
                       -> IO ()

type PFN_vmaClearVirtualBlock = FunPtr HS_vmaClearVirtualBlock

foreign import ccall unsafe "vmaSetVirtualAllocationUserData"
  vmaSetVirtualAllocationUserData :: VmaVirtualBlock
                                  -> VmaVirtualAllocation
                                  -> (Ptr Void)
                                  -> IO ()

type HS_vmaSetVirtualAllocationUserData = VmaVirtualBlock
                                  -> VmaVirtualAllocation
                                  -> (Ptr Void)
                                  -> IO ()

type PFN_vmaSetVirtualAllocationUserData = FunPtr HS_vmaSetVirtualAllocationUserData

foreign import ccall unsafe "vmaGetVirtualBlockStatistics"
  vmaGetVirtualBlockStatistics :: VmaVirtualBlock
                               -> (Ptr VmaStatistics)
                               -> IO ()

type HS_vmaGetVirtualBlockStatistics = VmaVirtualBlock
                               -> (Ptr VmaStatistics)
                               -> IO ()

type PFN_vmaGetVirtualBlockStatistics = FunPtr HS_vmaGetVirtualBlockStatistics

foreign import ccall unsafe "vmaCalculateVirtualBlockStatistics"
  vmaCalculateVirtualBlockStatistics :: VmaVirtualBlock
                                     -> (Ptr VmaDetailedStatistics)
                                     -> IO ()

type HS_vmaCalculateVirtualBlockStatistics = VmaVirtualBlock
                                     -> (Ptr VmaDetailedStatistics)
                                     -> IO ()

type PFN_vmaCalculateVirtualBlockStatistics = FunPtr HS_vmaCalculateVirtualBlockStatistics

foreign import ccall unsafe "vmaBuildVirtualBlockStatsString"
  vmaBuildVirtualBlockStatsString :: VmaVirtualBlock
                                  -> CString
                                  -> VkBool32
                                  -> IO ()

type HS_vmaBuildVirtualBlockStatsString = VmaVirtualBlock
                                  -> CString
                                  -> VkBool32
                                  -> IO ()

type PFN_vmaBuildVirtualBlockStatsString = FunPtr HS_vmaBuildVirtualBlockStatsString

foreign import ccall unsafe "vmaFreeVirtualBlockStatsString"
  vmaFreeVirtualBlockStatsString :: VmaVirtualBlock
                                 -> CString
                                 -> IO ()

type HS_vmaFreeVirtualBlockStatsString = VmaVirtualBlock
                                 -> CString
                                 -> IO ()

type PFN_vmaFreeVirtualBlockStatsString = FunPtr HS_vmaFreeVirtualBlockStatsString

foreign import ccall unsafe "vmaBuildStatsString"
  vmaBuildStatsString :: VmaAllocator
                      -> CString
                      -> VkBool32
                      -> IO ()

type HS_vmaBuildStatsString = VmaAllocator
                      -> CString
                      -> VkBool32
                      -> IO ()

type PFN_vmaBuildStatsString = FunPtr HS_vmaBuildStatsString

foreign import ccall unsafe "vmaFreeStatsString"
  vmaFreeStatsString :: VmaAllocator
                     -> CString
                     -> IO ()

type HS_vmaFreeStatsString = VmaAllocator
                     -> CString
                     -> IO ()

type PFN_vmaFreeStatsString = FunPtr HS_vmaFreeStatsString

