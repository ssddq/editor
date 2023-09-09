{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell     #-}

module Initialize.Allocator where

import Vk

import Initialize.Allocator.Info

-- | Initializes a VMA allocator
createAllocator
  :: VkPhysicalDevice
  -> VkDevice
  -> VkInstance
  -> IO VmaAllocator
createAllocator physicalDevice device instance_ = do
  let allocatorCreateInfo = mkAllocatorCreateInfo
                              |- physicalDevice
                              |- device
                              |- instance_
  allocator <- perform $ vmaCreateAllocator
                           |- p allocatorCreateInfo
  return $ allocator
