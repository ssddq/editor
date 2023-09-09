{-# OPTIONS_GHC -F -pgmF=tpr-pp#-}

{-# LANGUAGE TemplateHaskell #-}

module Font where

import Vk

createIndexBuffer
  :: VmaAllocator
  -> VmaAllocationCreateInfo
  -> Word64
  -> IO Buffer
createIndexBuffer allocator allocationCreateInfo n = do
  ( buffer, allocation, _ ) <- perform3 $ vmaCreateBuffer
                                            |- allocator
                                            |- p indexBufferCreateInfo
                                            |- p allocationCreateInfo
  ptr <- perform $ vmaMapMemory
                     |- allocator
                     |- allocation
  return $ Buffer { buffer
                  , allocation
                  , ptr
                  }
  where indexBufferCreateInfo = createVk @VkBufferCreateInfo
          $ set                @"sType"                   |* VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
         &* set                @"pNext"                   |* VK_NULL
         &* set                @"flags"                   |* VK_ZERO_FLAGS
         &* set                @"size"                    |* VkDeviceSize n
         &* set                @"usage"                   |* VK_BUFFER_USAGE_INDEX_BUFFER_BIT
         &* set                @"sharingMode"             |* VK_SHARING_MODE_EXCLUSIVE
         &* setListCountAndRef @"queueFamilyIndexCount"-- |*
                               @"pQueueFamilyIndices"     |* []

-- | Writes a vertex and index buffer with font data.
-- | This function contains a very large amount of non-trivial logic
-- | in the writeBuffers call (specifically, in writeDraw from Font.Utils).
-- | This is extremely unsafe, since it calls functions that
-- | index into multiple buffers without checks.

createIndexedBuffer
  :: FontWriter
  -> Vk    { font = X, vulkan = I }
  -> IO Vk { font = I, vulkan = I }
createIndexedBuffer fontWriter vk = do
  let mkVertexBuffer = createVertexBuffer
                         |- allocator
                         |- vmaAllocationCreateInfo
      mkIndexBuffer  = createIndexBuffer
                         |- allocator
                         |- vmaAllocationCreateInfo
  (vertexBuffer, indexBuffer, fontData) <- fontWriter
                                             |- mkVertexBuffer
                                             |- mkIndexBuffer
  let FontData {..} = fontData
      scale = (fSize * ppi) / (realToFrac unitsPerEmX2 * 72.0)
      lineHeight = scale * fromIntegral (ascender - descender + lineGap)
      textHeight = scale * fromIntegral (ascender + descender)
      font = Font
        { vertex = vertexBuffer
        , index  = indexBuffer
        , ..
        }
  return $ vk { font }
  where allocator = vk.vulkan.allocator
        Constants {..} = vk.constants
        vmaAllocationCreateInfo = createVk @VmaAllocationCreateInfo
          $  set               @"flags"                   |* VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT
         &*  set               @"usage"                   |* VMA_MEMORY_USAGE_AUTO
         &*  set               @"requiredFlags"           |* VK_ZERO_FLAGS
         &*  set               @"preferredFlags"          |* VK_ZERO_FLAGS
         &*  set               @"memoryTypeBits"          |* 0
         &*  set               @"pool"                    |* VK_NULL_HANDLE
         &*  set               @"pUserData"               |* VK_NULL
         &*  set               @"priority"                |* 1

createVertexBuffer
  :: VmaAllocator
  -> VmaAllocationCreateInfo
  -> Word64
  -> IO Buffer
createVertexBuffer allocator allocationCreateInfo n = do
  ( buffer, allocation, _ ) <- perform3 $ vmaCreateBuffer
                                            |- allocator
                                            |- p vertexBufferCreateInfo
                                            |- p allocationCreateInfo
  ptr <- perform $ vmaMapMemory
                     |- allocator
                     |- allocation
  return $ Buffer { buffer
                  , allocation
                  , ptr
                  }
  where vertexBufferCreateInfo = createVk @VkBufferCreateInfo
          $ set                @"sType"                   |* VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
         &* set                @"pNext"                   |* VK_NULL
         &* set                @"flags"                   |* VK_ZERO_FLAGS
         &* set                @"size"                    |* VkDeviceSize n
         &* set                @"usage"                   |* VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
         &* set                @"sharingMode"             |* VK_SHARING_MODE_EXCLUSIVE
         &* setListCountAndRef @"queueFamilyIndexCount"-- |*
                               @"pQueueFamilyIndices"     |* []
