{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command.Buffers where

import Vk

import Data.Vector qualified as V

createBuffer
  :: Word64
  -> VmaAllocator
  -> VkBufferUsageBitmask FlagMask
  -> IO Buffer
createBuffer size allocator usage = do
  (buffer, allocation, _ ) <- perform3 $ vmaCreateBuffer
                                           |- allocator
                                           |- p bufferCreateInfo
                                           |- p allocationCreateInfo
  ptr <- perform $ vmaMapMemory
                     |- allocator
                     |- allocation
  return $ Buffer { buffer
                  , allocation
                  , ptr
                  }
  where bufferCreateInfo = createVk @VkBufferCreateInfo
           $ set                @"sType"                   |* VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
          &* set                @"pNext"                   |* VK_NULL
          &* set                @"flags"                   |* VK_ZERO_FLAGS
          &* set                @"size"                    |* VkDeviceSize size
          &* set                @"usage"                   |* usage
          &* set                @"sharingMode"             |* VK_SHARING_MODE_EXCLUSIVE
          &* setListCountAndRef @"queueFamilyIndexCount"-- |*
                                @"pQueueFamilyIndices"     |* []
        allocationCreateInfo = createVk @VmaAllocationCreateInfo
           $ set                @"flags"                   |* VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
          &* set                @"usage"                   |* VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE
          &* set                @"requiredFlags"           |* VK_ZERO_FLAGS
          &* set                @"preferredFlags"          |* VK_ZERO_FLAGS
          &* set                @"memoryTypeBits"          |* 0
          &* set                @"pool"                    |* VK_NULL_HANDLE
          &* set                @"pUserData"               |* VK_NULL
          &* set                @"priority"                |* 1

createDrawBuffers
  :: Vk    { commandPool = I, drawBuffers = X, renderPipeline = I, vulkan = I }
  -> IO Vk { commandPool = I, drawBuffers = I, renderPipeline = I, vulkan = I }
createDrawBuffers vk = do
  let createFramebuffers = \image -> do
        let framebufferCreateInfo0 = mkFramebufferCreateInfo
                                       |- renderPass0.handle
                                       |- [colorAttachment.imageView, sampleAttachment.imageView]
                                       |- constants.render
            framebufferCreateInfo1 = mkFramebufferCreateInfo
                                       |- renderPass1.handle
                                       |- [image, colorAttachment.imageView, sampleAttachment.imageView]
                                       |- constants.present
        framebuffer0 <- perform $ vkCreateFramebuffer
                                    |- device
                                    |- p framebufferCreateInfo0
                                    |- VK_NULL
        framebuffer1 <- perform $ vkCreateFramebuffer
                                    |- device
                                    |- p framebufferCreateInfo1
                                    |- VK_NULL
        return $ Framebuffers framebuffer0 framebuffer1
  framebuffers <- mapM createFramebuffers imageViews
  let count = V.length framebuffers
  commandBuffers <- performArray
                      |- count
                      |- allocateCommandBuffers count
  indirectDrawBuffers <- V.replicateM count $ createBuffer
                                                |- 1000000
                                                |- allocator
                                                |- VK_BUFFER_USAGE_INDIRECT_BUFFER_BIT
  instanceDataBuffers <- V.replicateM count $ createBuffer
                                                |- 1000000
                                                |- allocator
                                                |- VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  let drawBuffers = V.generate count $ \n -> Buffers { indirectDraw  = indirectDrawBuffers V.! n
                                                     , instanceData  = instanceDataBuffers V.! n
                                                     , commandBuffer = commandBuffers      V.! n
                                                     , framebuffer   = framebuffers        V.! n
                                                     }
  return $ vk { drawBuffers }
  where Vk     {..} = vk
        Vulkan {..} = vulkan
        RenderPipeline {..} = renderPipeline
        allocateCommandBuffers count = vkAllocateCommandBuffers
                                   |- device
                                   |- p (commandBufferAllocateInfo $ fromIntegral count)
        commandBufferAllocateInfo count = createVk @VkCommandBufferAllocateInfo
          $ set                @"sType"              |* VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
         &* set                @"pNext"              |* VK_NULL
         &* set                @"commandPool"        |* commandPool
         &* set                @"level"              |* VK_COMMAND_BUFFER_LEVEL_PRIMARY
         &* set                @"commandBufferCount" |* count
        mkFramebufferCreateInfo renderPass attachments area = createVk @VkFramebufferCreateInfo
          $ set                @"sType"              |* VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
         &* set                @"pNext"              |* VK_NULL
         &* set                @"flags"              |* VK_ZERO_FLAGS
         &* set                @"renderPass"         |* renderPass
         &* setListCountAndRef @"attachmentCount" -- |*
                               @"pAttachments"       |* attachments
         &* set                @"width"              |* area.width
         &* set                @"height"             |* area.height
         &* set                @"layers"             |* 1

createFullscreenBuffer
  :: Vk    { fullscreenBuffer = X, vulkan = I }
  -> IO Vk { fullscreenBuffer = I, vulkan = I }
createFullscreenBuffer vk = do
  vertex <- createBuffer
              |- 72
              |- allocator
              |- VK_BUFFER_USAGE_VERTEX_BUFFER_BIT
  index  <- createBuffer
              |- 24
              |- allocator
              |- VK_BUFFER_USAGE_INDEX_BUFFER_BIT
  pokeArray (castPtr vertex.ptr :: Ptr (Int32, Int32, Word32))
    [ (-1,-1, 0)
    , (-1, 1, 0)
    , ( 1, 1, 0)
    , ( 1, 1, 0)
    , ( 1,-1, 0)
    , (-1,-1, 0)
    ]
  pokeArray (castPtr index.ptr :: Ptr Word32)
    [ 0 .. 5 ]
  let fullscreenBuffer = FullscreenBuffer { vertex, index, indexCount = 6 }
  return $ vk { fullscreenBuffer }
  where allocator = vk.vulkan.allocator

recreateFramebuffers
  :: Vk    { drawBuffers = I, renderPipeline = I, vulkan = I }
  -> IO Vk { drawBuffers = I, renderPipeline = I, vulkan = I }
recreateFramebuffers vk = do
  let createFramebuffer = \image -> do
        let framebufferCreateInfo1 = mkFramebufferCreateInfo1 image
        framebuffer0 <- perform $ vkCreateFramebuffer
                                    |- device
                                    |- p framebufferCreateInfo0
                                    |- VK_NULL
        framebuffer1 <- perform $ vkCreateFramebuffer
                                    |- device
                                    |- p framebufferCreateInfo1
                                    |- VK_NULL
        return $ Framebuffers framebuffer0 framebuffer1
  newDrawBuffers <- forM (V.zip drawBuffers imageViews) $ \(buffers, image) -> do
    let framebuffer0 = buffers.framebuffer.renderPass0
        framebuffer1 = buffers.framebuffer.renderPass1
    vkDestroyFramebuffer
      |- device
      |- framebuffer0
      |- VK_NULL
    vkDestroyFramebuffer
      |- device
      |- framebuffer1
      |- VK_NULL
    newFramebuffer <- createFramebuffer image
    return $ buffers { framebuffer = newFramebuffer }
  return $ vk { drawBuffers = newDrawBuffers }
  where Vk     {..} = vk
        Vulkan {..} = vulkan
        RenderPipeline {..} = renderPipeline
        framebufferCreateInfo0 = createVk @VkFramebufferCreateInfo
          $ set                @"sType"             |* VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
         &* set                @"pNext"             |* VK_NULL
         &* set                @"flags"             |* VK_ZERO_FLAGS
         &* set                @"renderPass"        |* renderPass0.handle
         &* setListCountAndRef @"attachmentCount"-- |*
                               @"pAttachments"      |* [colorAttachment.imageView, sampleAttachment.imageView]
         &* set                @"width"             |* constants.render.width
         &* set                @"height"            |* constants.render.height
         &* set                @"layers"            |* 1
        mkFramebufferCreateInfo1 img = createVk @VkFramebufferCreateInfo
          $ set                @"sType"             |* VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
         &* set                @"pNext"             |* VK_NULL
         &* set                @"flags"             |* VK_ZERO_FLAGS
         &* set                @"renderPass"        |* renderPass1.handle
         &* setListCountAndRef @"attachmentCount"-- |*
                               @"pAttachments"      |* [img, colorAttachment.imageView, sampleAttachment.imageView]
         &* set                @"width"             |* constants.present.width
         &* set                @"height"            |* constants.present.height
         &* set                @"layers"            |* 1
