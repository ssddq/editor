{-# OPTIONS_GHC -F -pgmF=tpr-pp#-}

{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Command.Record.Utils where

import Vk

import Data.Vector qualified as V


{-# INLINE bgColor #-}
bgColor :: (Float, Float, Float, Float)
bgColor = (0/255, 0/255, 0/255, 1)

{-# INLINE bgColorHighlight #-}
bgColorHighlight :: (Float, Float, Float, Float)
bgColorHighlight = (0/255, 0/255, 0/255, 1)

bindDescriptorSet
  :: VkCommandBuffer
  -> VkPipelineLayout
  -> VkDescriptorSet
  -> IO ()
bindDescriptorSet commandBuffer layout set
  | set == VK_NULL_HANDLE = return ()
  | otherwise = with set $ \pDescriptorSets -> do
                  vkCmdBindDescriptorSets
                    |- commandBuffer
                    |- VK_PIPELINE_BIND_POINT_GRAPHICS
                    |- layout
                    |- 0
                    |- 1
                    |- pDescriptorSets
                    |- 0
                    |- VK_NULL

bindDescriptorSets
  :: VkCommandBuffer
  -> VkPipelineLayout
  -> V.Vector VkDescriptorSet
  -> IO ()
bindDescriptorSets commandBuffer layout sets
  | V.null sets = return ()
  | otherwise   = do withVector sets
                       $ \n pDescriptorSets -> vkCmdBindDescriptorSets
                                                 |- commandBuffer
                                                 |- VK_PIPELINE_BIND_POINT_GRAPHICS
                                                 |- layout
                                                 |- 0
                                                 |- fromIntegral n
                                                 |- pDescriptorSets
                                                 |- 0
                                                 |- VK_NULL
                     return ()

{-# INLINE blank #-}
blank :: (Float, Float, Float, Float)
blank = (0, 0, 0, 0)

-- | Clear given area of a color attachment with the given clear color and index.
-- | Note that this clears the *output* attachments when called in a given subpass.

{-# INLINE clearColorAttachments #-}
clearColorAttachments
  :: VkCommandBuffer
  -> VkRect2D
  -> (Float, Float, Float, Float)
  -> V.Vector Word32
  -> IO ()
clearColorAttachments commandBuffer rect (r, g, b, a) indices =
  withVector attachments
    $ \attachmentCount pAttachments -> withVector clearRects
    $ \rectCount pRects -> vkCmdClearAttachments
                             |- commandBuffer
                             |- fromIntegral attachmentCount
                             |- pAttachments
                             |- fromIntegral rectCount
                             |- pRects
  where attachments = (flip V.map) indices $ \i -> createVk @VkClearAttachment
          $ set @"aspectMask"      |* VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"colorAttachment" |* i
         &* set @"clearValue"      |* clearColor
        clearRects = V.singleton $ createVk @VkClearRect
          $ set @"rect"           |* rect
         &* set @"baseArrayLayer" |* 0
         &* set @"layerCount"     |* 1
        clearColor = createVk @VkClearValue
          $ set   @"color"      |* clearColorValue
        clearColorValue = createVk @VkClearColorValue
          $ setAt @"float32" @0 |* r
         &* setAt @"float32" @1 |* g
         &* setAt @"float32" @2 |* b
         &* setAt @"float32" @3 |* a

{-# INLINE present2RenderX #-}
present2RenderX
  :: Constants
  -> Float
  -> Float
present2RenderX Constants{..} x = x * (fromIntegral render.width / fromIntegral present.width)

{-# INLINE present2RenderY #-}
present2RenderY
  :: Constants
  -> Float
  -> Float
present2RenderY Constants{..} y = y * (fromIntegral render.height / fromIntegral present.height)

-- | Do *not* use subpassClear without testing.
-- | clearColorAttachments has virtually no impact on frame times,
-- | whereas subpassClear seems to be a 50% increase with each invocation.
-- |
-- | This is because subpassClear draws over the entire color attachment;
-- | in contrast, (I assume) clearColorAttachments is optimized by the driver
-- | to flag the attachment as cleared, rather than drawing over each pixel.
-- |
-- | Keeping the function in case it's useful, but do *not* use without profiling.

subpassClear
  :: Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> Buffers
  -> VkViewport
  -> VkRect2D
  -> Subpass
  -> IO ()
subpassClear vk buffers viewport scissor subpass = do
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- subpass.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor  $ vkCmdSetScissor
                    |- commandBuffer
                    |- 0
                    |- 1
  bindDescriptorSet
    |- commandBuffer
    |- subpass.pipeline.layout
    |- subpass.descriptors.set
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- subpass.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  with2 fullscreenBuffer.vertex.buffer 0
    $ \pVertex ->
      \pOffset -> vkCmdBindVertexBuffers
                    |- commandBuffer
                    |- 0
                    |- 1
                    |- pVertex
                    |- pOffset
  vkCmdBindIndexBuffer
    |- commandBuffer
    |- fullscreenBuffer.index.buffer
    |- 0
    |- VK_INDEX_TYPE_UINT32
  vkCmdDrawIndexed
    |- commandBuffer
    |- 6
    |- 1
    |- 0
    |- 0
    |- 0
  where Vk        {..} = vk
        Buffers   {..} = buffers
        Constants {..} = constants
        Font      {..} = font

subpassDraw
  :: Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> Buffers
  -> VkViewport
  -> VkRect2D
  -> Subpass
  -> (VkDeviceSize, Word32)
  -> IO ()
subpassDraw vk buffers viewport scissor subpass (offset, drawCount) = do
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- subpass.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor $ vkCmdSetScissor
                   |- commandBuffer
                   |- 0
                   |- 1
  withVector [font.vertex.buffer, instanceData.buffer]
    $ \n pVertex -> withArray [0,0]
    $ \pOffset   -> vkCmdBindVertexBuffers
                      |- commandBuffer
                      |- 0
                      |- fromIntegral n
                      |- pVertex
                      |- pOffset
  vkCmdBindIndexBuffer
    |- commandBuffer
    |- font.index.buffer
    |- 0
    |- VK_INDEX_TYPE_UINT32
  bindDescriptorSet
    |- commandBuffer
    |- subpass.pipeline.layout
    |- subpass.descriptors.set
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- subpass.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  vkCmdDrawIndexedIndirect
    |- commandBuffer
    |- indirectDraw.buffer
    |- offset
    |- drawCount
    |- 20
  where Vk        {..} = vk
        Buffers   {..} = buffers
        Constants {..} = constants
        Font      {..} = font

subpassDrawFullscreen
  :: Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> Buffers
  -> VkViewport
  -> VkRect2D
  -> Subpass
  -> IO ()
subpassDrawFullscreen vk buffers viewport scissor subpass = do
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- subpass.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor $ vkCmdSetScissor
                   |- commandBuffer
                   |- 0
                   |- 1
  with2 fullscreenBuffer.vertex.buffer 0
    $ \pVertex ->
      \pOffset -> vkCmdBindVertexBuffers
                    |- commandBuffer
                    |- 0
                    |- 1
                    |- pVertex
                    |- pOffset
  vkCmdBindIndexBuffer
    |- commandBuffer
    |- fullscreenBuffer.index.buffer
    |- 0
    |- VK_INDEX_TYPE_UINT32
  vkCmdDrawIndexed
    |- commandBuffer
    |- 6
    |- 1
    |- 0
    |- 0
    |- 0
  where Vk      {..} = vk
        Buffers {..} = buffers

subpassResolve
  :: Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> Buffers
  -> VkViewport
  -> VkRect2D
  -> Subpass
  -> (VkDeviceSize, Word32)
  -> IO ()
subpassResolve vk buffers viewport scissor subpass (offset, drawCount) = do
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- subpass.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor  $ vkCmdSetScissor
                    |- commandBuffer
                    |- 0
                    |- 1
  bindDescriptorSet
    |- commandBuffer
    |- subpass.pipeline.layout
    |- subpass.descriptors.set
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- subpass.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  withVector [font.vertex.buffer, instanceData.buffer]
    $ \n pVertex -> withArray [0,0]
    $ \pOffset   -> vkCmdBindVertexBuffers
                      |- commandBuffer
                      |- 0
                      |- fromIntegral n
                      |- pVertex
                      |- pOffset
  vkCmdBindIndexBuffer
    |- commandBuffer
    |- font.index.buffer
    |- 0
    |- VK_INDEX_TYPE_UINT32
  bindDescriptorSet
    |- commandBuffer
    |- subpass.pipeline.layout
    |- subpass.descriptors.set
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- subpass.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  vkCmdDrawIndexedIndirect
    |- commandBuffer
    |- indirectDraw.buffer
    |- offset
    |- drawCount
    |- 20
  where Vk        {..} = vk
        Buffers   {..} = buffers
        Constants {..} = constants
        Font      {..} = font

{-# INLINE windNonzero #-}
windNonzero :: (Float, Float, Float, Float)
windNonzero = (1, 0, 0, 0)
