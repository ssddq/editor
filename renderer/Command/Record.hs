{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command.Record where

import Vk

import Command.Record.Info
import Command.Record.Utils


cmdRenderPass0
  :: Word32
  -> Buffers
  -> Vk    { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> IO Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
cmdRenderPass0 drawCount buffers vk = do
  let renderPassBeginInfo = mkRenderPassBeginInfo
                              |- renderPass0.handle
                              |- framebuffer.renderPass0
                              |- render
      viewport = mkViewport render
      scissor  = mkScissor render
  vkCmdBeginRenderPass
    |- commandBuffer
    |- p renderPassBeginInfo
    |- VK_SUBPASS_CONTENTS_INLINE
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- renderPass0.subpass0.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor $ vkCmdSetScissor
                   |- commandBuffer
                   |- 0
                   |- 1
  withArray [font.vertex.buffer, instanceData.buffer]
    $ \pVertex -> withArray [0,0]
    $ \pOffset -> vkCmdBindVertexBuffers
                    |- commandBuffer
                    |- 0
                    |- 2
                    |- pVertex
                    |- pOffset
  vkCmdBindIndexBuffer
    |- commandBuffer
    |- font.index.buffer
    |- 0
    |- VK_INDEX_TYPE_UINT32
  bindDescriptorSets
    |- commandBuffer
    |- renderPass0.subpass0.pipeline.layout
    |- renderPass0.subpass0.descriptors.sets
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- renderPass0.subpass0.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  let draw = vkCmdDrawIndexedIndirect
               |- commandBuffer
               |- indirectDraw.buffer
               |- 0
               |- drawCount
               |- 20
  draw
  vkCmdNextSubpass
    |- commandBuffer
    |- VK_SUBPASS_CONTENTS_INLINE
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- renderPass0.subpass1.pipeline.handle
  with viewport $ vkCmdSetViewport
                    |- commandBuffer
                    |- 0
                    |- 1
  with scissor  $ vkCmdSetScissor
                    |- commandBuffer
                    |- 0
                    |- 1
  bindDescriptorSets
    |- commandBuffer
    |- renderPass0.subpass1.pipeline.layout
    |- renderPass0.subpass1.descriptors.sets
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- renderPass0.subpass1.pipeline.layout
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
  let drawFullscreen = vkCmdDrawIndexed
                         |- commandBuffer
                         |- 6
                         |- 1
                         |- 0
                         |- 0
                         |- 0
  drawFullscreen
  vkCmdEndRenderPass
    |- commandBuffer
  return vk
  where Vk        {..} = vk
        Font      {..} = font
        Buffers   {..} = buffers
        Constants {..} = constants
        RenderPipeline {..} = renderPipeline

cmdRenderPass1
  :: Buffers
  -> Vk    { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> IO Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
cmdRenderPass1 buffers vk = do
  let renderPassBeginInfo = mkRenderPassBeginInfo
                              |- renderPass1.handle
                              |- framebuffer.renderPass1
                              |- present
      viewport = mkViewport present
      scissor  = mkScissor  present
  vkCmdBeginRenderPass
    |- commandBuffer
    |- p renderPassBeginInfo
    |- VK_SUBPASS_CONTENTS_INLINE
  vkCmdBindPipeline
    |- commandBuffer
    |- VK_PIPELINE_BIND_POINT_GRAPHICS
    |- renderPass1.subpass0.pipeline.handle
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
  bindDescriptorSets
    |- commandBuffer
    |- renderPass1.subpass0.pipeline.layout
    |- renderPass1.subpass0.descriptors.sets
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- renderPass1.subpass0.pipeline.layout
        |- VK_SHADER_STAGE_VERTEX_BIT
        |- 0
        |- 16
  let drawFullscreen = vkCmdDrawIndexed
                         |- commandBuffer
                         |- 6
                         |- 1
                         |- 0
                         |- 0
                         |- 0
  drawFullscreen
  vkCmdEndRenderPass
    |- commandBuffer
  return vk
  where Vk        {..} = vk
        Font      {..} = font
        Buffers   {..} = buffers
        Constants {..} = constants
        RenderPipeline {..} = renderPipeline

recordCommandBuffer
  :: Buffers
  -> Word32
  -> Vk    { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> IO Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
recordCommandBuffer buffers drawCount vk = do
  vkBeginCommandBuffer
    |- commandBuffer
    |- p commandBufferBeginInfo
  cmdRenderPass0 drawCount buffers vk
  cmdRenderPass1 buffers vk
  vkEndCommandBuffer
    |- commandBuffer
  return vk
  where Buffers {..} = buffers
