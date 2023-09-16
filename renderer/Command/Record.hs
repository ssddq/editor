{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command.Record where

import Vk

import Command.Record.Info
import Command.Record.Utils

import Data.Vector qualified as V

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
      cmdDraw    = subpassDraw vk buffers viewport scissor
      cmdResolve = subpassResolve vk buffers viewport scissor
      nextSubpass = vkCmdNextSubpass
                      |- commandBuffer
                      |- VK_SUBPASS_CONTENTS_INLINE
      clearAttachments = clearColorAttachments
                           |- commandBuffer
                           |- render
                           |- V.fromList [0,1]
  vkCmdBeginRenderPass
    |- commandBuffer
    |- p renderPassBeginInfo
    |- VK_SUBPASS_CONTENTS_INLINE
  cmdDraw
    |- renderPass0.draw0
    |- (0, 1)
  nextSubpass
  cmdResolve renderPass0.resolve0
  nextSubpass
  clearAttachments
  nextSubpass
  cmdDraw
    |- renderPass0.draw1
    |- (20, drawCount - 1)
  nextSubpass
  cmdResolve renderPass0.resolve1
  nextSubpass
  clearAttachments
  nextSubpass
  -- cmdDraw
  --   |- renderPass0.draw2
  --   |- (0, 0)
  nextSubpass
  -- cmdResolve renderPass0.resolve2
  nextSubpass
  clearAttachments
  nextSubpass
  -- cmdDraw
  --   |- renderPass0.draw3
  --   |- (0, 0)
  nextSubpass
  -- cmdResolve renderPass0.resolve3
  vkCmdEndRenderPass
    |- commandBuffer
  return vk
  where Vk        {..} = vk
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
    |- renderPass1.aa.pipeline.handle
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
  bindDescriptorSet
    |- commandBuffer
    |- renderPass1.aa.pipeline.layout
    |- renderPass1.aa.descriptors.set
  with (present, unitsPerEmX2, ppi) $ (. castPtr)
    $ vkCmdPushConstants
        |- commandBuffer
        |- renderPass1.aa.pipeline.layout
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
