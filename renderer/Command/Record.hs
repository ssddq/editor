{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}

module Command.Record where

import Vk

import Command.Record.Info
import Command.Record.Utils

cmdRenderPass0
  :: WriteState
  -> Buffers
  -> Vk    { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> IO Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
cmdRenderPass0 state buffers vk = do
  let renderPassBeginInfo = mkRenderPassBeginInfo
                              |- renderPass0.handle
                              |- framebuffer.renderPass0
                              |- render
      viewport = mkViewport render
      scissor  = mkScissor render
      cmdDraw  = subpassDraw vk buffers viewport scissor
      cmdResolve  = subpassResolve vk buffers viewport scissor
      nextSubpass = vkCmdNextSubpass
                      |- commandBuffer
                      |- VK_SUBPASS_CONTENTS_INLINE
      clearAttachments = clearColorAttachments
                           |- commandBuffer
                           |- mkRect2D (0, 0) (render.width, render.height)
  vkCmdBeginRenderPass
    |- commandBuffer
    |- p renderPassBeginInfo
    |- VK_SUBPASS_CONTENTS_INLINE
  -- draw0
  nextSubpass
  -- resolve0
  clearAttachments bgColor [0]
  clearColorAttachments
    |- commandBuffer
    |- mkRect2D ( round . present2RenderX constants $ state.xMin
                , round . present2RenderY constants $ state.currentLineStart
                )
                ( round . present2RenderX constants $ state.xMax - state.xMin
                , round . present2RenderY constants $ state.currentLineStop - state.currentLineStart
                )
    |- bgColor
    |- [0]
  nextSubpass
  -- clear0
  nextSubpass
  -- draw1
  cmdDraw
    |- renderPass0.draw1
    |- (20, state.instanceNum - 1)
  nextSubpass
  -- resolve1
  cmdResolve
    |- renderPass0.resolve1
    |- (20, state.instanceNum - 1)
  nextSubpass
  -- clear1
  clearAttachments blank [0]
  nextSubpass
  -- draw2
  cmdDraw
    |- renderPass0.draw2
    |- (0, 1)
  nextSubpass
  -- resolve2
  cmdDraw
    |- renderPass0.resolve2
    |- (0, 1)
  nextSubpass
  -- clear2
  clearAttachments blank [0]
  nextSubpass
  -- draw3
  nextSubpass
  -- resolve3
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
  -> WriteState
  -> Vk    { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
  -> IO Vk { font = I, fullscreenBuffer = I, renderPipeline = I, vulkan = I }
recordCommandBuffer buffers state vk = do
  vkBeginCommandBuffer
    |- commandBuffer
    |- p commandBufferBeginInfo
  cmdRenderPass0
    |- state
    |- buffers
    |- vk
  cmdRenderPass1
    |- buffers
    |- vk
  vkEndCommandBuffer
    |- commandBuffer
  return vk
  where Buffers {..} = buffers
