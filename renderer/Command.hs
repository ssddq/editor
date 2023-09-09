{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command
  ( module Command
  , module Command.Buffers
  , module Command.Signals
  , module Command.Write
  ) where

import Vk

import Command.Buffers
import Command.Signals
import Command.Write

import Command.Record

import Data.Vector qualified as V

import Graphics.Vulkan.Ext.VK_KHR_swapchain

createCommandPool
  :: Vk    { commandPool = X, vulkan = I }
  -> IO Vk { commandPool = I, vulkan = I }
createCommandPool vk = do
  commandPool <- perform $ vkCreateCommandPool
                             |- device
                             |- p commandPoolCreateInfo
                             |- VK_NULL
  return $ vk { commandPool }
  where Vk     {..} = vk
        Vulkan {..} = vulkan
        commandPoolCreateInfo = createVk @VkCommandPoolCreateInfo
          $ set @"sType"            |* VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
         &* set @"pNext"            |* VK_NULL
         &* set @"flags"            |* VK_COMMAND_POOL_CREATE_RESET_COMMAND_BUFFER_BIT
         &* set @"queueFamilyIndex" |* queueFamilyIndex

drawFrame
  :: Vk    { drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A b, vulkan = I }
  -> IO Vk { drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A b, vulkan = I }
drawFrame vk = do
  queue <- perform $ vkGetDeviceQueue
                       |- device
                       |- queueFamilyIndex
                       |- 0
  imageIndex <- perform $ vkAcquireNextImageKHR
                            |- device
                            |- swapchain
                            |- maxBound
                            |- semaphores.available
                            |- VK_NULL_HANDLE
  let Buffers {..} = drawBuffers V.! fromIntegral imageIndex
  waitOn
    |- device
    |- fences.inFlight
  vkResetCommandBuffer
    |- commandBuffer
    |- VK_COMMAND_BUFFER_RESET_RELEASE_RESOURCES_BIT
  (drawCount, textBuffer) <- writeIndirectDraws
                               |- indirectDraw
                               |- instanceData
                               |- vk
  recordCommandBuffer
    |- drawBuffers V.! fromIntegral imageIndex
    |- drawCount
    |- vk
  let submitInfo  = mkSubmitInfo  commandBuffer
      presentInfo = mkPresentInfo imageIndex
  vkQueueSubmit
    |- queue
    |- 1
    |- p submitInfo
    |- fences.inFlight
  vkQueuePresentKHR
    |- queue
    |- p presentInfo
  return $ vk { stream = stream { textBuffer } }
  where Vk      {..} = vk
        Vulkan  {..} = vulkan
        Signals {..} = signals
        mkSubmitInfo commandBuffer = createVk @VkSubmitInfo
          $ set                @"sType"                  |* VK_STRUCTURE_TYPE_SUBMIT_INFO
         &* set                @"pNext"                  |* VK_NULL
         &* setListRef         @"pWaitDstStageMask"      |* [VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT]
         &* setListCountAndRef @"waitSemaphoreCount"  -- |*
                               @"pWaitSemaphores"        |* [semaphores.available]
         &* setListCountAndRef @"commandBufferCount"  -- |*
                               @"pCommandBuffers"        |* [commandBuffer]
         &* setListCountAndRef @"signalSemaphoreCount"-- |*
                               @"pSignalSemaphores"      |* [semaphores.done]
        mkPresentInfo index = createVk @VkPresentInfoKHR
          $ set                @"sType"                  |* VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
         &* set                @"pNext"                  |* VK_NULL
         &* setListCountAndRef @"waitSemaphoreCount"  -- |*
                               @"pWaitSemaphores"        |* [semaphores.done]
         &* setListCountAndRef @"swapchainCount"      -- |*
                               @"pSwapchains"            |* [swapchain]
         &* setListRef         @"pImageIndices"          |* [index]
         &* set                @"pResults"               |* VK_NULL
