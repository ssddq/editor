{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeApplications    #-}

module Command.Record.Info where

import Vk

-- | Starts a renderpass with the given framebuffer's images,
-- | and with a render area of given size and no offset.
-- | Framebuffer is assumed to have 3 attachments
-- | which are set to (0,0,0,0) if cleared.
-- |
-- | It would be better at some point to extract the number of
-- | framebuffer attachments from some global data --
-- | e.g. ProgramConstants, and use that to determine
-- | how many clear colors to provide.
mkRenderPassBeginInfo
  :: VkRenderPass
  -> VkFramebuffer
  -> Area
  -> VkRenderPassBeginInfo
mkRenderPassBeginInfo renderPass framebuffer Area{ width, height } = createVk @VkRenderPassBeginInfo
   $ set                @"sType"              |* VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  &* set                @"pNext"              |* VK_NULL
  &* set                @"renderPass"         |* renderPass
  &* set                @"framebuffer"        |* framebuffer
  &* set                @"renderArea"         |* renderArea
  &* setListCountAndRef @"clearValueCount" -- |*
                        @"pClearValues"       |* [ clearColor
                                                 , clearColor
                                                 , clearColor
                                                 ]
  where renderArea = createVk @VkRect2D
          $ set   @"offset"     |* offset
         &* set   @"extent"     |* extent
        offset = createVk @VkOffset2D
          $ set   @"x"          |* 0
         &* set   @"y"          |* 0
        extent = createVk @VkExtent2D
          $ set   @"width"      |* width
         &* set   @"height"     |* height
        clearColor = createVk @VkClearValue
          $ set   @"color"      |* clearColorValue
        clearColorValue = createVk @VkClearColorValue
          $ setAt @"float32" @0 |* 0
         &* setAt @"float32" @1 |* 0
         &* setAt @"float32" @2 |* 0
         &* setAt @"float32" @3 |* 0

mkViewport
  :: Area
  -> VkViewport
mkViewport Area{ width, height } = createVk @VkViewport
   $ set @"x"        |* 0
  &* set @"y"        |* 0
  &* set @"width"    |* fromIntegral width
  &* set @"height"   |* fromIntegral height
  &* set @"minDepth" |* 0
  &* set @"maxDepth" |* 1

mkScissor
  :: Area
  -> VkRect2D
mkScissor Area{ width, height } = createVk @VkRect2D
   $ set @"offset" |* offset
  &* set @"extent" |* extent
  where offset = createVk @VkOffset2D
           $ set @"x"      |* 0
          &* set @"y"      |* 0
        extent = createVk @VkExtent2D
           $ set @"width"  |* width
          &* set @"height" |* height

commandBufferBeginInfo :: VkCommandBufferBeginInfo
commandBufferBeginInfo = createVk @VkCommandBufferBeginInfo
   $ set @"sType"            |* VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  &* set @"pNext"            |* VK_NULL
  &* set @"flags"            |* VK_ZERO_FLAGS
  &* set @"pInheritanceInfo" |* VK_NULL
