{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Pipeline.RenderPass.Info where

import Vk

import Graphics.Vulkan.Ext.VK_KHR_swapchain

-- | Creates a subpass description given
-- | input attachment references and
-- | output attachment references.
mkSubpassDescription
  :: [VkAttachmentReference]
  -> [VkAttachmentReference]
  -> VkSubpassDescription
mkSubpassDescription inputAttachments outputAttachments = createVk @VkSubpassDescription
   $ set                @"flags"                      |* VK_ZERO_FLAGS
  &* set                @"pipelineBindPoint"          |* VK_PIPELINE_BIND_POINT_GRAPHICS
  &* setListCountAndRef @"inputAttachmentCount"    -- |*
                        @"pInputAttachments"          |* inputAttachments
  &* setListCountAndRef @"colorAttachmentCount"    -- |*
                        @"pColorAttachments"          |* outputAttachments
  &* set                @"pResolveAttachments"        |* VK_NULL
  &* set                @"pDepthStencilAttachment"    |* VK_NULL
  &* setListCountAndRef @"preserveAttachmentCount" -- |*
                        @"pPreserveAttachments"       |* []

-- | Creates a VkAttachmentReference for a color attachment
-- | in the given position.
colorReference
  :: Word32
  -> VkAttachmentReference
colorReference n = createVk @VkAttachmentReference
   $ set @"attachment" |* n
  &* set @"layout"     |* VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL

-- | Creates a VkAttachmentReference for a shader input attachment
-- | in the given position.
inputReference
  :: Word32
  -> VkAttachmentReference
inputReference n = createVk @VkAttachmentReference
   $ set @"attachment" |* n
  &* set @"layout"     |* VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

-- | Creates a VkRenderPassCreateInfo given
-- | a list of attachment descriptions,
-- | subpass descriptions and
-- | subpass dependencies.
mkRenderPassCreateInfo
  :: [VkAttachmentDescription]
  -> [VkSubpassDescription]
  -> [VkSubpassDependency]
  -> VkRenderPassCreateInfo
mkRenderPassCreateInfo attachments subpasses dependencies = createVk @VkRenderPassCreateInfo
   $ set                @"sType"              |* VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  &* set                @"pNext"              |* VK_NULL
  &* set                @"flags"              |* VK_ZERO_FLAGS
  &* setListCountAndRef @"attachmentCount" -- |*
                        @"pAttachments"       |* attachments
  &* setListCountAndRef @"subpassCount"    -- |*
                        @"pSubpasses"         |* subpasses
  &* setListCountAndRef @"dependencyCount" -- |*
                        @"pDependencies"      |* dependencies

-- | Creates an attachment description given
-- | (loadOp , initialLayout) and
-- | (storeOp, finalLayout  ).
mkAttachmentDescription0
  :: (VkAttachmentLoadOp, VkImageLayout)
  -> (VkAttachmentStoreOp, VkImageLayout)
  -> VkAttachmentDescription
mkAttachmentDescription0 load store = createVk @VkAttachmentDescription
   $ set                @"flags"          |* VK_ZERO_FLAGS
  &* set                @"format"         |* VK_FORMAT_R16G16_SFLOAT
  &* set                @"samples"        |* VK_SAMPLE_COUNT_1_BIT
  &* set                @"loadOp"         |* loadOp
  &* set                @"storeOp"        |* storeOp
  &* set                @"stencilLoadOp"  |* VK_ATTACHMENT_LOAD_OP_DONT_CARE
  &* set                @"stencilStoreOp" |* VK_ATTACHMENT_STORE_OP_DONT_CARE
  &* set                @"initialLayout"  |* initialLayout
  &* set                @"finalLayout"    |* finalLayout
  where (loadOp , initialLayout ) = load
        (storeOp, finalLayout   ) = store

-- | Creates an attachment description given
-- | (loadOp , initialLayout) and
-- | (storeOp, finalLayout  ).
mkAttachmentDescription
  :: (VkAttachmentLoadOp, VkImageLayout)
  -> (VkAttachmentStoreOp, VkImageLayout)
  -> VkAttachmentDescription
mkAttachmentDescription load store = createVk @VkAttachmentDescription
   $ set                @"flags"          |* VK_ZERO_FLAGS
  &* set                @"format"         |* VK_FORMAT_B8G8R8A8_SRGB
  &* set                @"samples"        |* VK_SAMPLE_COUNT_1_BIT
  &* set                @"loadOp"         |* loadOp
  &* set                @"storeOp"        |* storeOp
  &* set                @"stencilLoadOp"  |* VK_ATTACHMENT_LOAD_OP_DONT_CARE
  &* set                @"stencilStoreOp" |* VK_ATTACHMENT_STORE_OP_DONT_CARE
  &* set                @"initialLayout"  |* initialLayout
  &* set                @"finalLayout"    |* finalLayout
  where (loadOp , initialLayout ) = load
        (storeOp, finalLayout   ) = store

-- | Creates a subpass dependency given the dependency flags, the
-- | source subpass number, stage mask and access flags, and the
-- | destination subpass number, stage mask and access flags.
mkSubpassDependency
  :: VkDependencyFlags
  -> (Word32, VkPipelineStageFlags, VkAccessFlags)
  -> (Word32, VkPipelineStageFlags, VkAccessFlags)
  -> VkSubpassDependency
mkSubpassDependency dependencyFlags src dst = createVk @VkSubpassDependency
   $ set @"srcSubpass"      |* srcSubpass
  &* set @"dstSubpass"      |* dstSubpass
  &* set @"srcStageMask"    |* srcStageMask
  &* set @"dstStageMask"    |* dstStageMask
  &* set @"srcAccessMask"   |* srcAccessMask
  &* set @"dstAccessMask"   |* dstAccessMask
  &* set @"dependencyFlags" |* dependencyFlags
  where (srcSubpass, srcStageMask, srcAccessMask) = src
        (dstSubpass, dstStageMask, dstAccessMask) = dst

-- | Pattern synonyms for various Vulkan constants,
-- | to be used with mkAttachmentDescription.

pattern LOAD  = VK_ATTACHMENT_LOAD_OP_LOAD
pattern CLEAR = VK_ATTACHMENT_LOAD_OP_CLEAR
pattern STORE = VK_ATTACHMENT_STORE_OP_STORE
pattern LOAD_DONT_CARE  = VK_ATTACHMENT_LOAD_OP_DONT_CARE
pattern STORE_DONT_CARE = VK_ATTACHMENT_STORE_OP_DONT_CARE
pattern UNDEFINED = VK_IMAGE_LAYOUT_UNDEFINED
pattern PRESENT   = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR
pattern INPUT     = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

-- | Pattern synonyms for various Vulkan constants,
-- | to be used with mkSubpassDependency.

pattern EXTERNAL  = VK_SUBPASS_EXTERNAL
pattern GLOBAL    = VK_ZERO_FLAGS
pattern BY_REGION = VK_DEPENDENCY_BY_REGION_BIT
pattern COLOR_ATTACHMENT_OUTPUT = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
pattern FRAGMENT_SHADER         = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT
pattern COLOR_ATTACHMENT_READ   = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT
pattern COLOR_ATTACHMENT_WRITE  = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT
pattern INPUT_ATTACHMENT_READ   = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT
