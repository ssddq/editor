{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Pipeline.Info where

import Vk

import Graphics.Vulkan.Ext.VK_KHR_swapchain

-- * Global render pass info

data AAPassInfo = AAPassInfo
  { aa           :: {-# UNPACK #-} !SubpassInfo
  , dependencies :: [VkSubpassDependency]
  , attachment0  :: {-# UNPACK #-} !AttachmentInfo
  , attachment1  :: {-# UNPACK #-} !AttachmentInfo
  , attachment2  :: {-# UNPACK #-} !AttachmentInfo
  }

data AttachmentInfo = AttachmentInfo
  { format  :: {-# UNPACK #-} !VkFormat
  , initial :: {-# UNPACK #-} !VkImageLayout
  , final   :: {-# UNPACK #-} !VkImageLayout
  , loadOp  :: {-# UNPACK #-} !VkAttachmentLoadOp
  , storeOp :: {-# UNPACK #-} !VkAttachmentStoreOp
  }

data DrawPassInfo = DrawPassInfo
  { draw0        :: {-# UNPACK #-} !SubpassInfo
  , resolve0     :: {-# UNPACK #-} !SubpassInfo
  , clear0       :: {-# UNPACK #-} !SubpassInfo
  , draw1        :: {-# UNPACK #-} !SubpassInfo
  , resolve1     :: {-# UNPACK #-} !SubpassInfo
  , clear1       :: {-# UNPACK #-} !SubpassInfo
  , draw2        :: {-# UNPACK #-} !SubpassInfo
  , resolve2     :: {-# UNPACK #-} !SubpassInfo
  , clear2       :: {-# UNPACK #-} !SubpassInfo
  , draw3        :: {-# UNPACK #-} !SubpassInfo
  , resolve3     :: {-# UNPACK #-} !SubpassInfo
  , dependencies :: [VkSubpassDependency]
  , attachment0  :: {-# UNPACK #-} !AttachmentInfo
  , attachment1  :: {-# UNPACK #-} !AttachmentInfo
  , attachment2  :: {-# UNPACK #-} !AttachmentInfo
  }

data DrawSubpass = DrawSubpass
  { draw    :: {-# UNPACK #-} !SubpassInfo
  , resolve :: {-# UNPACK #-} !SubpassInfo
  }

data Pipelines = Pipelines
  { draw0    :: {-# UNPACK #-} !Pipeline
  , resolve0 :: {-# UNPACK #-} !Pipeline
  , clear0   :: {-# UNPACK #-} !Pipeline
  , draw1    :: {-# UNPACK #-} !Pipeline
  , resolve1 :: {-# UNPACK #-} !Pipeline
  , clear1   :: {-# UNPACK #-} !Pipeline
  , draw2    :: {-# UNPACK #-} !Pipeline
  , resolve2 :: {-# UNPACK #-} !Pipeline
  , clear2   :: {-# UNPACK #-} !Pipeline
  , draw3    :: {-# UNPACK #-} !Pipeline
  , resolve3 :: {-# UNPACK #-} !Pipeline
  , aa       :: {-# UNPACK #-} !Pipeline
  }

data RenderPipelineInfo = RenderPipelineInfo
  { drawPass :: {-# UNPACK #-} !DrawPassInfo
  , aaPass   :: {-# UNPACK #-} !AAPassInfo
  }

data SubpassInfo = SubpassInfo
  { description :: {-# UNPACK #-} !VkSubpassDescription
  }


aaPassInfo :: AAPassInfo
aaPassInfo = AAPassInfo
  { aa = aaSubpassInfo
  , dependencies = [dependency]
  , attachment0
  , attachment1
  , attachment2
  }
  where aaSubpassInfo = SubpassInfo aaSubpassDescription
        aaSubpassDescription = mkSubpassDescription
                                 |- [inputReference 2]
                                 |- [colorReference 0]
        dependency = mkSubpassDependency
                |- GLOBAL
                |- (EXTERNAL, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
                |- (0       , FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ )
        attachment0 = AttachmentInfo
                        { format  = VK_FORMAT_B8G8R8A8_UNORM
                        , initial = UNDEFINED
                        , final   = PRESENT
                        , loadOp  = CLEAR
                        , storeOp = STORE
                        }
        attachment1 = AttachmentInfo
                        { format  = VK_FORMAT_R16G16_SFLOAT
                        , initial = INPUT
                        , final   = INPUT
                        , loadOp  = LOAD
                        , storeOp = STORE_DONT_CARE
                        }
        attachment2 = AttachmentInfo
                        { format  = VK_FORMAT_B8G8R8A8_UNORM
                        , initial = INPUT
                        , final   = INPUT
                        , loadOp  = LOAD
                        , storeOp = STORE
                        }

drawPassInfo :: DrawPassInfo
drawPassInfo = DrawPassInfo
  { draw0    = draw    -- 0
  , resolve0 = resolve -- 1
  , clear0   = clear   -- 2
  , draw1    = draw    -- 3
  , resolve1 = resolve -- 4
  , clear1   = clear   -- 5
  , draw2    = draw    -- 6
  , resolve2 = resolve -- 7
  , clear2   = clear   -- 8
  , draw3    = draw    -- 9
  , resolve3 = resolve -- 10
  , dependencies = [ drawResolveDep 0 1, resolveClearDep 1 2, clearDrawDep 2 3
                   , drawResolveDep 3 4, resolveClearDep 4 5, clearDrawDep 5 6
                   , drawResolveDep 6 7, resolveClearDep 7 8, clearDrawDep 8 9
                   , drawResolveDep 9 10
                   , resolveResolveDep 1 4, resolveResolveDep 4 7, resolveResolveDep 7 10
                   ]
  , attachment0
  , attachment1
  , attachment2
  }
  where clear = SubpassInfo clearSubpassDescription
        clearSubpassDescription = mkSubpassDescription
                                    |- []
                                    |- [colorReference 0, colorReference 1]
        draw = SubpassInfo drawDescription
        resolve = SubpassInfo resolveDescription
        drawDescription = mkSubpassDescription
                            |- []
                            |- [colorReference 0, colorReference 1]
        resolveDescription = mkSubpassDescription
                               |- [inputReference 0, inputReference 1]
                               |- [colorReference 2]
        drawResolveDep d r = mkSubpassDependency
                               |- BY_REGION
                               |- (d, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
                               |- (r, FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ )
        resolveClearDep r c = mkSubpassDependency
                                |- BY_REGION
                                |- (r, FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ )
                                |- (c, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
        clearDrawDep c d = mkSubpassDependency
                             |- BY_REGION
                             |- (c, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
                             |- (d, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
        resolveResolveDep r1 r2 = mkSubpassDependency
                              |- BY_REGION
                              |- (r1, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
                              |- (r2, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
        attachment0 = AttachmentInfo
                        { format = VK_FORMAT_R16G16_SFLOAT
                        , initial = UNDEFINED
                        , final = INPUT
                        , loadOp = CLEAR
                        , storeOp = STORE
                        }
        attachment1 = AttachmentInfo
                        { format = VK_FORMAT_R8G8B8A8_UNORM
                        , initial = UNDEFINED
                        , final = INPUT
                        , loadOp = CLEAR
                        , storeOp = STORE
                        }
        attachment2 = AttachmentInfo
                        { format = VK_FORMAT_B8G8R8A8_UNORM
                        , initial = UNDEFINED
                        , final = INPUT
                        , loadOp = CLEAR
                        , storeOp = STORE
                        }

-- | A lot of interdependent information about the render pipeline
-- | is used throughout the renderer code.
-- | Better to just declare it in one space
-- | and have everything reference this.

{-# INLINE renderPipelineInfo #-}
renderPipelineInfo :: RenderPipelineInfo
renderPipelineInfo = RenderPipelineInfo { drawPass = drawPassInfo, aaPass = aaPassInfo }



-- * Info creation functions

pattern BY_REGION = VK_DEPENDENCY_BY_REGION_BIT

pattern CLEAR = VK_ATTACHMENT_LOAD_OP_CLEAR

pattern COLOR_ATTACHMENT_OUTPUT = VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT

pattern COLOR_ATTACHMENT_READ   = VK_ACCESS_COLOR_ATTACHMENT_READ_BIT

pattern COLOR_ATTACHMENT_WRITE  = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT

-- | Pattern synonyms for various Vulkan constants,
-- | to be used with mkSubpassDependency.

pattern EXTERNAL  = VK_SUBPASS_EXTERNAL

pattern FRAGMENT_SHADER         = VK_PIPELINE_STAGE_FRAGMENT_SHADER_BIT

pattern GLOBAL    = VK_ZERO_FLAGS

pattern INPUT     = VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL

pattern INPUT_ATTACHMENT_READ   = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT

-- | Pattern synonyms for various Vulkan constants,
-- | to be used with mkAttachmentDescription.

pattern LOAD  = VK_ATTACHMENT_LOAD_OP_LOAD

pattern LOAD_DONT_CARE  = VK_ATTACHMENT_LOAD_OP_DONT_CARE

pattern PRESENT   = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR

pattern STORE = VK_ATTACHMENT_STORE_OP_STORE

pattern STORE_DONT_CARE = VK_ATTACHMENT_STORE_OP_DONT_CARE

pattern UNDEFINED = VK_IMAGE_LAYOUT_UNDEFINED

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

-- | Creates an attachment description given
-- | (loadOp , initialLayout) and
-- | (storeOp, finalLayout  ).

mkAttachmentDescription
  :: VkFormat
  -> (VkAttachmentLoadOp, VkImageLayout)
  -> (VkAttachmentStoreOp, VkImageLayout)
  -> VkAttachmentDescription
mkAttachmentDescription format load store = createVk @VkAttachmentDescription
   $ set                @"flags"          |* VK_ZERO_FLAGS
  &* set                @"format"         |* format
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
