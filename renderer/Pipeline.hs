{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}

module Pipeline where

import Vk

import Pipeline.Descriptor.Info (Resource(..))

import Pipeline.Info

import Pipeline.Descriptor qualified as Descriptor
import Pipeline.Graphics   qualified as Graphics
import Pipeline.Images     qualified as Images
import Pipeline.RenderPass qualified as RenderPass

-- | Creates the RenderPipeline for the program with two renderpasses,
-- | the first containing two subpasses and the second containing a single subpass.
-- | Also creates color attachment and sample image for these passes.

createRenderPipeline
  :: (Shaders, Shaders, Shaders, Shaders)
  -> Vk    { renderPipeline = X, vulkan = I }
  -> IO Vk { renderPipeline = I, vulkan = I }
createRenderPipeline (shaders0, shaders1, shaders2, shaders3) vk = do
  sampler <- Images.createSampler
                      |- device
  colorAttachment <- Images.createColorAttachmentImage
                              |- device
                              |- allocator
                              |- queueFamilyIndex
                              |- VK_FORMAT_R16G16_SFLOAT
                              |- (render.width, render.height)
  sampleAttachment <- Images.createSampleImage
                               |- device
                               |- allocator
                               |- queueFamilyIndex
                               |- (render.width, render.height)
  descriptors0 <- Descriptor.empty
  descriptors1 <- Descriptor.createDescriptors
                               |- device
                               |- [ InputAttachment 1 colorAttachment.imageView ]
  descriptors2 <- Descriptor.createDescriptors
                               |- device
                               |- [ ImageSampler 1 sampleAttachment.imageView sampler ]
  vkRenderPass0 <- RenderPass.createRenderPass
                                |- device
  vkRenderPass1 <- RenderPass.createRenderPass1
                                |- device
  pipelines <- Graphics.createPipelines
                 |- device
                 |- (descriptors0 , descriptors1 , descriptors2)
                 |- (shaders0     , shaders1     , shaders2    , shaders3)
                 |- (vkRenderPass0, vkRenderPass1)
  let draw0    = Subpass { pipeline    = pipelines.draw0
                         , descriptors = descriptors0
                         }
      resolve0 = Subpass { pipeline    = pipelines.resolve0
                         , descriptors = descriptors1
                         }
      clear0   = Subpass { pipeline    = pipelines.clear0
                         , descriptors = descriptors0
                         }
      draw1    = Subpass { pipeline    = pipelines.draw1
                         , descriptors = descriptors0
                         }
      resolve1 = Subpass { pipeline    = pipelines.resolve1
                         , descriptors = descriptors1
                         }
      clear1   = Subpass { pipeline    = pipelines.clear1
                         , descriptors = descriptors0
                         }
      draw2    = Subpass { pipeline    = pipelines.draw2
                         , descriptors = descriptors0
                         }
      resolve2 = Subpass { pipeline    = pipelines.resolve2
                         , descriptors = descriptors1
                         }
      clear2   = Subpass { pipeline    = pipelines.clear2
                         , descriptors = descriptors0
                         }
      draw3    = Subpass { pipeline    = pipelines.draw3
                         , descriptors = descriptors0
                         }
      resolve3 = Subpass { pipeline    = pipelines.resolve3
                         , descriptors = descriptors1
                         }
      aa       = Subpass { pipeline    = pipelines.aa
                         , descriptors = descriptors2
                         }
      renderPass0 = RenderPass0 { handle = vkRenderPass0
                                , draw0, resolve0, clear0
                                , draw1, resolve1, clear1
                                , draw2, resolve2, clear2
                                , draw3, resolve3
                                }
      renderPass1 = RenderPass1 { handle   = vkRenderPass1
                                , aa
                                }
  return $ vk { renderPipeline = RenderPipeline { renderPass0
                                                , renderPass1
                                                , colorAttachment
                                                , sampleAttachment
                                                , sampler
                                                }
              }
  where Vk        {..} = vk
        Vulkan    {..} = vulkan
        Constants {..} = constants
