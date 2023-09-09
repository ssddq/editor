{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}

module Pipeline where

import Vk

import Pipeline.Descriptor.Info (Resource(..))

import Pipeline.Descriptor qualified as Descriptor
import Pipeline.Graphics   qualified as Graphics
import Pipeline.Images     qualified as Images
import Pipeline.RenderPass qualified as RenderPass

-- | Creates the RenderPipeline for the program with two renderpasses,
-- | the first containing two subpasses and the second containing a single subpass.
-- | Also creates color attachment and sample image for these passes.

createRenderPipeline
  :: (Shaders, Shaders, Shaders)
  -> Vk    { renderPipeline = X, vulkan = I }
  -> IO Vk { renderPipeline = I, vulkan = I }
createRenderPipeline (shaders0, shaders1, shaders2) vk = do
  sampler <- Images.createSampler
                      |- device
  colorAttachment  <- Images.createColorAttachmentImage
                               |- device
                               |- allocator
                               |- queueFamilyIndex
                               |- (render.width, render.height)
  sampleAttachment <- Images.createSampleImage
                               |- device
                               |- allocator
                               |- queueFamilyIndex
                               |- (render.width, render.height)
  descriptors0 <- Descriptor.empty
  descriptors1 <- Descriptor.createDescriptors
                               |- device
                               |- [InputAttachment colorAttachment.imageView]
  descriptors2 <- Descriptor.createDescriptors
                               |- device
                               |- [ImageSampler sampleAttachment.imageView sampler]
  vkRenderPass0 <- RenderPass.createRenderPass
                                |- device
  vkRenderPass1 <- RenderPass.createRenderPass1
                                |- device
  (pipeline0, pipeline1, pipeline2) <- Graphics.createPipelines
                                                  |- device
                                                  |- (descriptors0 , descriptors1 , descriptors2)
                                                  |- (shaders0     , shaders1     , shaders2    )
                                                  |- (vkRenderPass0, vkRenderPass1)
  let subpass0 = Subpass { pipeline    = pipeline0
                         , descriptors = descriptors0
                         }
      subpass1 = Subpass { pipeline    = pipeline1
                         , descriptors = descriptors1
                         }
      subpass2 = Subpass { pipeline    = pipeline2
                         , descriptors = descriptors2
                         }
      renderPass0 = RenderPass0 { handle   = vkRenderPass0
                                , subpass0 = subpass0
                                , subpass1 = subpass1
                                }
      renderPass1 = RenderPass1 { handle   = vkRenderPass1
                                , subpass0 = subpass2
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
