{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.Images where

import Vk

import Pipeline.Images.Info

-- | Creates a standard linear sampler.
createSampler
  :: VkDevice
  -> IO (Ptr VkSampler)
createSampler device = do
  ptr <- malloc
  vkCreateSampler
    |- device
    |- p samplerCreateInfo
    |- VK_NULL
    |- ptr
  return ptr

-- | Creates a color attachment image, imageView and associated allocation.
createColorAttachmentImage
  :: VkDevice
  -> VmaAllocator
  -> GraphicsQueueFamily
  -> (Word32, Word32)     -- (width, height)
  -> IO Attachment
createColorAttachmentImage device allocator queueFamilyIndex (width, height) = do
  let imageCreateInfo = mkImageCreateInfo
                          |- ColorAttachmentImage
                          |- ATTACHMENT_FORMAT
                          |- (width, height)
                          |- queueFamilyIndex
  ( image, allocation, _ ) <- perform3 $ vmaCreateImage
                                           |- allocator
                                           |- p imageCreateInfo
                                           |- p allocationCreateInfo
  let imageViewCreateInfo = mkImageViewCreateInfo
                              |- image
                              |- ATTACHMENT_FORMAT
  imageView <- perform $ vkCreateImageView
                           |- device
                           |- p imageViewCreateInfo
                           |- VK_NULL
  return $ Attachment { image
                      , imageView
                      , allocation
                      }

-- | Creates a sample image, imageView and associated allocation.
createSampleImage
  :: VkDevice
  -> VmaAllocator
  -> GraphicsQueueFamily
  -> (Word32, Word32)     -- (width, height)
  -> IO Attachment
createSampleImage device allocator queueFamilyIndex (width, height) = do
  let imageCreateInfo = mkImageCreateInfo
                          |- SampleImage
                          |- SAMPLE_FORMAT
                          |- (width, height)
                          |- queueFamilyIndex
  ( image, allocation, _ ) <- perform3 $ vmaCreateImage
                                           |- allocator
                                           |- p imageCreateInfo
                                           |- p allocationCreateInfo
  let imageViewCreateInfo = mkImageViewCreateInfo
                              |- image
                              |- SAMPLE_FORMAT
  imageView <- perform $ vkCreateImageView
                           |- device
                           |- p imageViewCreateInfo
                           |- VK_NULL
  return $ Attachment { image
                      , imageView
                      , allocation
                      }
