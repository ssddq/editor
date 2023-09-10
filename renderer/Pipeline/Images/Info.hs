{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Pipeline.Images.Info where

import Vk

import Data.Bits

-- | Sampler create info for a simple linear sampler
samplerCreateInfo :: VkSamplerCreateInfo
samplerCreateInfo = createVk @VkSamplerCreateInfo
   $ set @"sType"                   |* VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  &* set @"pNext"                   |* VK_NULL
  &* set @"flags"                   |* VK_ZERO_FLAGS
  &* set @"magFilter"               |* VK_FILTER_LINEAR
  &* set @"minFilter"               |* VK_FILTER_LINEAR
  &* set @"mipmapMode"              |* VK_SAMPLER_MIPMAP_MODE_NEAREST
  &* set @"addressModeU"            |* VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  &* set @"addressModeV"            |* VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  &* set @"addressModeW"            |* VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
  &* set @"mipLodBias"              |* 0
  &* set @"anisotropyEnable"        |* VK_FALSE
  &* set @"maxAnisotropy"           |* 16.0
  &* set @"compareEnable"           |* VK_FALSE
  &* set @"compareOp"               |* VK_COMPARE_OP_ALWAYS
  &* set @"minLod"                  |* 0
  &* set @"maxLod"                  |* 0
  &* set @"borderColor"             |* VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
  &* set @"unnormalizedCoordinates" |* VK_TRUE

pattern SAMPLE_FORMAT :: VkFormat
pattern SAMPLE_FORMAT = VK_FORMAT_B8G8R8A8_UNORM

-- | ImageView create info for a standard 2D image view into a given image.
mkImageViewCreateInfo
  :: VkImage
  -> VkFormat
  -> VkImageViewCreateInfo
mkImageViewCreateInfo image format = createVk @VkImageViewCreateInfo
   $ set @"sType"            |* VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  &* set @"pNext"            |* VK_NULL
  &* set @"flags"            |* VK_ZERO_FLAGS
  &* set @"image"            |* image
  &* set @"viewType"         |* VK_IMAGE_VIEW_TYPE_2D
  &* set @"format"           |* format
  &* set @"components"       |* components
  &* set @"subresourceRange" |* subresourceRange
  where components = createVk @VkComponentMapping
          $ set @"r"                |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"g"                |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"b"                |* VK_COMPONENT_SWIZZLE_IDENTITY
         &* set @"a"                |* VK_COMPONENT_SWIZZLE_IDENTITY
        subresourceRange = createVk @VkImageSubresourceRange
          $ set @"aspectMask"       |* VK_IMAGE_ASPECT_COLOR_BIT
         &* set @"baseMipLevel"     |* 0
         &* set @"levelCount"       |* 1
         &* set @"baseArrayLayer"   |* 0
         &* set @"layerCount"       |* 1

-- | Allocation create info for a standard VMA allocation.
allocationCreateInfo :: VmaAllocationCreateInfo
allocationCreateInfo = createVk @VmaAllocationCreateInfo
   $ set @"flags"           |* VMA_ALLOCATION_CREATE_DEDICATED_MEMORY_BIT -- VMA_ALLOCATION_CREATE_HOST_ACCESS_RANDOM_BIT
  &* set @"usage"           |* VMA_MEMORY_USAGE_AUTO
  &* set @"requiredFlags"   |* VK_ZERO_FLAGS
  &* set @"preferredFlags"  |* VK_ZERO_FLAGS
  &* set @"memoryTypeBits"  |* 0
  &* set @"pool"            |* VK_NULL_HANDLE
  &* set @"pUserData"       |* VK_NULL
  &* set @"priority"        |* 1


-- | Image create info for either a color attachment image or a sample image,
-- | with specified width and height.

data ImageType = ColorAttachmentImage | SampleImage

mkImageCreateInfo
  :: ImageType
  -> VkFormat
  -> (Word32, Word32)     -- (width, height)
  -> GraphicsQueueFamily
  -> VkImageCreateInfo
mkImageCreateInfo imageType format (width, height) queueFamilyIndex = createVk @VkImageCreateInfo
   $ set                @"sType"                    |* VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  &* set                @"pNext"                    |* VK_NULL
  &* set                @"flags"                    |* VK_ZERO_FLAGS
  &* set                @"imageType"                |* VK_IMAGE_TYPE_2D
  &* set                @"format"                   |* format
  &* set                @"extent"                   |* extent
  &* set                @"mipLevels"                |* 1
  &* set                @"arrayLayers"              |* 1
  &* set                @"samples"                  |* VK_SAMPLE_COUNT_1_BIT
  &* set                @"tiling"                   |* VK_IMAGE_TILING_OPTIMAL
  &* set                @"sharingMode"              |* VK_SHARING_MODE_EXCLUSIVE
  &* setListCountAndRef @"queueFamilyIndexCount" -- |*
                        @"pQueueFamilyIndices"      |* [ queueFamilyIndex ]
  &* set                @"usage"                    |* usageBits imageType
  &* set                @"initialLayout"            |* VK_IMAGE_LAYOUT_UNDEFINED
  where usageBits ColorAttachmentImage = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT .|.
                                         VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
        usageBits SampleImage = VK_IMAGE_USAGE_SAMPLED_BIT     .|.
                                usageBits ColorAttachmentImage
        extent = createVk @VkExtent3D
          $ set @"width"  |* width
         &* set @"height" |* height
         &* set @"depth"  |* 1

