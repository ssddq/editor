{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Pipeline.Descriptor.Info where

import Vk

-- | Sets the create info for a uniform buffer descriptor,
-- | with specified buffer size. Cannot be shared between queue families.
mkBufferCreateInfo
  :: Word64
  -> VkBufferCreateInfo
mkBufferCreateInfo size = createVk @VkBufferCreateInfo
   $ set                @"sType"                   |* VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  &* set                @"pNext"                   |* VK_NULL
  &* set                @"flags"                   |* VK_ZERO_FLAGS
  &* set                @"size"                    |* VkDeviceSize size
  &* set                @"usage"                   |* VK_BUFFER_USAGE_UNIFORM_BUFFER_BIT
  &* set                @"sharingMode"             |* VK_SHARING_MODE_EXCLUSIVE
  &* setListCountAndRef @"queueFamilyIndexCount"-- |*
                        @"pQueueFamilyIndices"     |* []

-- | Sets the create info for the allocation for the uniform buffer.
-- | The allocation can be written to from the host,
-- | and is preferrably placed in device memory.
allocationCreateInfo :: VmaAllocationCreateInfo
allocationCreateInfo = createVk @VmaAllocationCreateInfo
   $ set                @"flags"               |* VMA_ALLOCATION_CREATE_HOST_ACCESS_SEQUENTIAL_WRITE_BIT
  &* set                @"usage"               |* VMA_MEMORY_USAGE_AUTO_PREFER_DEVICE
  &* set                @"requiredFlags"       |* VK_ZERO_FLAGS
  &* set                @"preferredFlags"      |* VK_ZERO_FLAGS
  &* set                @"memoryTypeBits"      |* 0
  &* set                @"pool"                |* VK_NULL_HANDLE
  &* set                @"pUserData"           |* VK_NULL
  &* set                @"priority"            |* 1

data Resource = UniformBuffer   Word32 (VkBuffer)
              | InputAttachment Word32 (VkImageView)
              | ImageSampler    Word32 (VkImageView) (Ptr VkSampler)

-- | Layout bindings and pool sizes for uniform buffer,
-- | input attachment and image sampler descriptor sets.
-- | For uniform buffers, a single descriptor is bound in position 0;
-- | for input attachments, a single descriptor is bound in position 1, and
-- | for image samplers, a single descriptor is bound in position 1.
-- | Pools allow a single descriptor of each type.
-- |
-- | Ideally, this should eventually be handled automatically using
-- | some globally-specified pipeline layout data.

descriptorSetLayoutBinding
  :: Resource
  -> VkDescriptorSetLayoutBinding
descriptorSetLayoutBinding (UniformBuffer binding _buffer) = createVk @VkDescriptorSetLayoutBinding
   $ set                @"binding"             |* binding
  &* set                @"descriptorType"      |* VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  &* set                @"descriptorCount"     |* 1
  &* set                @"stageFlags"          |* VK_SHADER_STAGE_VERTEX_BIT
  &* set                @"pImmutableSamplers"  |* VK_NULL
descriptorSetLayoutBinding (InputAttachment binding _imageView) = createVk @VkDescriptorSetLayoutBinding
   $ set                @"binding"             |* binding
  &* set                @"descriptorType"      |* VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  &* set                @"descriptorCount"     |* 1
  &* set                @"stageFlags"          |* VK_SHADER_STAGE_FRAGMENT_BIT
  &* set                @"pImmutableSamplers"  |* VK_NULL
descriptorSetLayoutBinding (ImageSampler binding _imageView sampler) = createVk @VkDescriptorSetLayoutBinding
   $ set                @"binding"             |* binding
  &* set                @"descriptorType"      |* VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  &* set                @"descriptorCount"     |* 1
  &* set                @"stageFlags"          |* VK_SHADER_STAGE_FRAGMENT_BIT
  &* set                @"pImmutableSamplers"  |* sampler

descriptorPoolSize
  :: Resource
  -> VkDescriptorPoolSize
descriptorPoolSize (UniformBuffer _ _buffer) = createVk @VkDescriptorPoolSize
  $ set                @"type"                |* VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
 &* set                @"descriptorCount"     |* 1
descriptorPoolSize (InputAttachment _ _imageView) = createVk @VkDescriptorPoolSize
  $ set                @"type"                |* VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
 &* set                @"descriptorCount"     |* 1
descriptorPoolSize (ImageSampler _ _imageView _sampler) = createVk @VkDescriptorPoolSize
  $ set                @"type"                |* VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
 &* set                @"descriptorCount"     |* 1

-- | Sets the create info for a descriptor set layout
-- | consisting of the given bindings.
mkDescriptorSetLayoutCreateInfo
  :: [VkDescriptorSetLayoutBinding]
  -> (VkDescriptorSetLayoutCreateInfo, Int)
mkDescriptorSetLayoutCreateInfo bindings = (descriptorSetLayoutCreateInfo, bindingCount)
  where bindingCount = length bindings
        descriptorSetLayoutCreateInfo = createVk @VkDescriptorSetLayoutCreateInfo
          $ set        @"sType"        |* VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
         &* set        @"pNext"        |* VK_NULL
         &* set        @"flags"        |* VK_ZERO_FLAGS
         &* set        @"bindingCount" |* fromIntegral bindingCount
         &* setListRef @"pBindings"    |* bindings

-- | Sets the create info for a descriptor pool
-- | with the given pool sizes.

mkDescriptorPoolCreateInfo
  :: [VkDescriptorPoolSize]
  -> VkDescriptorPoolCreateInfo
mkDescriptorPoolCreateInfo sizes = createVk @VkDescriptorPoolCreateInfo
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  &* set                @"pNext"               |* VK_NULL
  &* set                @"flags"               |* VK_ZERO_FLAGS
  &* set                @"maxSets"             |* fromIntegral (length sizes)
  &* setListCountAndRef @"poolSizeCount"--     |*
                        @"pPoolSizes"          |* sizes

-- | Sets the allocate info for a descriptor set,
-- | with a single pool and layout.
mkDescriptorSetAllocateInfo
  :: VkDescriptorPool
  -> VkDescriptorSetLayout
  -> VkDescriptorSetAllocateInfo
mkDescriptorSetAllocateInfo pool layout = descriptorSetAllocateInfo
  where descriptorSetAllocateInfo = createVk @VkDescriptorSetAllocateInfo
           $ set        @"sType"               |* VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
          &* set        @"pNext"               |* VK_NULL
          &* set        @"descriptorPool"      |* pool
          &* set        @"descriptorSetCount"  |* 1
          &* setListRef @"pSetLayouts"         |* [layout]


descriptorBufferInfo
  :: VkBuffer
  -> VkDescriptorBufferInfo
descriptorBufferInfo buffer = createVk @VkDescriptorBufferInfo
   $ set                @"buffer"              |* buffer
  &* set                @"offset"              |* VkDeviceSize 0
  &* set                @"range"               |* VkDeviceSize VK_WHOLE_SIZE

descriptorImageInfo
  :: VkImageView
  -> VkDescriptorImageInfo
descriptorImageInfo imageView = createVk @VkDescriptorImageInfo
   $ set                @"sampler"             |* VK_NULL_HANDLE
  &* set                @"imageLayout"         |* VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
  &* set                @"imageView"           |* imageView

-- | Creates the VkWriteDescriptorSet required to
-- | write and update the given descriptor set with the
-- | specified resource.
mkWriteDescriptorSet
  :: VkDescriptorSet
  -> Resource
  -> VkWriteDescriptorSet
mkWriteDescriptorSet descriptorSet (UniformBuffer binding buffer) = createVk @VkWriteDescriptorSet
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  &* set                @"pNext"               |* VK_NULL
  &* set                @"dstSet"              |* descriptorSet
  &* set                @"dstBinding"          |* binding
  &* set                @"dstArrayElement"     |* 0
  &* set                @"descriptorCount"     |* 1
  &* set                @"descriptorType"      |* VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER
  &* set                @"pImageInfo"          |* VK_NULL
  &* setVkRef           @"pBufferInfo"         |* descriptorBufferInfo buffer
  &* set                @"pTexelBufferView"    |* VK_NULL
mkWriteDescriptorSet descriptorSet (InputAttachment binding imageView)= createVk @VkWriteDescriptorSet
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  &* set                @"pNext"               |* VK_NULL
  &* set                @"dstSet"              |* descriptorSet
  &* set                @"dstBinding"          |* binding
  &* set                @"dstArrayElement"     |* 0
  &* set                @"descriptorCount"     |* 1
  &* set                @"descriptorType"      |* VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT
  &* setVkRef           @"pImageInfo"          |* descriptorImageInfo imageView
  &* set                @"pBufferInfo"         |* VK_NULL
  &* set                @"pTexelBufferView"    |* VK_NULL
mkWriteDescriptorSet descriptorSet (ImageSampler binding imageView _sampler) = createVk @VkWriteDescriptorSet
   $ set                @"sType"                   |* VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  &* set                @"pNext"                   |* VK_NULL
  &* set                @"dstSet"                  |* descriptorSet
  &* set                @"dstBinding"              |* binding
  &* set                @"dstArrayElement"         |* 0
  &* set                @"descriptorCount"         |* 1
  &* set                @"descriptorType"          |* VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
  &* setVkRef           @"pImageInfo"              |* descriptorImageInfo imageView
  &* set                @"pBufferInfo"             |* VK_NULL
  &* set                @"pTexelBufferView"        |* VK_NULL
