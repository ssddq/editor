{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Pipeline.Graphics.Info where

import Vk

import Data.Binary.Get
import Data.Bits

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL

-- | Additive color attachment color and alpha blending.

colorBlendAdd2 :: VkPipelineColorBlendStateCreateInfo
colorBlendAdd2 = createVk @VkPipelineColorBlendStateCreateInfo
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  &* set                @"pNext"               |* VK_NULL
  &* set                @"flags"               |* VK_ZERO_FLAGS
  &* set                @"logicOpEnable"       |* VK_FALSE
  &* set                @"logicOp"             |* VK_LOGIC_OP_COPY
  &* setListCountAndRef @"attachmentCount"  -- |*
                        @"pAttachments"        |* [colorBlendAttachmentState0, colorBlendAttachmentState1]
  &* setAt              @"blendConstants" @0   |* 0
  &* setAt              @"blendConstants" @1   |* 0
  &* setAt              @"blendConstants" @2   |* 0
  &* setAt              @"blendConstants" @3   |* 0
  where colorBlendAttachmentState0 = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_TRUE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD
        colorBlendAttachmentState1 = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_FALSE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD

colorBlendSrc2 :: VkPipelineColorBlendStateCreateInfo
colorBlendSrc2 = createVk @VkPipelineColorBlendStateCreateInfo
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  &* set                @"pNext"               |* VK_NULL
  &* set                @"flags"               |* VK_ZERO_FLAGS
  &* set                @"logicOpEnable"       |* VK_FALSE
  &* set                @"logicOp"             |* VK_LOGIC_OP_COPY
  &* setListCountAndRef @"attachmentCount"  -- |*
                        @"pAttachments"        |* [colorBlendAttachmentState0, colorBlendAttachmentState1]
  &* setAt              @"blendConstants" @0   |* 0
  &* setAt              @"blendConstants" @1   |* 0
  &* setAt              @"blendConstants" @2   |* 0
  &* setAt              @"blendConstants" @3   |* 0
  where colorBlendAttachmentState0 = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_TRUE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD
        colorBlendAttachmentState1 = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_FALSE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD

colorBlendAdd1 :: VkPipelineColorBlendStateCreateInfo
colorBlendAdd1 = createVk @VkPipelineColorBlendStateCreateInfo
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  &* set                @"pNext"               |* VK_NULL
  &* set                @"flags"               |* VK_ZERO_FLAGS
  &* set                @"logicOpEnable"       |* VK_FALSE
  &* set                @"logicOp"             |* VK_LOGIC_OP_COPY
  &* setListCountAndRef @"attachmentCount"  -- |*
                        @"pAttachments"        |* [colorBlendAttachmentState]
  &* setAt              @"blendConstants" @0   |* 0
  &* setAt              @"blendConstants" @1   |* 0
  &* setAt              @"blendConstants" @2   |* 0
  &* setAt              @"blendConstants" @3   |* 0
  where colorBlendAttachmentState = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_TRUE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD

colorBlendSrcAlpha1 :: VkPipelineColorBlendStateCreateInfo
colorBlendSrcAlpha1 = createVk @VkPipelineColorBlendStateCreateInfo
   $ set                @"sType"               |* VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  &* set                @"pNext"               |* VK_NULL
  &* set                @"flags"               |* VK_ZERO_FLAGS
  &* set                @"logicOpEnable"       |* VK_FALSE
  &* set                @"logicOp"             |* VK_LOGIC_OP_COPY
  &* setListCountAndRef @"attachmentCount"  -- |*
                        @"pAttachments"        |* [colorBlendAttachmentState]
  &* setAt              @"blendConstants" @0   |* 0
  &* setAt              @"blendConstants" @1   |* 0
  &* setAt              @"blendConstants" @2   |* 0
  &* setAt              @"blendConstants" @3   |* 0
  where colorBlendAttachmentState = createVk @VkPipelineColorBlendAttachmentState
          $ set @"colorWriteMask"       |*      VK_COLOR_COMPONENT_R_BIT .|. VK_COLOR_COMPONENT_G_BIT
                                            .|. VK_COLOR_COMPONENT_B_BIT .|. VK_COLOR_COMPONENT_A_BIT
         &* set @"blendEnable"          |* VK_TRUE
         &* set @"srcColorBlendFactor"  |* VK_BLEND_FACTOR_SRC_ALPHA
         &* set @"dstColorBlendFactor"  |* VK_BLEND_FACTOR_ONE_MINUS_SRC_ALPHA
         &* set @"colorBlendOp"         |* VK_BLEND_OP_ADD
         &* set @"srcAlphaBlendFactor"  |* VK_BLEND_FACTOR_ONE
         &* set @"dstAlphaBlendFactor"  |* VK_BLEND_FACTOR_ZERO
         &* set @"alphaBlendOp"         |* VK_BLEND_OP_ADD

-- | Creates a VkShaderModule for a given logical device
-- | from compiled shader bytecode.

createShaderModule
  :: VkDevice
  -> BS.ByteString
  -> IO VkShaderModule
createShaderModule device code = do
  let shaderModuleCreateInfo = mkShaderModuleCreateInfo
                                 |- code
  shader <- perform $ vkCreateShaderModule
                        |- device
                        |- p shaderModuleCreateInfo
                        |- VK_NULL
  return $ shader

-- | Fragment shader stage create info, for a given fragment shader module.

fragStageInfo
  :: VkShaderModule
  -> VkPipelineShaderStageCreateInfo
fragStageInfo fragModule = createVk @VkPipelineShaderStageCreateInfo
   $ set       @"sType"  |* VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  &* set       @"pNext"  |* VK_NULL
  &* set       @"stage"  |* VK_SHADER_STAGE_FRAGMENT_BIT
  &* set       @"module" |* fragModule
  &* setStrRef @"pName"  |* "main"

-- | Assembly state create info, stating that vertices should be
-- | assembled as triangles.

inputAssemblyState :: VkPipelineInputAssemblyStateCreateInfo
inputAssemblyState = createVk @VkPipelineInputAssemblyStateCreateInfo
   $ set @"sType"                  |* VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  &* set @"pNext"                  |* VK_NULL
  &* set @"topology"               |* VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
  &* set @"primitiveRestartEnable" |* VK_FALSE

-- | Graphics pipelinle create info from given parameters
-- | with dynamic viewport and scissor, which must be set for each pipeline
-- | before any draw calls in each subpass of the command buffer.

mkGraphicsPipelineCreateInfo
  :: VkShaderModule                       -- vertex shader module
  -> VkShaderModule                       -- fragment shader module
  -> Word32                               -- subpass
  -> VkPipelineLayout
  -> VkRenderPass
  -> VkPipelineVertexInputStateCreateInfo
  -> VkPipelineColorBlendStateCreateInfo
  -> VkGraphicsPipelineCreateInfo
mkGraphicsPipelineCreateInfo vertModule fragModule subpass
                             layout     renderPass
                             vertexInputState colorBlendState =
  createVk @VkGraphicsPipelineCreateInfo
     $ set                @"sType"                |* VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
    &* set                @"pNext"                |* VK_NULL
    &* set                @"flags"                |* VK_ZERO_FLAGS
    &* setListCountAndRef @"stageCount"        -- |*
                          @"pStages"              |* [ vertStageInfo vertModule
                                                     , fragStageInfo fragModule
                                                     ]
    &* setVkRef           @"pVertexInputState"    |* vertexInputState
    &* setVkRef           @"pInputAssemblyState"  |* inputAssemblyState
    &* set                @"pTessellationState"   |* VK_NULL
    &* setVkRef           @"pViewportState"       |* viewportState
    &* setVkRef           @"pRasterizationState"  |* rasterizationState
    &* setVkRef           @"pMultisampleState"    |* multisampleState
    &* set                @"pDepthStencilState"   |* VK_NULL
    &* setVkRef           @"pColorBlendState"     |* colorBlendState
    &* setVkRef           @"pDynamicState"        |* dynamicState
    &* set                @"layout"               |* layout
    &* set                @"renderPass"           |* renderPass
    &* set                @"subpass"              |* subpass
    &* set                @"basePipelineHandle"   |* VK_NULL_HANDLE
    &* set                @"basePipelineIndex"    |* -1
    where viewportState = createVk @VkPipelineViewportStateCreateInfo
            $ set                @"sType"           |* VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
           &* set                @"pNext"           |* VK_NULL
           &* set                @"flags"           |* VK_ZERO_FLAGS
           &* setListCountAndRef @"viewportCount"-- |*
                                 @"pViewports"      |* [viewport]
           &* setListCountAndRef @"scissorCount" -- |*                                                     )
                                 @"pScissors"       |* [scissor]
          viewport = createVk @VkViewport
            $ set                @"x"               |* 0
           &* set                @"y"               |* 0
           &* set                @"width"           |* 0
           &* set                @"height"          |* 0
           &* set                @"minDepth"        |* 0
           &* set                @"maxDepth"        |* 1
          scissor = createVk @VkRect2D
            $ set @"offset" |* offset
           &* set @"extent" |* extent
          offset = createVk @VkOffset2D
            $ set @"x"      |* 0
           &* set @"y"      |* 0
          extent = createVk @VkExtent2D
            $ set @"width"  |* 0
           &* set @"height" |* 0
          dynamicState = createVk @VkPipelineDynamicStateCreateInfo
            $ set @"sType" |* VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
           &* set @"pNext" |* VK_NULL
           &* set @"flags" |* VK_ZERO_FLAGS
           &* setListCountAndRef @"dynamicStateCount" -- |*
                                 @"pDynamicStates"       |* [ VK_DYNAMIC_STATE_VIEWPORT
                                                            , VK_DYNAMIC_STATE_SCISSOR
                                                            ]

-- | Create info for a pipeline layout with a single (provided) descriptor set layout.
-- | Allows for push constants corresponding to the window size.

mkPipelineLayoutCreateInfo
  :: VkDescriptorSetLayout
  -> VkPipelineLayoutCreateInfo
mkPipelineLayoutCreateInfo VK_NULL_HANDLE = createVk @VkPipelineLayoutCreateInfo
   $ set                @"sType"                     |* VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  &* set                @"pNext"                     |* VK_NULL
  &* set                @"flags"                     |* VK_ZERO_FLAGS
  &* setListCountAndRef @"setLayoutCount"         -- |*
                        @"pSetLayouts"               |* []
  &* setListCountAndRef @"pushConstantRangeCount" -- |*
                        @"pPushConstantRanges"       |* [pushConstants]
  where pushConstants = createVk @VkPushConstantRange
          $ set @"stageFlags" |* VK_SHADER_STAGE_VERTEX_BIT
         &* set @"offset"     |* 0
         &* set @"size"       |* 16
mkPipelineLayoutCreateInfo descriptorSetLayout = createVk @VkPipelineLayoutCreateInfo
   $ set                @"sType"                     |* VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  &* set                @"pNext"                     |* VK_NULL
  &* set                @"flags"                     |* VK_ZERO_FLAGS
  &* setListCountAndRef @"setLayoutCount"         -- |*
                        @"pSetLayouts"               |* [descriptorSetLayout]
  &* setListCountAndRef @"pushConstantRangeCount" -- |*
                        @"pPushConstantRanges"       |* [pushConstants]
  where pushConstants = createVk @VkPushConstantRange
          $ set @"stageFlags" |* VK_SHADER_STAGE_VERTEX_BIT
         &* set @"offset"     |* 0
         &* set @"size"       |* 16

-- | Create info for a shader module with given bytecode.

mkShaderModuleCreateInfo
  :: BS.ByteString
  -> VkShaderModuleCreateInfo
mkShaderModuleCreateInfo code = createVk @VkShaderModuleCreateInfo
   $ set        @"sType"    |* VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  &* set        @"pNext"    |* VK_NULL
  &* set        @"flags"    |* VK_ZERO_FLAGS
  &* set        @"codeSize" |* fromIntegral |- BS.length code
  &* setListRef @"pCode"    |* runGet
                                 |- decodeWord32
                                 |- BL.fromStrict code
  where decodeWord32 :: Get [Word32]
        decodeWord32 = do
          empty <- isEmpty
          case (empty) of
            True  -> return []
            False -> do word32    <- getWord32le
                        remaining <- decodeWord32
                        return (word32 : remaining)

-- | Multisample state create info, with no sampling.

multisampleState :: VkPipelineMultisampleStateCreateInfo
multisampleState = createVk @VkPipelineMultisampleStateCreateInfo
  $ set @"sType"                 |* VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
 &* set @"pNext"                 |* VK_NULL
 &* set @"flags"                 |* VK_ZERO_FLAGS
 &* set @"sampleShadingEnable"   |* VK_FALSE
 &* set @"rasterizationSamples"  |* VK_SAMPLE_COUNT_1_BIT
 &* set @"minSampleShading"      |* 0
 &* set @"pSampleMask"           |* VK_NULL
 &* set @"alphaToCoverageEnable" |* VK_FALSE
 &* set @"alphaToOneEnable"      |* VK_FALSE

-- | Rasterization state create info.

rasterizationState :: VkPipelineRasterizationStateCreateInfo
rasterizationState = createVk @VkPipelineRasterizationStateCreateInfo
   $ set    @"sType"                    |* VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  &* set    @"pNext"                    |* VK_NULL
  &* set    @"flags"                    |* VK_ZERO_FLAGS
  &* set    @"depthClampEnable"         |* VK_FALSE
  &* set    @"rasterizerDiscardEnable"  |* VK_FALSE
  &* set    @"polygonMode"              |* VK_POLYGON_MODE_FILL
  &* set    @"cullMode"                 |* VK_CULL_MODE_NONE
  &* set    @"frontFace"                |* VK_FRONT_FACE_CLOCKWISE
  &* set    @"depthBiasEnable"          |* VK_FALSE
  &* set    @"depthBiasConstantFactor"  |* 0
  &* set    @"depthBiasClamp"           |* 0
  &* set    @"depthBiasSlopeFactor"     |* 0
  &* set    @"lineWidth"                |* 1

-- | Vertex shader stage create info, for a given vertex shader module.

vertStageInfo
  :: VkShaderModule
  -> VkPipelineShaderStageCreateInfo
vertStageInfo vertModule = createVk @VkPipelineShaderStageCreateInfo
   $ set       @"sType"  |* VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  &* set       @"pNext"  |* VK_NULL
  &* set       @"stage"  |* VK_SHADER_STAGE_VERTEX_BIT
  &* set       @"module" |* vertModule
  &* setStrRef @"pName"  |* "main"

vertexInputState0 :: VkPipelineVertexInputStateCreateInfo
vertexInputState0 = createVk @VkPipelineVertexInputStateCreateInfo
   $ set                @"sType"                              |* sType
  &* set                @"pNext"                              |* VK_NULL
  &* set                @"flags"                              |* VK_ZERO_FLAGS
  &* setListCountAndRef @"vertexBindingDescriptionCount"   -- |*
                        @"pVertexBindingDescriptions"         |* [bindingDesc0, bindingDesc1]
  &* setListCountAndRef @"vertexAttributeDescriptionCount" -- |*
                        @"pVertexAttributeDescriptions"       |* [vertX, vertY, vertDesc, posX, posY, xx, xy, yx, yy, fontSize, red, green, blue, alpha]
  where sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        bindingDesc0 = createVk @VkVertexInputBindingDescription
          $ set                @"binding"              |* 0
         &* set                @"stride"               |* 12
         &* set                @"inputRate"            |* VK_VERTEX_INPUT_RATE_VERTEX
        vertX = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 0
         &* set                @"format"               |* VK_FORMAT_R32_SINT
         &* set                @"offset"               |* 0
        vertY = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 1
         &* set                @"format"               |* VK_FORMAT_R32_SINT
         &* set                @"offset"               |* 4
        vertDesc = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 2
         &* set                @"format"               |* VK_FORMAT_R32_UINT
         &* set                @"offset"               |* 8
        bindingDesc1 = createVk @VkVertexInputBindingDescription
          $ set                @"binding"              |* 1
         &* set                @"stride"               |* (fromIntegral $ sizeOf @DrawData undefined)
         &* set                @"inputRate"            |* VK_VERTEX_INPUT_RATE_INSTANCE
        posX = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 3
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 0
        posY = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 4
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 4
        xx = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 5
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 8
        xy = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 6
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 12
        yx = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 7
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 16
        yy = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 8
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 20
        fontSize = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 9
         &* set                @"format"               |* VK_FORMAT_R32_SFLOAT
         &* set                @"offset"               |* 24
        red = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 10
         &* set                @"format"               |* VK_FORMAT_R8_UNORM
         &* set                @"offset"               |* 28
        green = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 11
         &* set                @"format"               |* VK_FORMAT_R8_UNORM
         &* set                @"offset"               |* 29
        blue = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 12
         &* set                @"format"               |* VK_FORMAT_R8_UNORM
         &* set                @"offset"               |* 30
        alpha = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 1
         &* set                @"location"             |* 13
         &* set                @"format"               |* VK_FORMAT_R8_UNORM
         &* set                @"offset"               |* 31

-- | Specifies how vertex data is read and passed to the shader.
-- | Currently, there is a single binding in position 0 with
-- | a 4-byte x-coordinate at location 0,
-- | a 4-byte y-coordinate at location 1,
-- | and a 4-byte vertex description at location 2.
-- |
-- | This is currently hardcoded, but ideally
-- | this would be set-up automatically from
-- | pipeline data.

vertexInputState1 :: VkPipelineVertexInputStateCreateInfo
vertexInputState1 = createVk @VkPipelineVertexInputStateCreateInfo
   $ set                @"sType"                              |* sType
  &* set                @"pNext"                              |* VK_NULL
  &* set                @"flags"                              |* VK_ZERO_FLAGS
  &* setListCountAndRef @"vertexBindingDescriptionCount"   -- |*
                        @"pVertexBindingDescriptions"         |* [bindingDesc]
  &* setListCountAndRef @"vertexAttributeDescriptionCount" -- |*
                        @"pVertexAttributeDescriptions"       |* [vertX, vertY, vertDesc]
  where sType = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
        bindingDesc = createVk @VkVertexInputBindingDescription
          $ set                @"binding"              |* 0
         &* set                @"stride"               |* 12
         &* set                @"inputRate"            |* VK_VERTEX_INPUT_RATE_VERTEX
        vertX = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 0
         &* set                @"format"               |* VK_FORMAT_R32_SINT
         &* set                @"offset"               |* 0
        vertY = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 1
         &* set                @"format"               |* VK_FORMAT_R32_SINT
         &* set                @"offset"               |* 4
        vertDesc = createVk @VkVertexInputAttributeDescription
          $ set                @"binding"              |* 0
         &* set                @"location"             |* 2
         &* set                @"format"               |* VK_FORMAT_R32_UINT
         &* set                @"offset"               |* 8

-- | Creates shader modules, a layout and a graphics pipeline create info.
-- | The result is created as a continuation so that the created
-- | shader modules are automatically destroyed
-- | after the layout and create info are consumed.
-- |
-- | I'm not sure that it's necessarily more efficient than
-- | simply creating each pipeline individually, but
-- | since multiple pipelines can be created in a single call
-- | I'm using this approach to (hopefully?) allow for some optimization;
-- | it's unlikely that this is slower, at the very least.

withPipelineCreateInfo
  :: VkDevice
  -> Descriptors
  -> (VkShaderModule, VkShaderModule)
  -> VkRenderPass
  -> Word32                                                     -- subpass
  -> VkPipelineVertexInputStateCreateInfo
  -> VkPipelineColorBlendStateCreateInfo
  -> (VkPipelineLayout -> VkGraphicsPipelineCreateInfo -> IO b)
  -> IO b
withPipelineCreateInfo device descriptors (vertModule, fragModule) renderpass subpass
                         vertexInputState colorBlendState continuation = do
  let pipelineLayoutCreateInfo   = mkPipelineLayoutCreateInfo
                                     |- descriptors.layout
  layout <- perform $ vkCreatePipelineLayout
                        |- device
                        |- p pipelineLayoutCreateInfo
                        |- VK_NULL
  let graphicsPipelineCreateInfo = mkGraphicsPipelineCreateInfo
                                     |- vertModule
                                     |- fragModule
                                     |- subpass
                                     |- layout
                                     |- renderpass
                                     |- vertexInputState
                                     |- colorBlendState
  continuation layout graphicsPipelineCreateInfo

