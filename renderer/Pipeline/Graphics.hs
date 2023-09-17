{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.Graphics where

import Vk

import Pipeline.Info
import Pipeline.Graphics.Info

import Data.Vector qualified as V

-- | Create pipeline layouts using the given descriptors.
-- | Uses withPipelineCreateInfo to produce continuations that
-- | automatically create and destroy shader modules.
-- |
-- | This function only allows a single descriptor set for each pipeline.

createPipelines
  :: VkDevice
  -> (Descriptors, Descriptors, Descriptors)
  -> (Shaders, Shaders, Shaders, Shaders)
  -> (VkRenderPass, VkRenderPass)
  -> IO Pipelines
createPipelines device descriptors shaders renderPasses = do
  let (,) renderPass0 renderPass1 = renderPasses
      (,,) descriptors0 descriptors1 descriptors2 = descriptors
      (,,,) shaders0 shaders1 shaders2 shaders3 = shaders
  vertModule0 <- createShaderModule device shaders0.vertex
  fragModule0 <- createShaderModule device shaders0.fragment
  vertModule1 <- createShaderModule device shaders1.vertex
  fragModule1 <- createShaderModule device shaders1.fragment
  vertModule2 <- createShaderModule device shaders2.vertex
  fragModule2 <- createShaderModule device shaders2.fragment
  vertModule3 <- createShaderModule device shaders3.vertex
  fragModule3 <- createShaderModule device shaders3.fragment
  let withDrawPipelineCreateInfo n = withPipelineCreateInfo
                                       |- device
                                       |- descriptors0
                                       |- (vertModule0, fragModule0)
                                       |- renderPass0
                                       |- n
                                       |- vertexInputState0
                                       |- colorBlendAdd
      withResolvePipelineCreateInfo n = withPipelineCreateInfo
                                          |- device
                                          |- descriptors1
                                          |- (vertModule1, fragModule1)
                                          |- renderPass0
                                          |- n
                                          |- vertexInputState0
                                          |- colorBlendSrcAlpha
      withClearPipelineCreateInfo n = withPipelineCreateInfo
                                        |- device
                                        |- descriptors0
                                        |- (vertModule3, fragModule3)
                                        |- renderPass0
                                        |- n
                                        |- vertexInputState0
                                        |- colorBlendSrc
      withAAPipelineCreateInfo = withPipelineCreateInfo
                                   |- device
                                   |- descriptors2
                                   |- (vertModule2, fragModule2)
                                   |- renderPass1
                                   |- 0
                                   |- vertexInputState1
                                   |- colorBlendAdd
  pipelines <-
    withDrawPipelineCreateInfo    0  $ \layout0  info0  ->
    withResolvePipelineCreateInfo 1  $ \layout1  info1  ->
    withClearPipelineCreateInfo   2  $ \layout2  info2  ->
    withDrawPipelineCreateInfo    3  $ \layout3  info3  ->
    withResolvePipelineCreateInfo 4  $ \layout4  info4  ->
    withClearPipelineCreateInfo   5  $ \layout5  info5  ->
    withDrawPipelineCreateInfo    6  $ \layout6  info6  ->
    withResolvePipelineCreateInfo 7  $ \layout7  info7  ->
    withClearPipelineCreateInfo   8  $ \layout8  info8  ->
    withDrawPipelineCreateInfo    9  $ \layout9  info9  ->
    withResolvePipelineCreateInfo 10 $ \layout10 info10 ->
    withAAPipelineCreateInfo         $ \layout11 info11 -> do
      let pipelineCreateInfos = V.fromList [ info0, info1, info2
                                           , info3, info4, info5
                                           , info6, info7, info8
                                           , info9, info10
                                           , info11
                                           ]
      pipelines <- withVector pipelineCreateInfos
                   $ \n ptr -> performArray n
                   $ vkCreateGraphicsPipelines
                       |- device
                       |- VK_NULL_HANDLE
                       |- fromIntegral n
                       |- ptr
                       |- VK_NULL
      return $ Pipelines
        { draw0    = Pipeline { handle = pipelines V.! 0 , layout = layout0  }
        , resolve0 = Pipeline { handle = pipelines V.! 1 , layout = layout1  }
        , clear0   = Pipeline { handle = pipelines V.! 2 , layout = layout2  }
        , draw1    = Pipeline { handle = pipelines V.! 3 , layout = layout3  }
        , resolve1 = Pipeline { handle = pipelines V.! 4 , layout = layout4  }
        , clear1   = Pipeline { handle = pipelines V.! 5 , layout = layout5  }
        , draw2    = Pipeline { handle = pipelines V.! 6 , layout = layout6  }
        , resolve2 = Pipeline { handle = pipelines V.! 7 , layout = layout7  }
        , clear2   = Pipeline { handle = pipelines V.! 8 , layout = layout8  }
        , draw3    = Pipeline { handle = pipelines V.! 9 , layout = layout9  }
        , resolve3 = Pipeline { handle = pipelines V.! 10, layout = layout10 }
        , aa       = Pipeline { handle = pipelines V.! 11, layout = layout11 }
        }
  mapM_
    |- (\shaderModule -> vkDestroyShaderModule device shaderModule VK_NULL)
    |- [ vertModule0, fragModule0
       , vertModule1, fragModule1
       , vertModule2, fragModule2
       , vertModule3, fragModule3
       ]
  return $ pipelines
