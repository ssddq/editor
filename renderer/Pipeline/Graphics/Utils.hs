{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.Graphics.Utils where

import Vk

import Data.Vector qualified as V

-- | Creates graphics pipelines from the givens layouts and create infos.
-- | Note that the implementation of this function is unsafe since
-- | it indexes into a vector in memory produced by vkCreateGraphicsPipelines.
createGraphicsPipelines
  :: VkDevice
  -> VkPipelineLayout
  -> VkGraphicsPipelineCreateInfo
  -> VkPipelineLayout
  -> VkGraphicsPipelineCreateInfo
  -> VkPipelineLayout
  -> VkGraphicsPipelineCreateInfo
  -> IO (Pipeline, Pipeline, Pipeline)
createGraphicsPipelines device layout0 info0 layout1 info1 layout2 info2 = do
  let pipelineCreateInfos = [ info0, info1, info2 ]
      pipelineCount = 3
  pipelines <- withArray pipelineCreateInfos $ \ptr -> performArray pipelineCount
                                             $ vkCreateGraphicsPipelines
                                                 |- device
                                                 |- VK_NULL_HANDLE
                                                 |- (fromIntegral pipelineCount)
                                                 |- ptr
                                                 |- VK_NULL
  return $ ( Pipeline { handle = pipelines V.! 0
                      , layout = layout0
                      }
           , Pipeline { handle = pipelines V.! 1
                      , layout = layout1
                      }
           , Pipeline { handle = pipelines V.! 2
                      , layout = layout2
                      }
           )
