{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.Graphics where

import Vk

import Pipeline.Graphics.Info
import Pipeline.Graphics.Utils



-- | Create pipeline layouts using the given descriptors.
-- | Uses withPipelineCreateInfo to produce continuations that
-- | automatically create and destroy shader modules.
-- |
-- | This function only allows a single descriptor set for each pipeline.

createPipelines
  :: VkDevice
  -> (Descriptors, Descriptors, Descriptors)
  -> (Shaders, Shaders, Shaders)
  -> (VkRenderPass, VkRenderPass)
  -> IO (Pipeline, Pipeline, Pipeline)
createPipelines device descriptors shaders renderPasses = do
  let (descriptors0, descriptors1, descriptors2) = descriptors
      (shaders0   , shaders1   , shaders2   ) = shaders
      (renderPass0, renderPass1)              = renderPasses
      withPipelineCreateInfo00 = withPipelineCreateInfo0
                                   |- device
                                   |- descriptors0
                                   |- shaders0
                                   |- renderPass0
                                   |- 0
                                   |- multisampleState0
                                   |- vertexInputState0
      withPipelineCreateInfo01 = withPipelineCreateInfo
                                   |- device
                                   |- descriptors1
                                   |- shaders1
                                   |- renderPass0
                                   |- 1
                                   |- multisampleState0
                                   |- vertexInputState1
      withPipelineCreateInfo10 = withPipelineCreateInfo
                                   |- device
                                   |- descriptors2
                                   |- shaders2
                                   |- renderPass1
                                   |- 0
                                   |- multisampleState0
                                   |- vertexInputState1
  pipelines <- withPipelineCreateInfo00 $ \layout00 info00 ->
               withPipelineCreateInfo01 $ \layout01 info01 ->
               withPipelineCreateInfo10 $ \layout10 info10 ->
                                         createGraphicsPipelines device layout00 info00
                                                                        layout01 info01
                                                                        layout10 info10
  return $ pipelines
