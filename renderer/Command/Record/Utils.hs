module Command.Record.Utils where

import Vk

import Data.Vector qualified as V

bindDescriptorSets
  :: VkCommandBuffer
  -> VkPipelineLayout
  -> V.Vector VkDescriptorSet
  -> IO ()
bindDescriptorSets commandBuffer layout sets
  | V.null sets = return ()
  | otherwise   = do withVector sets
                       $ \n pDescriptorSets -> vkCmdBindDescriptorSets
                                                 |- commandBuffer
                                                 |- VK_PIPELINE_BIND_POINT_GRAPHICS
                                                 |- layout
                                                 |- 0
                                                 |- fromIntegral n
                                                 |- pDescriptorSets
                                                 |- 0
                                                 |- VK_NULL
                     return ()
