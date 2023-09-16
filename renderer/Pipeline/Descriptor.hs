{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE TypeApplications         #-}


module Pipeline.Descriptor where

import Vk

import Pipeline.Descriptor.Info

import Data.Vector qualified as V


-- | Creates descriptor sets, layouts and a descriptor pool for the given resources.
-- |
-- | Managing descriptors is somewhat annoying and probably pointless.
-- | It would probably be better if we just refactored to use vkCmdPushDescriptorSetKHR
-- | from the VK_KHR_push_descriptors extension.

createDescriptors
  :: VkDevice
  -> [Resource]
  -> IO Descriptors
createDescriptors device resources = do
  let descriptorPoolCreateInfo = mkDescriptorPoolCreateInfo $ map descriptorPoolSize resources
      (descriptorSetLayoutCreateInfo, _) = mkDescriptorSetLayoutCreateInfo $ map descriptorSetLayoutBinding resources
  layout <- perform $ vkCreateDescriptorSetLayout
                        |- device
                        |- p descriptorSetLayoutCreateInfo
                        |- VK_NULL
  pool <- perform $ vkCreateDescriptorPool
                      |- device
                      |- p descriptorPoolCreateInfo
                      |- VK_NULL
  let descriptorSetAllocateInfo = mkDescriptorSetAllocateInfo
                                    |- pool
                                    |- layout
  set <- perform $ vkAllocateDescriptorSets
                     |- device
                     |- p descriptorSetAllocateInfo
  let writeDescriptorSets = V.fromList $ map
                                |- mkWriteDescriptorSet set
                                |- resources
  withVector writeDescriptorSets
    $ \n pDescriptorWrites -> vkUpdateDescriptorSets
                                |- device
                                |- (fromIntegral n)
                                |- pDescriptorWrites
                                |- 0
                                |- VK_NULL
  return $ Descriptors { set
                       , pool
                       , layout
                       }

empty :: IO Descriptors
empty = pure $ Descriptors { set    = VK_NULL_HANDLE
                           , pool   = VK_NULL_HANDLE
                           , layout = VK_NULL_HANDLE
                           }
