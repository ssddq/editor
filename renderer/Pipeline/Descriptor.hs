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
      (descriptorSetLayoutCreateInfo, bindingCount) = mkDescriptorSetLayoutCreateInfo $ map descriptorSetLayoutBinding resources
  layouts <- performArray bindingCount $ vkCreateDescriptorSetLayout
                                           |- device
                                           |- p descriptorSetLayoutCreateInfo
                                           |- VK_NULL
  pool <- perform $ vkCreateDescriptorPool
                      |- device
                      |- p descriptorPoolCreateInfo
                      |- VK_NULL
  let (descriptorSetAllocateInfo, descriptorSetCount) = mkDescriptorSetAllocateInfo
                                                          |- pool
                                                          |- layouts
  sets <- performArray descriptorSetCount $ vkAllocateDescriptorSets
                                              |- device
                                              |- p descriptorSetAllocateInfo
  let writeDescriptorSets = V.zipWith mkWriteDescriptorSet sets $ V.fromList resources
  withVector writeDescriptorSets
    $ \n pDescriptorWrites -> vkUpdateDescriptorSets
                                |- device
                                |- (fromIntegral n)
                                |- pDescriptorWrites
                                |- 0
                                |- VK_NULL
  return $ Descriptors { sets
                       , pool
                       , layouts
                       }

empty :: IO Descriptors
empty = pure $ Descriptors { sets    = V.empty
                           , pool    = VK_NULL_HANDLE
                           , layouts = V.empty
                           }
