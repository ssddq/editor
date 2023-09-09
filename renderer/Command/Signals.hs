{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command.Signals where

import Vk

createSignals
  :: Vk    { signals = X, vulkan = I }
  -> IO Vk { signals = I, vulkan = I }
createSignals vk = do
  inFlight  <- perform $ vkCreateFence
                           |- device
                           |- p fenceCreateInfo
                           |- VK_NULL
  available <- perform $ vkCreateSemaphore
                           |- device
                           |- p semaphoreCreateInfo
                           |- VK_NULL
  done      <- perform $ vkCreateSemaphore
                           |- device
                           |- p semaphoreCreateInfo
                           |- VK_NULL
  let semaphores = Semaphores { available, done }
      fences     = Fences     { inFlight        }
      signals    = Signals { semaphores, fences }
  return $ vk { signals }
  where device = vk.vulkan.device
        semaphoreCreateInfo = createVk @VkSemaphoreCreateInfo
          $ set @"sType" VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
         &* set @"pNext" VK_NULL
         &* set @"flags" VK_ZERO_FLAGS
        fenceCreateInfo = createVk @VkFenceCreateInfo
          $ set @"sType" |* VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
         &* set @"pNext" |* VK_NULL
         &* set @"flags" |* VK_FENCE_CREATE_SIGNALED_BIT


waitOn
  :: VkDevice
  -> VkFence
  -> IO ()
waitOn device fence = with fence $ \pFences -> do
  vkWaitForFences
    |- device
    |- 1
    |- pFences
    |- VK_TRUE
    |- maxBound
  vkResetFences
    |- device
    |- 1
    |- pFences
  return ()

