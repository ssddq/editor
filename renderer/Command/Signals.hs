{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}

module Command.Signals where

import Vk

import Data.Vector qualified as V

createSignals
  :: Vk    { signals = X, vulkan = I }
  -> IO Vk { signals = I, vulkan = I }
createSignals vk = do
  fences     <- V.replicateM (fromIntegral vk.constants.imageCount) $ do
                  inFlight <- perform $ vkCreateFence
                                          |- device
                                          |- p fenceCreateInfo
                                          |- VK_NULL
                  return $ Fences { inFlight }
  semaphores <- V.replicateM (fromIntegral vk.constants.imageCount) $ do
                  available <- perform $ vkCreateSemaphore
                                           |- device
                                           |- p semaphoreCreateInfo
                                           |- VK_NULL
                  done      <- perform $ vkCreateSemaphore
                                           |- device
                                           |- p semaphoreCreateInfo
                                           |- VK_NULL
                  return $ Semaphores { available, done }
  let drawIndex = 0
      signals   = Signals { drawIndex, fences, semaphores }
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

incrementDrawIndex
  :: Vk { signals = I }
  -> Vk { signals = I }
incrementDrawIndex vk = vk { signals }
  where signals   = vk.signals { drawIndex }
        drawIndex = (vk.signals.drawIndex + 1) `mod` fromIntegral vk.constants.imageCount

nextFences
  :: Vk { signals = I }
  -> Fences
nextFences vk = signals.fences V.! (signals.drawIndex `mod` fromIntegral vk.constants.imageCount)
  where signals = vk.signals

nextSemaphores
  :: Vk { signals = I }
  -> Semaphores
nextSemaphores vk = signals.semaphores V.! (signals.drawIndex `mod` fromIntegral vk.constants.imageCount)
  where signals = vk.signals

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
