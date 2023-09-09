{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Initialize.Instance where

import SDL qualified as SDL
import Vk

import Initialize.Instance.Info

import SDL.Video.Vulkan qualified as SDL

-- | Creates Vulkan instance with given layers and extensions,
-- | as well as window extensions required by SDL.
createInstance
  :: Layers
  -> Extensions
  -> SDL.Window
  -> IO VkInstance
createInstance layers extensions window = do
  windowExtensions <- SDL.vkGetInstanceExtensions window
  let instanceCreateInfo = mkInstanceCreateInfo
                             |- layers
                             |- windowExtensions ++ extensions
  instance_ <- perform $ vkCreateInstance
                           |- p instanceCreateInfo
                           |- VK_NULL
  return $ instance_
