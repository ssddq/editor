{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell     #-}

module Initialize.Window where

import SDL qualified as SDL
import Vk

import Data.Coerce

import SDL.Video.Vulkan qualified as SDL


-- | Create a window with SDL.
createWindow
  :: Constants
  -> IO SDL.Window
createWindow constants = do
  SDL.initializeAll
  window <- SDL.createWindow
                  |- name
                  |- SDL.defaultWindow { SDL.windowGraphicsContext = SDL.VulkanContext
                                       , SDL.windowInitialSize     = SDL.V2 windowWidth windowHeight
                                       , SDL.windowResizable       = True
                                       }
  return $ window
  where name      = constants.name
        windowWidth  = fromIntegral $ constants.present.width
        windowHeight = fromIntegral $ constants.present.height

-- | Create a Vulkan surface for the created window and instance.
createSurface
  :: VkInstance
  -> SDL.Window
  -> IO VkSurfaceKHR
createSurface instance_ window = do
  sdlSurface <- SDL.vkCreateSurface
                      |- window
                      |- coerce instance_
  let surface = coerce sdlSurface
  return $ surface

