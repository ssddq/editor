{-# OPTIONS_GHC -fprof-auto -F -pgmF=tpr-pp#-}

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BinaryLiterals        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Font qualified as Font
import Renderer
import Shaders
import Filebuffer

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import SDL.Event          qualified as SDL
import SDL.Input.Keyboard qualified as SDL
import SDL.Raw            qualified as SDLRaw

import Control.Concurrent

import Data.FileEmbed


import System.Environment qualified as Env
import System.IO

-- * main

initializeVulkan
  :: Font.Font
  -> FilePath
  -> IO Vk { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
initializeVulkan font file = do
  initializeVk emptyVk
  >>= loadFile file
  >>= createRenderPipeline renderPassShaders
  >>= createIndexedBuffer (Font.bufferWriter font)
  >>= createFullscreenBuffer
  >>= createCommandPool
  >>= createDrawBuffers
  >>= createSignals
  >>= drawFrame
  where renderPassShaders = ( Shaders vertShader0 fragShader0
                            , Shaders vertShader1 fragShader1
                            , Shaders vertShader2 fragShader2
                            )

main :: IO ()
main = withArgs $ \file -> do
  setNumCapabilities 8
  font <- Font.parse_Font <$> Font.loadTables fontFile
  vk <- initializeVulkan font file
  SDLRaw.startTextInput
  mainLoop vk
  return ()
  where fontFile = $(makeRelativeToProject "fonts/FiraCode/FiraCode.ttf" >>= embedFile)

mainLoop
  :: Vk    { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
  -> IO Vk { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
mainLoop vk = do
  event <- SDL.waitEvent
  case event of
    SDL.Event _ (Key SDL.KeycodeQ Ctrl) -> return vk
    e -> mainLoop =<< handle e vk



-- * SDL event handling

{-# INLINE Key #-}
pattern Key :: SDL.Keycode -> SDL.KeyModifier -> SDL.EventPayload
pattern Key keycode modifier <- SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ keycode modifier))

{-# INLINE Shift #-}
pattern Shift :: SDL.KeyModifier
pattern Shift <- (shiftDown -> True)

{-# INLINE Ctrl #-}
pattern Ctrl :: SDL.KeyModifier
pattern Ctrl <- (ctrlDown -> True)

handle
  :: SDL.Event
  -> Vk    { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
  -> IO Vk { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
handle (SDL.Event _time event) = case (event) of
  -- Window resize events
  SDL.WindowSizeChangedEvent _                       -> recreateSwapchain >=> recreateFramebuffers >=> drawFrame
  SDL.WindowRestoredEvent    _                       -> recreateSwapchain >=> recreateFramebuffers >=> drawFrame
  -- Print filebuffer state for debugging
  Key SDL.KeycodeReturn Shift                        -> printFilebuffer
  -- Navigation
  Key SDL.KeycodeRight _                             -> drawFrame . move Forward
  Key SDL.KeycodeLeft  _                             -> drawFrame . move Back
  Key SDL.KeycodeDown  _                             -> drawFrame . move Down
  Key SDL.KeycodeUp    _                             -> drawFrame . move Up
  -- Delete text
  Key SDL.KeycodeDelete    _                         -> drawFrame . updateFilebuffer (delete 1)
  Key SDL.KeycodeBackspace _                         -> drawFrame . updateFilebuffer (delete 1) . move Back
  -- Insert text
  Key SDL.KeycodeReturn _                            -> drawFrame . insertText "\n"
  SDL.TextInputEvent (SDL.TextInputEventData _ text) -> drawFrame . insertText text
  -- Otherwise
  _                                                  -> return

{-# INLINE shiftDown #-}
shiftDown
  :: SDL.KeyModifier
  -> Bool
shiftDown modifier = modifier.keyModifierLeftShift || modifier.keyModifierRightShift

{-# INLINE ctrlDown #-}
ctrlDown
  :: SDL.KeyModifier
  -> Bool
ctrlDown modifier = modifier.keyModifierLeftCtrl || modifier.keyModifierRightCtrl


-- * Filebuffer manipulation

insertText
  :: T.Text
  -> Vk { stream = A Filebuffer }
  -> Vk { stream = A Filebuffer }
insertText text vk = vk { stream }
  where stream = vk.stream { textBuffer = filebuffer' }
        filebuffer' = insert (T.encodeUtf8 text) vk.stream.textBuffer

loadFile
  :: FilePath
  -> Vk    { stream = X }
  -> IO Vk { stream = A Filebuffer }
loadFile path vk = do
  handle <- openFile path ReadMode
  size   <- fromIntegral <$> hFileSize handle
  lines  <- scanLines path
  let filebuffer = Filebuffer
                     { cursor = Position 0 0
                     , start  = Position 0 0
                     , size
                     , handle
                     , edits  = noEdits
                     , lines
                     , visualLineCount = maxBound
                     }
      stream = StreamBuffer filebuffer streamFilebuffer updateVisualLineCount
  return $ vk { stream }
  where updateVisualLineCount :: Int -> Filebuffer -> Filebuffer
        updateVisualLineCount n filebuffer = filebuffer { visualLineCount = n }


move
  :: Direction
  -> Vk { stream = A Filebuffer }
  -> Vk { stream = A Filebuffer }
move direction = updateFilebuffer $ movePosition direction

printFilebuffer
  :: Vk    { stream = A Filebuffer }
  -> IO Vk { stream = A Filebuffer }
printFilebuffer vk = do
  print vk.stream.textBuffer
  return vk

updateFilebuffer
  :: (Filebuffer -> Filebuffer)
  -> Vk { stream = A Filebuffer }
  -> Vk { stream = A Filebuffer }
updateFilebuffer f vk = vk { stream }
  where stream = vk.stream { textBuffer = repositionStart . f $ vk.stream.textBuffer }

withArgs
  :: (String -> IO ())
  -> IO ()
withArgs f = do
  args <- Env.getArgs
  case args of
    []         -> print ("File required" :: String)
    (file : _) -> f file
