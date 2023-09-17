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

import Filebuffer
import Font       qualified as Font
import Renderer
import Shaders
import Haskell

import Data.Text          qualified as T
import Data.Text.Encoding qualified as T

import SDL.Event          qualified as SDL
import SDL.Input.Keyboard qualified as SDL
import SDL.Raw            qualified as SDLRaw

import Control.Concurrent

import Data.FileEmbed

import Streaming qualified as S
import Streaming.Prelude qualified as SP

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
                            , Shaders vertShader3 fragShader3
                            )

main :: IO ()
main = withArgs $ \file -> do
  setNumCapabilities 8
  font <- Font.parse_Font <$> Font.loadTables fontFile
  vk <- initializeVulkan font file
  SDLRaw.stopTextInput
  mainLoop vk
  return ()
  where fontFile = $(makeRelativeToProject "fonts/FiraCode/FiraCode-Regular.ttf" >>= embedFile)

mainLoop
  :: Vk    { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
  -> IO Vk { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
mainLoop vk@Vk{mode} = do
  drawFrame vk
  event <- SDL.waitEvent
  case event of
    SDL.Event _ (Key SDL.KeycodeQ Ctrl) -> return vk
    e -> mainLoop =<< handle mode e vk



-- * SDL event handling

{-# INLINE Ctrl #-}
pattern Ctrl :: SDL.KeyModifier
pattern Ctrl <- (ctrlDown -> True)

{-# INLINE Key #-}
pattern Key :: SDL.Keycode -> SDL.KeyModifier -> SDL.EventPayload
pattern Key keycode modifier <- SDL.KeyboardEvent (SDL.KeyboardEventData _ SDL.Pressed _ (SDL.Keysym _ keycode modifier))

{-# INLINE Shift #-}
pattern Shift :: SDL.KeyModifier
pattern Shift <- (shiftDown -> True)

{-# INLINE ctrlDown #-}
ctrlDown
  :: SDL.KeyModifier
  -> Bool
ctrlDown modifier = modifier.keyModifierLeftCtrl || modifier.keyModifierRightCtrl

{-# INLINE waitForChars #-}
waitForChars
  :: Int
  -> IO (Maybe T.Text)
waitForChars n = do
  SDLRaw.startTextInput
  result <- loop "" n
  SDLRaw.stopTextInput
  return result
  where loop :: T.Text -> Int -> IO (Maybe T.Text)
        loop text 0 = return (Just text)
        loop text n = do
          SDL.Event _ event <- SDL.waitEvent
          case event of
            SDL.TextInputEvent (SDL.TextInputEventData _ text') -> loop (T.concat [text, text']) $ n - 1
            Key SDL.KeycodeEscape _ -> return Nothing
            _ -> loop text n

{-# INLINE seek #-}
seek
  :: Vk    { stream = A Filebuffer }
  -> IO Vk { stream = A Filebuffer }
seek vk = do
  text <- waitForChars 2
  case (text) of
    Nothing -> return vk
    Just chars -> do
      next <- S.inspect $ scan 4096 filebuffer $ T.encodeUtf8 chars
      case (next) of
        Left  _ -> return vk
        Right (position S.:> _) -> do
          let filebuffer' = repositionStart $ filebuffer { cursor = position }
              stream = vk.stream { textBuffer = filebuffer' }
          return $ vk { stream }
  where filebuffer = vk.stream.textBuffer

{-# INLINE seekBackwards #-}
seekBackwards
  :: Vk    { stream = A Filebuffer }
  -> IO Vk { stream = A Filebuffer }
seekBackwards vk = do
  text <- waitForChars 2
  case (text) of
    Nothing -> return vk
    Just chars -> do
      next <- SP.last_ $ scanBackwards 4096 filebuffer $ T.encodeUtf8 chars
      case (next) of
        Nothing -> return vk
        Just position -> do
          let filebuffer' = repositionStart $ filebuffer { cursor = position }
              stream = vk.stream { textBuffer = filebuffer' }
          return $ vk { stream }
  where filebuffer = vk.stream.textBuffer

handle
  :: Mode
  -> SDL.Event
  -> Vk    { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
  -> IO Vk { commandPool = I, drawBuffers = I, font = I, fullscreenBuffer = I, renderPipeline = I, signals = I, stream = A Filebuffer, vulkan = I }
handle Normal (SDL.Event _time event) = case (event) of
  -- Window resize events
  SDL.WindowSizeChangedEvent _ -> recreateSwapchain . updateFilebuffer id >=> recreateFramebuffers >=> drawFrame
  SDL.WindowRestoredEvent    _ -> recreateSwapchain . updateFilebuffer id >=> recreateFramebuffers >=> drawFrame
  -- Enter insert mode
  Key SDL.KeycodeI _           -> setMode Insert >=> drawFrame
  -- Seek
  Key SDL.KeycodeS Shift       -> seekBackwards >=> drawFrame
  Key SDL.KeycodeS _           -> seek >=> drawFrame
  -- Delete text
  Key SDL.KeycodeDelete    _ -> drawFrame . updateFilebuffer (delete 1)
  Key SDL.KeycodeBackspace _ -> drawFrame . updateFilebuffer (delete 1) . move Back
  -- Print filebuffer state for debugging
  Key SDL.KeycodeReturn Shift -> printFilebuffer
  -- Navigation
  Key SDL.KeycodeRight _ -> drawFrame . move Forward
  Key SDL.KeycodeLeft  _ -> drawFrame . move Back
  Key SDL.KeycodeDown  _ -> drawFrame . move Down
  Key SDL.KeycodeUp    _ -> drawFrame . move Up
  -- Otherwise
  _ -> return
handle Insert (SDL.Event _time event) = case (event) of
  -- Window resize events
  SDL.WindowSizeChangedEvent _ -> recreateSwapchain . updateFilebuffer id >=> recreateFramebuffers >=> drawFrame
  SDL.WindowRestoredEvent    _ -> recreateSwapchain . updateFilebuffer id >=> recreateFramebuffers >=> drawFrame
  -- Exit insert mode
  Key SDL.KeycodeEscape _ -> setMode Normal >=> drawFrame
  -- Print filebuffer state for debugging
  Key SDL.KeycodeReturn Shift -> printFilebuffer
  -- Navigation
  Key SDL.KeycodeRight _ -> drawFrame . move Forward
  Key SDL.KeycodeLeft  _ -> drawFrame . move Back
  Key SDL.KeycodeDown  _ -> drawFrame . move Down
  Key SDL.KeycodeUp    _ -> drawFrame . move Up
  -- Delete text
  Key SDL.KeycodeDelete    _ -> drawFrame . updateFilebuffer (delete 1)
  Key SDL.KeycodeBackspace _ -> drawFrame . updateFilebuffer (delete 1) . move Back
  -- Insert text
  Key SDL.KeycodeReturn _                            -> drawFrame . insertText "\n"
  SDL.TextInputEvent (SDL.TextInputEventData _ text) -> drawFrame . insertText text
  -- Otherwise
  _ -> return

{-# INLINE setMode #-}
setMode
  :: Mode
  -> Vk    { stream = A Filebuffer }
  -> IO Vk { stream = A Filebuffer }
setMode Insert vk = do
  SDLRaw.startTextInput
  return $ vk { mode = Insert }
setMode Normal vk = do
  SDLRaw.stopTextInput
  return $ vk { mode = Normal }

{-# INLINE shiftDown #-}
shiftDown
  :: SDL.KeyModifier
  -> Bool
shiftDown modifier = modifier.keyModifierLeftShift || modifier.keyModifierRightShift



-- * Filebuffer manipulation

insertText
  :: T.Text
  -> Vk { stream = A Filebuffer }
  -> Vk { stream = A Filebuffer }
insertText text = updateFilebuffer $ insert (T.encodeUtf8 text)

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
      stream = StreamBuffer filebuffer (streamFilebuffer hsParser) updateVisualLineCount $ Color 46 52 64 255
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
