{-# OPTIONS_GHC -F -pgmF=tpr-pp #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Command.Write where

import Vk

import Streaming          qualified as S
import Streaming.Internal qualified as S

data DrawState
  = RegularDraw
  | EndOfLine
  | Newline
  | Done

drawState
  :: Char
  -> Float
  -> WriteState
  -> DrawState
drawState char width state
  | state.positionY >= state.yMax        = Done
  | char == '\n'                         = Newline
  | state.positionX + width > state.xMax = EndOfLine
  | otherwise                            = RegularDraw

initialState :: WriteState
initialState = WriteState
  { instanceNum = 0
  , positionX = 0
  , positionY = 0
  , xMin = 0
  , xMax = 0
  , yMax = 0
  , lines = 0
  , color = Color 255 255 255 255
  , currentLine = False
  , currentLineStart = 0
  , currentLineStop = 0
  }

writeCharacterDraw
  :: Char
  -> Constants
  -> Font
  -> Ptr VkDrawIndexedIndirectCommand
  -> Ptr DrawData
  -> WriteState
  -> IO (DrawState, Ptr VkDrawIndexedIndirectCommand, Ptr DrawData, WriteState)
writeCharacterDraw char constants font pDrawCmds pDrawData state = case (drawState char width state) of
  RegularDraw -> do let xOffset  = state.positionX
                        yOffset  = state.positionY
                        drawData = mkDrawData xOffset yOffset
                    poke pDrawCmds drawCmd
                    poke pDrawData drawData
                    let positionX   = state.positionX + width
                        instanceNum = state.instanceNum + 1
                        state' = state { positionX, instanceNum }
                    return $ ( RegularDraw
                             , advancePtr pDrawCmds 1
                             , advancePtr pDrawData 1
                             , state'
                             )
  EndOfLine   -> do let xOffset  = state.xMin
                        yOffset  = state.positionY + lineHeight
                        drawData = mkDrawData xOffset yOffset
                    poke pDrawCmds drawCmd
                    poke pDrawData drawData
                    let positionX   = state.xMin + width
                        positionY   = yOffset
                        instanceNum = state.instanceNum + 1
                        state' = state { positionX, positionY, instanceNum }
                    return $ ( EndOfLine
                             , advancePtr pDrawCmds 1
                             , advancePtr pDrawData 1
                             , state'
                             )
  Newline -> do let positionX = state.xMin
                    positionY = state.positionY + lineHeight
                    lines     = state.lines + 1
                    currentLine = False
                    currentLineStop = case (state.currentLine) of
                      True  -> state.positionY + textHeight / 2
                      False -> state.currentLineStop
                    state' = state { positionX, positionY, lines, currentLine, currentLineStop }
                return $ (Newline, pDrawCmds, pDrawData, state')
  Done    -> return $ (Done, pDrawCmds, pDrawData, state)
  where Font      {..} = font
        Constants {..} = constants
        (,,,) firstIndex indexCount advance _ = lookup $ fromIntegral $ fromEnum char
        width = scale * fromIntegral advance
        mkDrawData x y = DrawData x y scale 0 0 scale fSize state.color
        drawCmd = createVk @VkDrawIndexedIndirectCommand
          $ set @"indexCount"    |* fromIntegral indexCount
         &* set @"instanceCount" |* 1
         &* set @"firstIndex"    |* fromIntegral firstIndex
         &* set @"vertexOffset"  |* 0
         &* set @"firstInstance" |* state.instanceNum


-- | The cursor can have clipping issues with the surrounding text since it's drawn only once,
-- | and the basepoint from which the winding number is calculated
-- | is not the same basepoint that the adjacent glyphs use.
-- |
-- | To fix this, we could opt to simply draw the cursor twice:
-- | this is not really correct but is extremely easy,
-- | since we can just write the draw command into the buffer twice here.
-- |
-- | I am not doing this at this point, since it's not a significant issue and
-- | we'll likely refactor later to draw UI elements into a separate color attachment
-- | that is overlaid at the end.

writeCursorDraw
  :: Color
  -> Font
  -> Ptr VkDrawIndexedIndirectCommand
  -> Ptr DrawData
  -> WriteState
  -> IO (Ptr VkDrawIndexedIndirectCommand, Ptr DrawData, WriteState)
writeCursorDraw color font pDrawCmds pDrawData state = do
  let drawData = DrawData { xOffset = state.positionX
                          , yOffset = state.positionY - (textHeight / 2)
                          , xx = 1.5
                          , xy = 0
                          , yx = 0
                          , yy = lineHeight / 2
                          , fSize = 1.0
                          , color
                          }
  poke pDrawCmds drawCmd
  poke pDrawData drawData
  let instanceNum = state.instanceNum + 1
      currentLine = True
      currentLineStart = state.positionY - lineHeight / 2 - textHeight / 2
      state' = state { instanceNum, currentLine, currentLineStart }
  return $ ( advancePtr pDrawCmds 1
           , advancePtr pDrawData 1
           , state'
           )
  where Font {..} = font
        drawCmd = createVk @VkDrawIndexedIndirectCommand
          $ set @"indexCount"    |* 12
         &* set @"instanceCount" |* 1
         &* set @"firstIndex"    |* 0
         &* set @"vertexOffset"  |* 0
         &* set @"firstInstance" |* state.instanceNum

writeFullscreenDraw
  :: Constants
  -> Color
  -> Ptr VkDrawIndexedIndirectCommand
  -> Ptr DrawData
  -> WriteState
  -> IO (Ptr VkDrawIndexedIndirectCommand, Ptr DrawData, WriteState)
writeFullscreenDraw constants color pDrawCmds pDrawData state = do
  let drawData = DrawData { xOffset = fromIntegral constants.present.width / 2
                          , yOffset = fromIntegral constants.present.height / 2
                          , xx = fromIntegral constants.present.width
                          , xy = 0
                          , yx = 0
                          , yy = fromIntegral constants.present.height
                          , fSize = 1.0
                          , color
                          }
  poke pDrawCmds drawCmd
  poke pDrawData drawData
  let instanceNum = state.instanceNum + 1
      state' = state { instanceNum }
  return $ ( advancePtr pDrawCmds 1
           , advancePtr pDrawData 1
           , state'
           )
  where drawCmd = createVk @VkDrawIndexedIndirectCommand
          $ set @"indexCount"    |* 12
         &* set @"instanceCount" |* 1
         &* set @"firstIndex"    |* 0
         &* set @"vertexOffset"  |* 0
         &* set @"firstInstance" |* state.instanceNum

-- | Note this function is unsafe at the moment, since there is no check to prevent
-- | writing past the memory allocated by the pointer. The initial allocation is large
-- | enough that this shouldn't matter, but this should be made safe eventually.
-- |
-- | Additionally, this function makes no effort to instance the draw calls,
-- | instead creating a new draw call for each character. And instancing would be a fairly easy refactor:
-- | we could simply produce an IntMap keyed on character code points when consuming the stream,
-- | updating the instance count on insertion, with vectors holding the per-instance data.
-- | The resulting IntMap can then be consumed to write instanced draw data.

writeIndirectDrawStream
  :: Mode
  -> Constants
  -> Font
  -> Ptr VkDrawIndexedIndirectCommand
  -> Ptr DrawData
  -> WriteState
  -> S.Stream (S.Of Symbol) IO ()
  -> IO WriteState
writeIndirectDrawStream mode constants font = loop
  where loop :: Ptr VkDrawIndexedIndirectCommand -> Ptr DrawData -> WriteState -> S.Stream (S.Of Symbol) IO () -> IO WriteState
        -- We're dividing by lineHeight, but this should be safe.
        -- If a font declares lineHeight = 0 then many (many!) things would've failed already
        loop _ _ state (S.Return _) = return $ state'
          where lines' = state.lines + max 0 (floor $ (state.yMax - state.positionY) / font.lineHeight)
                state' = state { lines = lines' }
        loop pDrawCmds pDrawData state (S.Effect m) = loop pDrawCmds pDrawData state =<< m
        loop pDrawCmds pDrawData state (S.Step (Cursor cursor S.:> stream)) = do
          result <- writeCursorDraw (cursor mode) font pDrawCmds pDrawData state
          let (,,) pDrawCmds' pDrawData' state' = result
          loop pDrawCmds' pDrawData' state' stream
        loop pDrawCmds pDrawData state (S.Step (ColorChange color S.:> stream)) = loop pDrawCmds pDrawData (state { color = color }) stream
        loop pDrawCmds pDrawData state (S.Step (Char c S.:> stream)) = do
          let char = toEnum $ fromIntegral c
          result <- writeCharacterDraw char constants font pDrawCmds pDrawData state
          case (result) of
            (,,,) Done _ _ state'                -> return $ state'
            (,,,) _ pDrawCmds' pDrawData' state' -> loop pDrawCmds' pDrawData' state' stream

writeIndirectDraws
  :: Buffer
  -> Buffer
  -> Vk { font = I, stream = A b }
  -> IO (WriteState, b)
writeIndirectDraws bufferI bufferD vk = do
  let ptrI = castPtr bufferI.ptr
      ptrD = castPtr bufferD.ptr
      startState = initialState { positionX = 10
                                , positionY = ( (fromIntegral $ ascender + lineGap ) * scale ) + 10
                                , xMax = fromIntegral $ present.width - 10
                                , yMax = fromIntegral $ present.height - 10
                                , xMin = 10
                                }
  (ptrI', ptrD', state) <- writeFullscreenDraw
              |- constants
  --          |- Color 0 0 0 175
              |- Color 0 0 0 0
              |- ptrI
              |- ptrD
              |- startState
  state' <- writeIndirectDrawStream
             |- mode
             |- constants
             |- font
             |- ptrI'
             |- ptrD'
             |- state
             |- toStream stream
  let stream' = stream.updateVisualLineCount state'.lines stream.textBuffer
  return $ (state', stream')
  where Vk        {..} = vk
        Constants {..} = constants
        Font      {..} = font
