{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE TypeApplications         #-}

module Font.TTF.Utils where

import Common

import Control.Monad
import Control.Monad.State.Lazy

import Data.Bits
import Data.Ord
import Data.Vector qualified as V
import Data.Word

import Font.TTF.Patterns
import Font.TTF.Types

import Foreign.Ptr
import Foreign.Storable

import GHC.Records qualified as GHC

-- | This is the state used in writing the font data to the vertex/index buffers.
-- | All fields are strict since there is no reason evaluation should not be forced at every stage.
data WriteState = WriteState
  { vertexLoc      :: {-# UNPACK #-} !Word32
  , indexLoc       :: {-# UNPACK #-} !Word32
  , glyphBaseIndex :: {-# UNPACK #-} !Word32
  , writeInst      :: {-# UNPACK #-} !Int
  , contourNum     :: {-# UNPACK #-} !Int
  , contourStart   :: {-# UNPACK #-} !Int
  , contourEnd     :: {-# UNPACK #-} !Int
  , currentGlyph   :: {-# UNPACK #-} !Int
  }
  deriving (Show)

{-# INLINE throwFailure #-}
throwFailure
  :: Either String a
  -> a
throwFailure r = case r of
  Left  e -> error e
  Right a -> a


bufferWriter
  :: forall buffer void. (GHC.HasField "ptr" buffer (Ptr void))
  => Font
  -> (Word64 -> IO buffer)
  -> (Word64 -> IO buffer)
  -> IO (buffer, buffer, FontData)
bufferWriter font mkVertexBuffer mkIndexBuffer = do
  vertexBuffer <- mkVertexBuffer vertexBufferSize
  indexBuffer  <- mkIndexBuffer  indexBufferSize
  indexRanges  <- writeBuffers
                    |- font.glyf
                    |- font.hmtx
                    |- vertexBuffer.ptr
                    |- indexBuffer.ptr
  let fontData = FontData
        { unitsPerEmX2 = 2 * fromIntegral font.head.unitsPerEm
        , ascender     = hhea.ascender
        , descender    = hhea.descender
        , lineGap      = hhea.lineGap
        , lookup       = glyphRangeLookup font indexRanges
        }
  return $ (vertexBuffer, indexBuffer, fontData)
  where hhea = font.hhea
        -- | Overestimate for the vertex buffer size, where the bound is obtained by noting that:
        -- |   1) Each simple glyf must have a `base` vertex.
        -- |   2) Each on-curve point may result in up to 2 vertices,
        -- |      since it may be part of 2 consecutive draw calls, and vertices are not shared.
        -- |   3) Each off-curve control point may result in up to 3 vertices:
        -- |      the control itself and averages to the left and right.
        vertexBufferSize = (fromIntegral $ size @Vertex) * (2 * font.glyf.numVertices + font.glyf.numSimple + font.glyf.numOffCurve )
        -- | Required index buffer size, which includes numDraws * 3 indices for the font and 4 * 3 indices for the rectangle used to render the cursor.
        indexBufferSize  = 3 * (fromIntegral $ size @Index) * (font.glyf.numDraws + 4)

-- | Iterate a monadic action by piping the output into the next iteration.
chainM
  :: (Monad m)
  => Int
  -> (a -> m a)
  -> (a -> m a)
chainM 1 f a = f a
chainM n f a = f a >>= chainM (n - 1) f

-- | Retrieves the glyph index and transformation data from a glyph component record.
getComponentData
  :: Record "Component"
  -> (Int, Transformation)
getComponentData component = ( fromIntegral $ component.glyphIndex
                             , Transformation { xOffset = component.argument1
                                              , yOffset = component.argument2
                                              , xx = a
                                              , xy = b
                                              , yx = c
                                              , yy = d
                                              }
                             )
  where (a,b,c,d) = getScaleData component.transformation

-- | Extracts a transformation record for a glyph in a composite glyph
-- | as a standardized matrix.
getScaleData
  :: Record "Transformation"
  -> (Float, Float, Float, Float)
getScaleData (SCALE a)            = (a, 0, 0, a)
getScaleData (X_AND_Y_SCALE x y)  = (x, 0, 0, y)
getScaleData (TWO_BY_TWO a b c d) = (a, b, c, d)

-- FIX
glyphIndexLookup
  :: Word32
  -> Font
  -> Int
glyphIndexLookup n font = glyphIndexLookup4 n enc
  where enc = case (V.find (\a -> format a == 4) (font.cmap.subtables)) of
                Just e  -> e
                Nothing -> error "no format 4 character mapping table in font"

glyphIndexLookup12
  :: Word32
  -> Table "Encoding"
  -> Int
glyphIndexLookup12 n table@(Format_12{}) = fromIntegral $ indexFind12 (table.groups) n
glyphIndexLookup12 _ _                   = error "glyphIndexLookup12 was called on an encoding table not in Format 12."

-- glyphIndexLookup n font = glyphIndexLookup12 n enc
--   where Just enc = V.find (\a -> format a == 12) (font.cmap.subtables)
glyphIndexLookup4
  :: Word32
  -> Table "Encoding"
  -> Int
glyphIndexLookup4 n table@(Format_4{}) =
  case (segmentFind4 (fromIntegral n) table.startCode table.endCode) of
    Nothing -> 0
    Just i  -> idCalculate4 n i table
--    Just i  -> trace (show $ idCalculate4 32 2 table) $ idCalculate4 n i table
glyphIndexLookup4 _ _ = error "glyphIndexLookup4 was called on an encoding table not in Format 4."

glyphRangeLookup
  :: Font
  -> GlyphIndexRanges
  -> Word32
  -> (Int, Int, Int, Int)
glyphRangeLookup font (GlyphIndexRanges v) n = case (v V.! i) of
  SimpleGlyph (IndexRange a b) advanceWidth lsb -> (fromIntegral a, fromIntegral $ b - a + 1, fromIntegral $ advanceWidth, fromIntegral $ lsb)
  SpaceGlyph  advanceWidth lsb                  -> (0, 0, fromIntegral advanceWidth, fromIntegral lsb)
  _                                             -> error "glyph range lookup called on a glyph that was not a simple glyph"
  where i = glyphIndexLookup n font

idCalculate4
  :: Word32
  -> Int
  -> Table "Encoding"
  -> Int
idCalculate4 n i table@(Format_4{}) = case (table.idRangeOffsets V.! i) of
  0 -> (fromIntegral n) + (fromIntegral $ table.idDelta V.! i)
  k -> case (fromIntegral $ table.glyphIdArray V.! l) of
         0 -> 0
         j -> j + fromIntegral (table.idDelta V.! i)
       where l = fromIntegral @Word16 $ (div k 2 - div table.segCountX2 2)
               + (fromIntegral n - table.startCode V.! i)
               + fromIntegral i
idCalculate4 _ _ _ = error "idCalculate4 called on table not in format 4"

-- Binary search through a MapGroup record in the cmap table. Assumes the startCharCodes are ordered, and the vector of MapGroups is well-formed. Returns 0 (for the unknown symbol glyph) if the glyph cannot be found.
indexFind12
  :: V.Vector (Record "MapGroup")
  -> Word32
  -> Word32
indexFind12 v n
  | l == 0 = 0
  | l == 1 = case (n >= va.startCharCode && n <= va.endCharCode) of
               True  -> va.startGlyphID + (n - va.startCharCode)
               False -> 0
  | otherwise = case (n < (v V.! k).startCharCode) of
                  True  -> indexFind12 (V.take k v) n
                  False -> indexFind12 (V.drop k v) n
  where l = V.length v
        k = max (div (V.length v - 1) 2) 1
        va = V.head v

rectangle :: Record "Glyph"
rectangle = Simple
  { numberOfContours = 1
  , xMin = -1
  , yMin = -1
  , xMax =  1
  , yMax =  1
  , endPtsOfContours  = V.singleton 3
  , instructionLength = 0
  , instructions = V.empty
  , flags        = V.replicate
                       |- 4
                       |- zeroBits .|. ON_CURVE_POINT
  , xCoordinates = V.fromList [-1,  1,  1, -1]
  , yCoordinates = V.fromList [-1, -1,  1,  1]
  , numDraws     = 4
  , numOffCurve  = 0
  }

-- | Perform a binary search to find the segment in which a character code appears.
segmentFind4
  :: Word16
  -> V.Vector Word16 -- start codes
  -> V.Vector Word16 -- end codes
  -> Maybe Int
segmentFind4 n startCodes endCodes
  | l == 0 = Nothing
  | l == 1 = case (n >= V.head startCodes && n <= V.head endCodes) of
               True  -> Just 0
               False -> Nothing
  | otherwise = case (n <= endCodes V.! k) of
                  True  -> segmentFind4
                             |- n
                             |- V.take (1 + k) startCodes
                             |- V.take (1 + k) endCodes
                  False -> fmap (+ (k+1)) $ segmentFind4
                             |- n
                             |- V.drop (1 + k) startCodes
                             |- V.drop (1 + k) endCodes
  where l  = V.length startCodes
        k  = div (V.length startCodes - 1) 2

-- | Produces points to write into the vertex buffer with
-- | all the necessary bits set, when given the base vertex
-- | and a triple of the start, control and end vertex of the draw call.
setDrawBits
  :: Vertex                         -- glyph base
  -> (Vertex, Maybe Vertex, Vertex) -- (start, control, end)
  -> (Vertex, Maybe Vertex, Vertex) -- (start, control, end)
setDrawBits glyphBase (start, Nothing, end)      = (\(a, _, c) -> (a, Nothing, c))
                                                 $ setPointBits 3
                                                 $ (start, glyphBase, end)
setDrawBits glyphBase (start, Just control, end) = (\(a, b, c) -> (a, Just b, c) )
                                                 $  setPointBits 4
                                                 $ (\(a, _, c) -> (a, control, c))
                                                 $ setPointBits 3
                                                 $ (start, glyphBase, end)

-- | Sets bits 0 and 1 to indicate the position of the vertex in the draw call.
-- | Sets bit n (which should be either 3 or 4) to indicate
-- | the orientation of the triangle in the draw call.
-- | Bit n is set when the (third - second) point vector is
-- | counterclockwise from the (first - second) point vector.
setPointBits
  :: Int
  -> (Vertex, Vertex, Vertex)
  -> (Vertex, Vertex, Vertex)
setPointBits n (p1, p2, p3)
  | orientation > 0 = ( setBitCounterClockwise $ setBitStart  p1
                      , setBitCounterClockwise $ setBitVertex p2
                      , setBitCounterClockwise $ setBitEnd    p3
                      )
  | otherwise       = ( setBitStart  p1
                      , setBitVertex p2
                      , setBitEnd    p3
                      )
  where Vertex x1 y1 _z1 = p1
        Vertex x2 y2 _z2 = p2
        Vertex x3 y3 _z3 = p3
        orientation  = (x1 - x2) * (y3 - y2)
                     - (x3 - x2) * (y1 - y2)
        setBitStart  (Vertex a b c) = Vertex a b (c .|. bit 1)
        setBitVertex (Vertex a b c) = Vertex a b (c          )
        setBitEnd    (Vertex a b c) = Vertex a b (c .|. bit 0)
        setBitCounterClockwise (Vertex a b c) = Vertex a b (c .|. bit n)

-- | Wraps a given value between two others, so that
-- | e.g. wrapIndex 1 5 7 = 3.
wrapIndex
  :: (Integral a)
  => a
  -> a
  -> a
  -> a
wrapIndex start end i = start +       (i    - start    )
                                `mod` (end  - start + 1)

-- | Write a list of elements to the given pointer at the specified position,
-- | and return the resulting index in the pointer.
-- | This is extremely unsafe, since it implicitly
-- | assumes that the pointer has had enough memory allocated
-- | that all elements of the list can be written.
writeAt
  :: (Storable a)
  => Ptr a
  -> Word32
  -> [a]
  -> IO Word32
writeAt _ptr n [] = return n
writeAt  ptr n (a : as) = do pokeElemOff ptr (fromIntegral n) a
                             writeAt ptr (1 + n) as

-- To write the font data, we map writeGlyphData over its glyphs.
writeBuffers
  :: Table "glyf"
  -> Table "hmtx"
  -> Ptr a
  -> Ptr a
  -> IO GlyphIndexRanges
writeBuffers glyf hmtx ptrVertex ptrIndex = do
  let ptrV = castPtr ptrVertex
      ptrI = castPtr ptrIndex
      writer = do
        let (baseX, baseY) = ( 2 * ( fromIntegral rectangle.xMin - 1 )
                             , 2 * ( fromIntegral rectangle.yMin - 1 )
                             )
        lift $ pokeElemOff
                 |- ptrV
                 |- 0
                 |- Vertex baseX baseY (zeroBits .|. bit 2)
        state <- get
        newState <- lift $ chainM
                 |- fromIntegral rectangle.numberOfContours
                 |- writeContourData ptrV ptrI rectangle
                 |- state { vertexLoc      = 1
                          , glyphBaseIndex = 0
                          , contourNum     = 0
                          , contourStart   = 0
                          , contourEnd     = 3
                          }
        put $ newState
        V.mapM
            |- writeGlyphData hmtx ptrV ptrI
            |- glyf.glyphs
  glyphIndexRanges <- fmap
                        |* GlyphIndexRanges
                        |* evalStateT
                             |- writer
                             |- initialState -- { vertexLoc = 9
                                             -- , indexLoc  = 12
                                             -- }
  return $ glyphIndexRanges
  where initialState = WriteState { vertexLoc      = 0
                                  , indexLoc       = 0
                                  , glyphBaseIndex = 0
                                  , writeInst      = 0
                                  , contourNum     = 0
                                  , contourStart   = 0
                                  , contourEnd     = 0
                                  , currentGlyph   = 0
                                  }

-- | This function can only be called on a Simple glyph.
-- | Writes contour data by folding over its points.
-- |
-- | I'd prefer to rewrite this by having first..last in the WriteState structure for consistency,
-- | but that probably doesn't matter.
writeContourData
  :: Ptr Vertex
  -> Ptr Index
  -> Record "Glyph"
  -> WriteState
  -> IO WriteState
writeContourData ptrV ptrI glyph state = do
  let contourNum       = state.contourNum
      update s         = s { contourNum = contourNum + 1 }
      endPtOfContour c = glyph.endPtsOfContours V.! (fromIntegral $ c)
      first = case (contourNum) of
                0 -> 0
                _ -> fromIntegral $ 1 + endPtOfContour (contourNum - 1)
      last  = fromIntegral $ endPtOfContour contourNum
  fmap
    |* update
    |* foldM
         |- writeDraw ptrV ptrI glyph
         |- state { contourStart = first
                  , contourEnd   = last
                  }
         |- [first..last]

-- | Writes vertex and index buffer data, while tracking state.
-- | This function uses setDrawBits to set flags for the vertex shader,
-- | and doubles all point coordinates to defer any rounding involved in
-- | interpolating between consecutive off-curve points.
-- |
-- | Most of the interesting logic is contained in setDrawBits.
-- | This function simply interpolates between consecutive off-curve points,
-- | and naively writes draw call data for 1 or 2 triangles at a time, with
-- | the only optimization being that the 'base' vertex of the glyph
-- | -- chosen to be (xMin - 1, yMin - 1) -- is not duplicated in the
-- | vertex buffer.
-- |
-- | Note that writeDraw will almost certainly write more than numVertices
-- | many vertices into the vertex buffer. This is because triangles are written
-- | independently, and vertices are not reused (except the 'base' vertex of the
-- | glyph). Practically, every on-curve point might lead to 2 vertex writes
-- | and every off-curve point might lead to 3 vertex writes (averages on either side).
-- |
-- | Note, this function indexes into multiple arrays simultaneously
-- | without bounds checks. This is highly unsafe.
writeDraw
  :: Ptr Vertex
  -> Ptr Index
  -> Record "Glyph"
  -> WriteState
  -> Int
  -> IO WriteState
writeDraw pVertex pIndex glyph state n = case ( onCurve n, onCurve $ n+1, onCurve $ n+2 ) of
  (True , True , _    ) -> do let (first, _ , third) = setBits ( vertex n
                                                               , Nothing
                                                               , vertex (n+1)
                                                               )
                              newVertexLoc <- writeVertex [ first
                                                          , third
                                                          ]
                              newIndexLoc  <- writeIndexSingle
                              return $ state { vertexLoc = newVertexLoc
                                             , indexLoc  = newIndexLoc
                                             }
  (True , False, True ) -> do let (first, second, third) = setBits ( vertex n
                                                                   , Just $ vertex (n+1)
                                                                   , vertex (n+2)
                                                                   )
                                  control = case (second) of
                                              Just c  -> c
                                              Nothing -> error "setBits did not return a control point"
                              newVertexLoc <- writeVertex [ first
                                                          , control
                                                          , third
                                                          ]
                              newIndexLoc  <- writeIndexDouble
                              return $ state { vertexLoc = newVertexLoc
                                             , indexLoc  = newIndexLoc
                                             }
  (True , False, False) -> do let (first, second, third) = setBits ( vertex n
                                                                   , Just $ vertex (n+1)
                                                                   , average (n+1) (n+2)
                                                                   )
                                  control = case (second) of
                                              Just c  -> c
                                              Nothing -> error "setBits did not return a control point"
                              newVertexLoc <- writeVertex [ first
                                                          , control
                                                          , third
                                                          ]
                              newIndexLoc  <- writeIndexDouble
                              return $ state { vertexLoc = newVertexLoc
                                             , indexLoc  = newIndexLoc
                                             }
  (False, True , _    ) -> do return $ state
  (False, False, True ) -> do let (first, second, third) = setBits ( average n (n+1)
                                                                   , Just $ vertex (n+1)
                                                                   , vertex (n+2)
                                                                   )
                                  control = case (second) of
                                              Just c  -> c
                                              Nothing -> error "setBits did not return a control point"
                              newVertexLoc <- writeVertex [ first
                                                          , control
                                                          , third
                                                          ]
                              newIndexLoc  <- writeIndexDouble
                              return $ state { vertexLoc = newVertexLoc
                                             , indexLoc  = newIndexLoc
                                             }
  (False, False, False) -> do let (first, second, third) = setBits ( average (n  ) (n+1)
                                                                   , Just $ vertex (n+1)
                                                                   , average (n+1) (n+2)
                                                                   )
                                  control = case (second) of
                                              Just c  -> c
                                              Nothing -> error "setBits did not return a control point"
                              newVertexLoc <- writeVertex [ first
                                                          , control
                                                          , third
                                                          ]
                              newIndexLoc  <- writeIndexDouble
                              return $ state { vertexLoc = newVertexLoc
                                             , indexLoc  = newIndexLoc
                                             }
  where start = state.contourStart
        end   = state.contourEnd
        vLoc  = state.vertexLoc
        bLoc  = state.glyphBaseIndex
        wrap  = wrapIndex start end
        setBits = setDrawBits glyphBase
        writeVertex = writeAt
                         |- pVertex
                         |- state.vertexLoc
        writeIndexSingle = writeAt
                             |- pIndex
                             |- state.indexLoc
                             |- [ vLoc, bLoc, vLoc + 1 ]
        writeIndexDouble = writeAt
                             |- pIndex
                             |- state.indexLoc
                             |- [ vLoc, bLoc, vLoc + 2, vLoc, vLoc + 1, vLoc + 2 ]
        xCoordinate  i = fromIntegral $ glyph.xCoordinates V.! wrap i
        yCoordinate  i = fromIntegral $ glyph.yCoordinates V.! wrap i
        onCurve i   = test $     (glyph.flags V.! wrap i)
                             .&. (ON_CURVE_POINT        )
        glyphBase = Vertex
                      |- 2 * fromIntegral glyph.xMin - 1
                      |- 2 * fromIntegral glyph.yMin - 1
                      |- zeroBits
        vertex  i   = Vertex
                        |- 2 * xCoordinate i
                        |- 2 * yCoordinate i
                        |- zeroBits
        average i j = Vertex
                        |- xCoordinate i + xCoordinate j
                        |- yCoordinate i + yCoordinate j
                        |- zeroBits

-- | To write the glyph data, we chain writeContourData
-- | as many times as there are contours.
-- | Returns the index range written to for use in
-- | building the Vulkan draw call.
-- |
-- | This should probably be modified to check (and special-case in the output)
-- | if numberOfContours == 0, which is possible for a simple glyph.
-- | This could also be made more robust by folding, rather than chaining,
-- | but this would require slicing the glyph data into its contours.
-- | This can be done, but I'm not sure if there's a good reason: it's still unsafe,
-- | (possibly) marginally less performant, and isn't any more elegant.
writeGlyphData
  :: Table "hmtx"
  -> Ptr Vertex
  -> Ptr Index
  -> Record "Glyph"
  -> StateT WriteState IO GlyphDrawInfo
writeGlyphData hmtx ptrV ptrI glyph@(Simple {}) = do
  state <- get
  let baseX = 2 * (fromIntegral glyph.xMin - 1)
      baseY = 2 * (fromIntegral glyph.yMin - 1)
      updateState s = put $ s { currentGlyph = s.currentGlyph + 1 }
  lift $ pokeElemOff
           |- ptrV
           |- fromIntegral state.vertexLoc
           |- Vertex baseX baseY (zeroBits .|. bit 2)
  (updateState =<<) . lift $ chainM
                               |- fromIntegral glyph.numberOfContours
                               |- writeContourData ptrV ptrI glyph
                               |- state { vertexLoc      = state.vertexLoc + 1
                                        , glyphBaseIndex = state.vertexLoc
                                        , contourNum     = 0
                                        }
  newIndexLoc <- (\s -> s.indexLoc) <$> get
  let hMetrics         = hmtx.hMetrics
      leftSideBearings = hmtx.leftSideBearings
  return $ SimpleGlyph
    { drawIndices  = IndexRange { firstr = fromIntegral $ state.indexLoc
                                , lastr  = fromIntegral $ newIndexLoc - 1
                                }
    , advanceWidth = (hMetrics V.! clamp (0, V.length hMetrics - 1) state.currentGlyph).advanceWidth
    , lsb          = leftSideBearings V.! clamp (0, V.length leftSideBearings - 1) state.currentGlyph
    }
writeGlyphData _hmtx _ptrV _ptrI _glyph@(Composite { components }) = do
  let drawdata = V.toList $ fmap
                              |- getComponentData
                              |- components
  modify $ \state -> state { currentGlyph = state.currentGlyph + 1 }
  return $ CompositeGlyph drawdata
writeGlyphData hmtx _ptrV _ptrI _glyph@(Space) = do
  state <- get
  put $ state { currentGlyph = state.currentGlyph + 1}
  let hMetrics         = hmtx.hMetrics
      leftSideBearings = hmtx.leftSideBearings
  return $ SpaceGlyph { advanceWidth = (hMetrics V.! clamp (0, V.length hMetrics - 1) state.currentGlyph).advanceWidth
                      , lsb          = leftSideBearings V.! clamp (0, V.length leftSideBearings - 1) state.currentGlyph
                      }
