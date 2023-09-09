{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE RecordWildCards          #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module Font.TTF.Parse where

import Prelude hiding (head, length)

import Control.Monad
import Control.Monad.State.Strict

import Data.Serialize.Get

import Data.Bits
import Data.Int
import Data.Word

import Data.Foldable hiding (length)
import Data.List     (group, head, sort)

import Data.ByteString qualified as Strict
import Data.Vector     qualified as V

import Font.Generics
import Font.Load
import Font.TTF.Patterns
import Font.TTF.Types
import Font.TTF.Utils

data ParseState = ParseState
  { initialOnCurve  :: !Bool
  , previousOnCurve :: !Bool
  , currentContour  :: !Int
  , currentPoint    :: !Word16
  , numDraws        :: !Word64
  , numOffCurve     :: !Word64
  }


fontDirectory
  :: Word16
  -> Get FontDirectory
fontDirectory n = do
  tableDirectory <- V.replicateM (fromIntegral n) tablePosition
  return $ FontDirectory
    { numTables = n
    , tableDirectory
    }

getArgs
  :: Word16
  -> Get (Int32, Int32)
getArgs n = case ( test $ n .&. ARG_1_AND_2_ARE_WORDS
                 , test $ n .&. ARGS_ARE_XY_VALUES
                 ) of
  (True, True)   -> do a <- getInt16be
                       b <- getInt16be
                       return (fromIntegral a,fromIntegral b)
  (True, False)  -> do a <- getWord16be
                       b <- getWord16be
                       return (fromIntegral a, fromIntegral b)
  (False, True)  -> do a <- getInt8
                       b <- getInt8
                       return (fromIntegral a, fromIntegral b)
  (False, False) -> do a <- getWord8
                       b <- getWord8
                       return (fromIntegral a, fromIntegral b)

getInstructions
  :: Bool
  -> Get (Word16, V.Vector Word8)
getInstructions False = pure (0, V.empty)
getInstructions True = do n <- getWord16be
                          w <- V.replicateM (fromIntegral n) getWord8
                          return (n, w)

glyph
  :: Table "loca"
  -> Int
  -> (StateT (Word64, Word64, Word64, Word64) Get) (Record "Glyph")
glyph loca n = do
  let (a, b) = (offsets loca V.! n, offsets loca V.!? (n+1))
  case (Just a == b) of
    True  -> pure Space
    False -> do n' <- lift bytesRead
                lift $ skip ( fromIntegral a - n' )
                g <- lift glyphNonEmpty
                case g of
                  Simple{} -> do (numDraws, numVertices, numOffCurve, numSimple) <- get
                                 put ( numDraws + g.numDraws
                                     , numVertices + (fromIntegral $ V.last (endPtsOfContours g) + 1)
                                     , numOffCurve + g.numOffCurve
                                     , numSimple + 1
                                     )
                                 return g
                  _        -> return g

glyphNonEmpty :: Get (Record "Glyph")
glyphNonEmpty = do
  numberOfContours <- getInt16be
  xMin <- getInt16be
  yMin <- getInt16be
  xMax <- getInt16be
  yMax <- getInt16be
  case (numberOfContours > 0) of
    True -> do endPtsOfContours  <- V.replicateM (fromIntegral numberOfContours ) getWord16be
               instructionLength <- getWord16be
               instructions      <- V.replicateM (fromIntegral instructionLength) getWord8
               let k =  (fromIntegral $ V.last endPtsOfContours) + 1
               (flags, ParseState{numDraws, numOffCurve}) <- runStateT (V.unfoldrExactNM k (parse_flag endPtsOfContours) (0,0)) initialState
               xCoordinates <- V.unfoldrExactNM k (xCoordinate flags) (0,0)
               yCoordinates <- V.unfoldrExactNM k (yCoordinate flags) (0,0)
               return $ Simple
                 { numberOfContours
                 , xMin
                 , yMin
                 , xMax
                 , yMax
                 , endPtsOfContours
                 , instructionLength
                 , instructions
                 , flags
                 , xCoordinates
                 , yCoordinates
                 , numDraws
                 , numOffCurve
                 }
    False -> do (components, b) <- runStateT (V.unfoldrM parse_component True) False
                (instructionLength, instructions) <- getInstructions b
                return $ Composite
                  { numberOfContours
                  , xMin
                  , yMin
                  , xMax
                  , yMax
                  , components
                  , instructionLength
                  , instructions
                  }

initialState :: ParseState
initialState = ParseState { initialOnCurve  = False
                          , previousOnCurve = False
                          , currentContour  = 0
                          , currentPoint    = 0
                          , numDraws        = 0
                          , numOffCurve     = 0
                          }

-- This should really be pure if a is a strict ByteString rather than a filepath

loadTables
  :: (Slice a)
  => a
  -> IO RawFont
loadTables a = do
  headerRaw <- slice a (0,12)
  let n = throwFailure $ runGet (skip 4 >> getWord16be) $ headerRaw
  tableDirectoryRaw <- slice a (12, 16*(fromIntegral n))
  let fd = throwFailure $ runGet (fontDirectory n) $ tableDirectoryRaw
  sliceTables a (tableDirectory fd)

parse_Font
  :: RawFont
  -> Font
parse_Font raw = Font { cmap
                      , glyf
                      , head
                      , hhea
                      , hmtx
                      , loca
                      , maxp
                      , post
                      }
  where cmap = throwFailure $ runGet (parse_cmap $ snd
               $ cmap_r raw  )
               $ rawBS cmap_r
        glyf = throwFailure $ runGet (parse_glyf loca maxp)
               $ rawBS glyf_r
        head = throwFailure $ runGet parse_head  $ rawBS head_r
        hhea = throwFailure $ runGet parse_hhea  $ rawBS hhea_r
        hmtx = throwFailure $ runGet (parse_hmtx hhea maxp)
               $ rawBS hmtx_r
        loca = throwFailure $ runGet (parse_loca head maxp)
                           $ rawBS loca_r
        maxp = throwFailure $ runGet parse_maxp  $ rawBS maxp_r
        post = throwFailure $ runGet parse_post  $ rawBS post_r
        rawBS field = snd $ field raw

parse_cmap
  :: Strict.ByteString
  -> Get (Table "cmap")
parse_cmap rawBS = do
  version         <- getWord16be
  numTables       <- getWord16be
  encodingRecords <- V.replicateM (fromIntegral numTables)
                                  (parse_encodingRecord  )
  let (uniqueSubtables :: [Word32]) = ( fmap Data.List.head
                                      . Data.List.group
                                      . sort
                                      . fmap subtableOffset
                                      . toList
                                      )
                                      ( encodingRecords
                                      )
  -- It would definitely be safer to refactor this so that parse_encodingTable simply splices out the section of rawBS that it needs to use
  subtables <- V.sequence $ V.fromList
                          $ fmap (parse_encodingTable)
                          $ Prelude.zip uniqueSubtables (Prelude.drop 1 uniqueSubtables ++ [(fromIntegral $ Strict.length rawBS)])
  return $ T_cmap
    { version
    , numTables
    , encodingRecords
    , subtables
    }

parse_component
  :: Bool
  -> (StateT Bool Get) (Maybe (Record "Component", Bool))
parse_component False = pure Nothing
parse_component True  = do flags                  <- lift getWord16be
                           glyphIndex             <- lift getWord16be
                           (argument1, argument2) <- lift $ getArgs flags
                           transformation         <- lift $ parse_transformation flags
                           state <- get
                           put (state || (test $ flags .&. WE_HAVE_INSTRUCTIONS))
                           return $ Just $ ( Component
                                               { flags
                                               , glyphIndex
                                               , argument1
                                               , argument2
                                               , transformation
                                               }
                                           , test $ flags .&. MORE_COMPONENTS
                                           )

parse_constantMapGroup :: Get (Record "MapGroup")
parse_constantMapGroup = into ConstantMapGroup

parse_encodingRecord :: Get (Record "Encoding")
parse_encodingRecord = into Encoding

parse_encodingTable
  :: (Word32, Word32)
  -> Get (Table "Encoding")
parse_encodingTable (offset, end) = do
  pos <- bytesRead
  skip ( fromIntegral offset - pos )
  format <- getWord16be
  case format of
    0  -> error "'cmap' table in format 0 is not currently supported."
    2  -> error "'cmap' table in format 2 is not currently supported."
    4  -> parse_encodingTable_4 $ fromIntegral end
    6  -> error "'cmap' table in format 6 is not currently supported."
    8  -> error "'cmap' table in format 8 is not currently supported."
    10 -> error "'cmap' table in format 10 is not currently supported."
    12 -> parse_encodingTable_12
    13 -> error "'cmap' table in format 13 is not currently supported."
    14 -> error "'cmap' table in format 14 is not currently supported."
    n  -> error $ "'cmap' subtable format" ++ (show n) ++ "is invalid."

parse_encodingTable_12 :: Get (Table "Encoding")
parse_encodingTable_12 = do
  let format = 12
  reserved <- getWord16be
  length   <- getWord32be
  language <- getWord32be
  numGroups <- getWord32be
  groups <- V.replicateM (fromIntegral numGroups) parse_sequentialMapGroup
  return $ Format_12
    { format
    , reserved
    , length
    , language
    , numGroups
    , groups
    }

parse_encodingTable_4
  :: Int64
  -> Get (Table "Encoding")
parse_encodingTable_4 n = do
  let format = 4
  length     <- fromIntegral <$> getWord16be
  language   <- fromIntegral <$> getWord16be
  segCountX2 <- getWord16be
  let segCount = fromIntegral $ div segCountX2 2
  searchRange    <- getWord16be
  entrySelector  <- getWord16be
  rangeShift     <- getWord16be
  endCode        <- V.replicateM segCount getWord16be
  reservedPad    <- getWord16be
  startCode      <- V.replicateM segCount getWord16be
  idDelta        <- V.replicateM segCount getInt16be
  idRangeOffsets <- V.replicateM segCount getWord16be
  pos          <- bytesRead
  glyphIdArray <- V.replicateM (div (fromIntegral n - pos) 2) getWord16be
  return $ Format_4
    { format
    , length
    , language
    , segCountX2
    , searchRange
    , entrySelector
    , rangeShift
    , endCode
    , reservedPad
    , startCode
    , idDelta
    , idRangeOffsets
    , glyphIdArray
    }

parse_flag
  :: V.Vector Word16
  -> (Word8, Int)
  -> (StateT ParseState Get) (Word8, (Word8, Int))
parse_flag v (_, 0) = do flag <- lift $ getWord8
                         state <- get
                         put $ updateDrawCount v (test $ flag .&. ON_CURVE_POINT) state
                         case (test $ flag .&. REPEAT_FLAG) of
                           True  -> do n <- lift $ getWord8
                                       return $ (  flag
                                                , (flag, fromIntegral n)
                                                ) -- spec states "repeat n additional times". possible that spec is just poorly written and this is supposed to be n-1
                           False ->    return $ (  flag
                                                , (flag, 0)
                                                )
parse_flag v (flag, n) = do state <- get
                            put $ updateDrawCount v (test $ flag .&. ON_CURVE_POINT) state
                            return $ (  flag
                                     , (flag, n - 1)
                                     )

parse_glyf
  :: Table "loca"
  -> Table "maxp"
  -> Get (Table "glyf")
parse_glyf loca maxp = do
  let glyphStateT = V.generateM (fromIntegral $ maxp.numGlyphs)
                                (glyph loca                   )
  (glyphs, (numDraws, numVertices, numOffCurve, numSimple)) <- runStateT glyphStateT (0,0,0,0)
  return $ T_glyf
    { glyphs
    , numDraws
    , numVertices
    , numOffCurve
    , numSimple
    }

parse_head :: Get (Table "head")
parse_head = into T_head

parse_hhea :: Get (Table "hhea")
parse_hhea = into T_hhea

parse_hmtx
  :: Table "hhea"
  -> Table "maxp"
  -> Get (Table "hmtx")
parse_hmtx hhea maxp = T_hmtx <$> V.replicateM (k) parse_longHorMetric
                              <*> V.replicateM (n - k) getInt16be
  where n = fromIntegral $ maxp.numGlyphs
        k = fromIntegral $ numberOfHMetrics hhea

parse_loca
  :: Table "head"
  -> Table "maxp"
  -> Get (Table "loca")
parse_loca head maxp = T_loca <$> V.replicateM (fromIntegral n) getWord
  where n = maxp.numGlyphs
        getWord = case (indexToLocFormat head) of
                    0 -> fmap ((*2) . fromIntegral) getWord16be
                    1 -> getWord32be
                    _ -> error "Invalid 'indexToLocFormat' in table 'head'"

parse_longHorMetric :: Get (Record "longHorMetric")
parse_longHorMetric = into LongHorMetric

parse_maxp :: Get (Table "maxp")
parse_maxp = do
  version <- getVersion16Dot16
  case (version.major, version.minor) of
    (0,5) -> into $ T_maxp_0_5 version
    (1,0) -> into $ T_maxp_1_0 version
    _     -> error "Invalid version number in 'maxp' table."

-- parse_post should be made to properly handle the different 'post' table formats, but I only plan on using the header information (if anything) at the moment.

parse_post :: Get (Table "post")
parse_post = into T_post_1_0

parse_sequentialMapGroup :: Get (Record "MapGroup")
parse_sequentialMapGroup = into SequentialMapGroup

parse_subHeaderRecord :: Get (Record "SubHeader")
parse_subHeaderRecord = into SubHeader

parse_transformation
  :: Word16
  -> Get (Record "Transformation")
parse_transformation flags
  | test $ flags .&. WE_HAVE_A_SCALE          = SCALE <$> getF2DOT14
  | test $ flags .&. WE_HAVE_AN_X_AND_Y_SCALE = X_AND_Y_SCALE <$> getF2DOT14
                                                              <*> getF2DOT14
  | test $ flags .&. WE_HAVE_A_TWO_BY_TWO     = TWO_BY_TWO <$> getF2DOT14
                                                           <*> getF2DOT14
                                                           <*> getF2DOT14
                                                           <*> getF2DOT14
  | otherwise                                 = pure $ SCALE 1

-- Optional tables are Maybe, required tables are errors (if used but not initialized)

rawFont :: RawFont
rawFont = RawFont { cmap_r = error "Could not find table 'cmap'"
                  , glyf_r = error "Could not find table 'glyf'"
                  , head_r = error "Could not find table 'head'"
                  , hhea_r = error "Could not find table 'hhea'"
                  , hmtx_r = error "Could not find table 'hmtx'"
                  , loca_r = error "Could not find table 'loca'"
                  , maxp_r = error "Could not find table 'maxp'"
                  , post_r = error "Could not find table 'post'"
                  }

sliceTables
  :: (Slice a)
  => a
  -> V.Vector TablePosition
  -> IO RawFont
sliceTables a v = foldM (updateWithTablePosition a) rawFont v

tablePosition :: Get TablePosition
tablePosition = do
  a <- getWord8
  b <- getWord8
  c <- getWord8
  d <- getWord8
  skip 4
  offset <- getWord32be
  length <- getWord32be
  return $ TablePosition
    { tag = fmap (toEnum . fromIntegral) [a,b,c,d]
    , offset
    , length
    }

-- updateDrawCount takes an array of contour endpoints and a boolean flag indicating whether the current point is on-curve (True) or off-curve (False), and updates the ParseState count for the number of draw calls (numDraws) and number of off-curve points (numOffCurve), with book-keeping for the other entries.

updateDrawCount
  :: V.Vector Word16
  -> Bool
  -> ParseState
  -> ParseState
updateDrawCount endPts onCurve state@ParseState{ currentPoint    = i
                                               , currentContour  = n
                                               }
  | i == 0    = state { previousOnCurve = onCurve
                      , initialOnCurve  = onCurve
                      , currentPoint    = state.currentPoint   + 1
                      , numOffCurve     = state.numOffCurve    + (fromIntegral . fromEnum . not $ onCurve)
                      }
  | i <  k    = state { previousOnCurve = onCurve
                      , currentContour  = state.currentContour
                      , currentPoint    = state.currentPoint   + 1
                      , numDraws        = state.numDraws       + f (state.previousOnCurve) onCurve
                      , numOffCurve     = state.numOffCurve    + (fromIntegral . fromEnum . not $ onCurve)
                      }
  | i == k    = state { previousOnCurve = onCurve
                      , currentContour  = state.currentContour
                      , currentPoint    = state.currentPoint   + 1
                      , numDraws        = state.numDraws       + f (state.previousOnCurve) onCurve
                                                               + f onCurve (state.initialOnCurve)
                      , numOffCurve     = state.numOffCurve    + (fromIntegral . fromEnum . not $ onCurve)
                      }
  | otherwise = state { previousOnCurve = onCurve
                      , initialOnCurve  = onCurve
                      , currentContour  = state.currentContour + 1
                      , currentPoint    = state.currentPoint   + 1
                      , numOffCurve     = state.numOffCurve    + (fromIntegral . fromEnum . not $ onCurve)
                      }
  where k = endPts V.! n
        f prev cur = case (prev, cur) of
        -- If the current point is off-curve, there are two draw calls: one where the point is the middle vertex of the triangle, and another where the basepoint of the glyph is the middle vertex.
          (_    , False) -> 2
        -- If the current point is on-curve and preceded by an on-curve point, there is a single draw call where the basepoint of the glyph is the middle vertex, and the remaining two vertices are the on-curve points.
          (True , True ) -> 1
        -- If the current point is on-curve but was preceded by an off-curve point, it was part of that off-curve point's draw call.
          (False, True ) -> 0

updateWithTablePosition
  :: (Slice a)
  => a
  -> RawFont
  -> TablePosition
  -> IO RawFont
updateWithTablePosition a fr tp = do
  case (tag tp) of
    "cmap" -> extract $ \b -> fr { cmap_r = (tp,b) }
    "glyf" -> extract $ \b -> fr { glyf_r = (tp,b) }
    "head" -> extract $ \b -> fr { head_r = (tp,b) }
    "hhea" -> extract $ \b -> fr { hhea_r = (tp,b) }
    "hmtx" -> extract $ \b -> fr { hmtx_r = (tp,b) }
    "loca" -> extract $ \b -> fr { loca_r = (tp,b) }
    "maxp" -> extract $ \b -> fr { maxp_r = (tp,b) }
    "post" -> extract $ \b -> fr { post_r = (tp,b) }
    _      -> pure $ fr
  where extract f = do bs <- slice a ( fromIntegral $ tp.offset
                                     , fromIntegral $ tp.length
                                     )
                       pure $ f bs

xCoordinate
  :: V.Vector Word8
  -> (Int32, Int)
  -> Get (Int32, (Int32, Int))
xCoordinate flags (xLast, index) = case ( test $ (flags V.! index) .&. X_SHORT_VECTOR
                                        , test $ (flags V.! index) .&. X_IS_SAME_OR_POSITIVE_X_SHORT_VECTOR
                                        ) of
  (True , True ) -> do xCurrent <- getWord8
                       let xNew = xLast + fromIntegral xCurrent
                       return $ (  xNew
                                , (xNew, index + 1)
                                )
  (True , False) -> do xCurrent <- getWord8
                       let xNew = xLast - fromIntegral xCurrent
                       return $ (  xNew
                                , (xNew, index + 1)
                                )
  (False, True ) ->    return $ (  xLast
                                , (xLast, index + 1)
                                )
  (False, False) -> do xCurrent <- getInt16be
                       let xNew = xLast + fromIntegral xCurrent
                       return $ (  xNew
                                , (xNew, index + 1)
                                )

yCoordinate
  :: V.Vector Word8
  -> (Int32, Int)
  -> Get (Int32, (Int32, Int))
yCoordinate flags (yLast, index) = case ( test $ (flags V.! index) .&. Y_SHORT_VECTOR
                                        , test $ (flags V.! index) .&. Y_IS_SAME_OR_POSITIVE_Y_SHORT_VECTOR
                                        ) of
  (True , True ) -> do yCurrent <- getWord8
                       let yNew = yLast + fromIntegral yCurrent
                       return $ (  yNew
                                , (yNew, index + 1)
                                )
  (True , False) -> do yCurrent <- getWord8
                       let yNew = yLast - fromIntegral yCurrent
                       return $ (  yNew
                                , (yNew, index + 1)
                                )
  (False, True ) ->    return $ (  yLast
                                , (yLast, index + 1)
                                )
  (False, False) -> do yCurrent <- getInt16be
                       let yNew = yLast + fromIntegral yCurrent
                       return $ (  yNew
                                , (yNew, index + 1)
                                )
