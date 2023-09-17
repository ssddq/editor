{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE NoFieldSelectors         #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE UndecidableInstances     #-}

module Types where

import Common
import SDL qualified
import Streaming qualified as S
import VMA

import Data.ByteString qualified as BS
import Data.Kind
import Data.Text
import Data.Vector     qualified as V
import Data.Word

import Foreign.Ptr
import Foreign.Storable

import GHC.Records  qualified as GHC
import GHC.TypeLits qualified as Type

import Graphics.Vulkan.Core_1_0
import Graphics.Vulkan.Ext.VK_KHR_swapchain

-- * Special type parameter flags

-- | Flag that a field in Vk has been initialized with a value of type `a`.

data A a

-- | Flag that a field in Vk has been initialized with a value of its corresponding type.

data I


-- | Flag that a field in Vk has not been initialized, and has type ().

type X = ()



-- * Vulkan data types

data Area = Area
  { width  :: {-# UNPACK #-} !Word32
  , height :: {-# UNPACK #-} !Word32
  }
  deriving (Show)

data Attachment = Attachment
  { image      :: {-# UNPACK #-} !VkImage
  , imageView  :: {-# UNPACK #-} !VkImageView
  , allocation :: {-# UNPACK #-} !VmaAllocation
  }

data Available

data Buffer = Buffer
  { buffer     :: {-# UNPACK #-} !VkBuffer
  , allocation :: {-# UNPACK #-} !VmaAllocation
  , ptr        :: {-# UNPACK #-} !(Ptr Void)
  }

data Buffers = Buffers
  { framebuffer   :: {-# UNPACK #-} !Framebuffers
  , commandBuffer :: {-# UNPACK #-} !VkCommandBuffer
  , indirectDraw  :: {-# UNPACK #-} !Buffer
  , instanceData  :: {-# UNPACK #-} !Buffer
  }

data Build = Build
  { presentMode    :: {-# UNPACK #-} !VkPresentModeKHR
  , instanceLayers :: ![String]
  }

data CharacterMap = Map Word32 Int

data Constants = Constants
  { name       :: {-# UNPACK #-} !Text
  , imageCount :: {-# UNPACK #-} !Word32
  , ppi        :: {-# UNPACK #-} !Float
  , fSize      :: {-# UNPACK #-} !Float
  , render     :: {-# UNPACK #-} !Area
  , present    :: {-# UNPACK #-} !Area
  }
  deriving (Show)

data Descriptors = Descriptors
  { set    :: {-# UNPACK #-} !VkDescriptorSet
  , layout :: {-# UNPACK #-} !VkDescriptorSetLayout
  , pool   :: {-# UNPACK #-} !VkDescriptorPool
  }

data Done

data DrawData = DrawData
  { xOffset :: {-# UNPACK #-} !Float
  , yOffset :: {-# UNPACK #-} !Float
  , xx      :: {-# UNPACK #-} !Float
  , xy      :: {-# UNPACK #-} !Float
  , yx      :: {-# UNPACK #-} !Float
  , yy      :: {-# UNPACK #-} !Float
  , fSize   :: {-# UNPACK #-} !Float
  , color   :: {-# UNPACK #-} !Color
  }

data Fences = Fences
  { inFlight :: {-# UNPACK #-} !VkFence
  }

data Font = Font
  { vertex       :: {-# UNPACK #-} !Buffer
  , index        :: {-# UNPACK #-} !Buffer
  , scale        :: {-# UNPACK #-} !Float
  , lineHeight   :: {-# UNPACK #-} !Float
  , textHeight   :: {-# UNPACK #-} !Float
  , unitsPerEmX2 :: {-# UNPACK #-} !Word32
  , ascender     :: {-# UNPACK #-} !Int16
  , descender    :: {-# UNPACK #-} !Int16
  , lineGap      :: {-# UNPACK #-} !Int16
  , lookup       :: Word32 -> (Int, Int, Int, Int)
  }

data Framebuffers = Framebuffers
  { renderPass0 :: {-# UNPACK #-} !VkFramebuffer
  , renderPass1 :: {-# UNPACK #-} !VkFramebuffer
  }

data FullscreenBuffer = FullscreenBuffer
  { vertex     :: {-# UNPACK #-} !Buffer
  , index      :: {-# UNPACK #-} !Buffer
  , indexCount :: {-# UNPACK #-} !Word32
  }

data Pipeline = Pipeline
  { handle :: {-# UNPACK #-} !VkPipeline
  , layout :: {-# UNPACK #-} !VkPipelineLayout
  }

data PipelineShaders = PipelineShaders
  { draw    :: {-# UNPACK #-} !BS.ByteString
  , resolve :: {-# UNPACK #-} !BS.ByteString
  , clear   :: {-# UNPACK #-} !BS.ByteString
  , aa      :: {-# UNPACK #-} !BS.ByteString
  }

data PushConstants = PushConstants
  { area :: {-# UNPACK #-} !Area
  }

-- | RenderPasses contain their Vulkan handle,
-- | as well as subpass structure.

data RenderPass0 = RenderPass0
  { handle   :: {-# UNPACK #-} !VkRenderPass
  , draw0    :: {-# UNPACK #-} !Subpass
  , resolve0 :: {-# UNPACK #-} !Subpass
  , clear0   :: {-# UNPACK #-} !Subpass
  , draw1    :: {-# UNPACK #-} !Subpass
  , resolve1 :: {-# UNPACK #-} !Subpass
  , clear1   :: {-# UNPACK #-} !Subpass
  , draw2    :: {-# UNPACK #-} !Subpass
  , resolve2 :: {-# UNPACK #-} !Subpass
  , clear2   :: {-# UNPACK #-} !Subpass
  , draw3    :: {-# UNPACK #-} !Subpass
  , resolve3 :: {-# UNPACK #-} !Subpass
  }

data RenderPass1 = RenderPass1
  { handle :: {-# UNPACK #-} !VkRenderPass
  , aa     :: {-# UNPACK #-} !Subpass
  }

-- | Contains all relevant Vulkan render pipeline data,
-- | including renderpass/subpass structure, pipelines,
-- | attachments and descriptors.
-- |
-- | This should probably be extended over swapchains.

data RenderPipeline = RenderPipeline
  { renderPass0      :: {-# UNPACK #-} !RenderPass0
  , renderPass1      :: {-# UNPACK #-} !RenderPass1
  , colorAttachment  :: {-# UNPACK #-} !Attachment
  , sampleAttachment :: {-# UNPACK #-} !Attachment
  , sampler          :: {-# UNPACK #-} !(Ptr VkSampler)
  }

data Semaphores = Semaphores
  { available :: {-# UNPACK #-} !VkSemaphore
  , done      :: {-# UNPACK #-} !VkSemaphore
  }

data Shaders = Shaders
  { vertex   :: {-# UNPACK #-} !BS.ByteString
  , fragment :: {-# UNPACK #-} !BS.ByteString
  }

data Signals = Signals
  { semaphores :: {-# UNPACK #-} !Semaphores
  , fences     :: {-# UNPACK #-} !Fences
  }

data StreamBuffer b = StreamBuffer
  { textBuffer            :: b
  , streamer              :: b -> S.Stream (S.Of Symbol) IO ()
  , updateVisualLineCount :: Int -> b -> b
  , bgColor               :: {-# UNPACK #-} !Color
  }

-- | Subpass contains its pipeline, pipeline layout and descriptor info.

data Subpass = Subpass
  { pipeline    :: {-# UNPACK #-} !Pipeline
  , descriptors :: {-# UNPACK #-} !Descriptors
  }

data VkData a = VkData
  { vkData      :: [a]
  , vkDataSize  :: {-# UNPACK #-} !Word64
  , vkDataCount :: {-# UNPACK #-} !Word32
  }

data Vulkan = Vulkan
  { window           :: {-# UNPACK #-} !SDL.Window
  , instance_        :: {-# UNPACK #-} !VkInstance
  , physicalDevice   :: {-# UNPACK #-} !VkPhysicalDevice
  , queueFamilyIndex :: {-# UNPACK #-} !Word32
  , device           :: {-# UNPACK #-} !VkDevice
  , surface          :: {-# UNPACK #-} !VkSurfaceKHR
  , allocator        :: {-# UNPACK #-} !VmaAllocator
  , swapchain        :: {-# UNPACK #-} !VkSwapchainKHR
  , imageViews       :: {-# UNPACK #-} !ImageViews
  }

data WriteState = WriteState
  { instanceNum :: {-# UNPACK #-} !Word32
  , positionX   :: {-# UNPACK #-} !Float
  , positionY   :: {-# UNPACK #-} !Float
  , xMax        :: {-# UNPACK #-} !Float
  , yMax        :: {-# UNPACK #-} !Float
  , xMin        :: {-# UNPACK #-} !Float
  , lines       :: {-# UNPACK #-} !Int
  , color       :: {-# UNPACK #-} !Color
  }


newtype FragmentShaderModules = FragmentShaderModules [VkShaderModule]

newtype Queue l = Queue VkQueue

newtype Semaphore l = Semaphore VkSemaphore

newtype VertexShaderModules = VertexShaderModules [VkShaderModule]


type CommandPools   = V.Vector VkCommandPool

type DrawBuffers = V.Vector Buffers

type Extensions  = [CString]

type Field :: Type -> k -> Type
type family Field a b where
  Field ()    b = ()
  Field I     b = b
  Field (A a) f = f a
  Field a     b = Type.TypeError (Type.Text "Invalid type parameter in Vk")

type FontWriter =
     forall buffer void. (GHC.HasField "ptr" buffer (Ptr void))
  => (Word64 -> IO buffer)
  -> (Word64 -> IO buffer)
  -> IO (buffer, buffer, FontData)

type GraphicsQueueFamily = Word32

type ImageViews     = V.Vector VkImageView

type Layers      = [String]

-- | Null f is the type obtained by evaluating f
-- | at () in each of its type parameters.

type Null :: k -> Type
type family Null a where
  Null (f :: Type -> k) = Null (f ())
  Null (a :: Type     ) = a

-- | Target f is the type of the result of f,
-- | when fully evaluated.

type Target :: k -> Type
type family Target a where
  Target (a -> b) = Target b
  Target a = a


class CreateVkData a where
  createVkData :: [a] -> VkData a

instance (Storable a) => CreateVkData a where
  createVkData as = VkData as (fromIntegral $ sizeOf @a undefined * count) (fromIntegral count)
    where count = Prelude.length as

class Empty a where
  createEmpty :: a -> Null (Target a)

instance {-# OVERLAPPING #-} (Empty b) => Empty (() -> b) where
  createEmpty f = createEmpty (f ())

instance (Target a ~ a) => Empty a where
  createEmpty a = a

instance Storable DrawData where
  sizeOf _ = 32
  alignment _ = 32
  peek ptr = do
    xOffset <- peek $ castPtr ptr
    yOffset <- peekByteOff (castPtr ptr) 4
    xx      <- peekByteOff (castPtr ptr) 8
    xy      <- peekByteOff (castPtr ptr) 12
    yx      <- peekByteOff (castPtr ptr) 16
    yy      <- peekByteOff (castPtr ptr) 20
    fSize   <- peekByteOff (castPtr ptr) 24
    color   <- peekByteOff (castPtr ptr) 28
    return $ DrawData
      { xOffset
      , yOffset
      , fSize
      , xx
      , xy
      , yx
      , yy
      , color
      }
  {-# INLINE poke #-}
  poke ptr drawData = do
    poke        (castPtr ptr)    drawData.xOffset
    pokeByteOff (castPtr ptr) 4  drawData.yOffset
    pokeByteOff (castPtr ptr) 8  drawData.xx
    pokeByteOff (castPtr ptr) 12 drawData.xy
    pokeByteOff (castPtr ptr) 16 drawData.yx
    pokeByteOff (castPtr ptr) 20 drawData.yy
    pokeByteOff (castPtr ptr) 24 drawData.fSize
    pokeByteOff (castPtr ptr) 28 drawData.color

instance Storable Area where
  sizeOf    _ = 8
  alignment _ = 8
  peek ptr = do
    width  <- peek (castPtr ptr)
    height <- peek (castPtr ptr)
    return $ Area { width, height }
  {-# INLINE poke #-}
  poke ptr Area { width, height } = do
    poke        (castPtr ptr)   width
    pokeByteOff (castPtr ptr) 4 height


{-# INLINE toStream #-}
toStream
  :: StreamBuffer b
  -> S.Stream (S.Of Symbol) IO ()
toStream (StreamBuffer b f _ _) = f b
