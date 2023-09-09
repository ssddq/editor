import Distribution.Simple
import System.Process
import Gen
import qualified Data.ByteString as B

import Data.Attoparsec.ByteString.Char8

main = do
  vkExtraFile <- B.readFile "extra-headers/vk_extra.h"
  vmaFile     <- B.readFile "VulkanMemoryAllocator/include/vk_mem_alloc.h"
  let Right vkParsed = parseOnly file vkExtraFile
  let Right vmaParsed = parseOnly file vmaFile
  writeFile "hsc/VkExtra.hsc" (vkImports ++ vkParsed)
  writeFile "hsc/VMA.hsc" (vmaImports ++ vmaParsed)
  defaultMain

vmaImports = "#include \"vk_mem_alloc.h\"\n\
\#include \"vulkan/vulkan.h\"\n\
\{-# LANGUAGE CPP #-}\n\
\{-# LANGUAGE KindSignatures #-}\n\
\{-# LANGUAGE DataKinds #-}\n\
\{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
\{-# LANGUAGE PatternSynonyms #-}\n\
\{-# LANGUAGE StandaloneDeriving #-}\n\
\{-# LANGUAGE FlexibleInstances #-}\n\
\{-# LANGUAGE TypeFamilies #-}\n\
\\n\
\module VMA (module VMA, module VkExtra) where\n\
\\n\
\import Prelude \n\
\import Foreign.Ptr\n\
\import Foreign.Storable\n\
\import Data.Bits\n\
\import Data.Word\n\
\\n\
\import VkExtra \n\
\\n\
\import Graphics.Vulkan\n\
\import Graphics.Vulkan.Core_1_0\n\
\\n\
\import Graphics.Vulkan.Marshal.Internal\n\
\\n\
\import Graphics.Vulkan.Ext.VK_KHR_bind_memory2\n\
\import Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2\n\
\import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2\n\n"

vkImports = "#include \"vk_mem_alloc.h\"\n\
\#include \"vulkan/vulkan.h\"\n\
\{-# LANGUAGE CPP #-}\n\
\{-# LANGUAGE KindSignatures #-}\n\
\{-# LANGUAGE DataKinds #-}\n\
\{-# LANGUAGE GeneralizedNewtypeDeriving #-}\n\
\{-# LANGUAGE PatternSynonyms #-}\n\
\{-# LANGUAGE StandaloneDeriving #-}\n\
\{-# LANGUAGE FlexibleInstances #-}\n\
\{-# LANGUAGE TypeFamilies #-}\n\
\\n\
\module VkExtra where\n\
\\n\
\import Prelude hiding (sinh)\n\
\import Foreign.Ptr\n\
\\n\
\import Graphics.Vulkan\n\
\import Graphics.Vulkan.Core_1_0\n\
\\n\
\import Graphics.Vulkan.Marshal.Internal\n\
\\n\
\import Graphics.Vulkan.Ext.VK_KHR_bind_memory2\n\
\import Graphics.Vulkan.Ext.VK_KHR_get_memory_requirements2\n\
\import Graphics.Vulkan.Ext.VK_KHR_get_physical_device_properties2\n\n"
