{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.RenderPass where

import Vk

import Pipeline.Info

-- | Creates the draw renderpass, as described in Pipeline.RenderPass.Info

createRenderPass
  :: VkDevice
  -> IO VkRenderPass
createRenderPass device = do
  let attachment0 = mkAttachmentDescription
                      |- pass.attachment0.format
                      |- (pass.attachment0.loadOp , pass.attachment0.initial)
                      |- (pass.attachment0.storeOp, pass.attachment0.final  )
      attachment1 = mkAttachmentDescription
                      |- pass.attachment1.format
                      |- (pass.attachment1.loadOp , pass.attachment1.initial)
                      |- (pass.attachment1.storeOp, pass.attachment1.final  )
      renderPassCreateInfo = mkRenderPassCreateInfo
                               |- [ attachment0, attachment1 ]
                               |- [ pass.draw0.description, pass.resolve0.description, pass.clear0.description
                                  , pass.draw1.description, pass.resolve1.description, pass.clear1.description
                                  , pass.draw2.description, pass.resolve2.description, pass.clear2.description
                                  , pass.draw3.description, pass.resolve0.description
                                  ]
                               |- pass.dependencies
  perform $ vkCreateRenderPass
              |- device
              |- p renderPassCreateInfo
              |- VK_NULL
  where pass = renderPipelineInfo.drawPass

-- | Creates the antialiasing renderpass, as described in Pipeline.RenderPass.Info

createRenderPass1
  :: VkDevice
  -> IO VkRenderPass
createRenderPass1 device = do
  let attachment0 = mkAttachmentDescription
                      |- pass.attachment0.format
                      |- (pass.attachment0.loadOp , pass.attachment0.initial)
                      |- (pass.attachment0.storeOp, pass.attachment0.final  )
      attachment1 = mkAttachmentDescription
                      |- pass.attachment1.format
                      |- (pass.attachment1.loadOp , pass.attachment1.initial)
                      |- (pass.attachment1.storeOp, pass.attachment1.final  )
      attachment2 = mkAttachmentDescription
                      |- pass.attachment2.format
                      |- (pass.attachment2.loadOp , pass.attachment2.initial)
                      |- (pass.attachment2.storeOp, pass.attachment2.final  )
      renderPassCreateInfo = mkRenderPassCreateInfo
                               |- [ attachment0, attachment1, attachment2 ]
                               |- [ pass.aa.description ]
                               |- pass.dependencies
  perform $ vkCreateRenderPass
              |- device
              |- p renderPassCreateInfo
              |- VK_NULL
  where pass = renderPipelineInfo.aaPass
