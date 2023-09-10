{-# LANGUAGE BangPatterns             #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE TemplateHaskell          #-}
{-# LANGUAGE TypeApplications         #-}

module Pipeline.RenderPass where

import Vk

import Pipeline.RenderPass.Info

-- | Creates the first renderpass, with
-- | 2 subpasses, 3 attachments and
-- | a subpass dependency that the
-- | fragment shader for the second subpass
-- | must wait on the color attachment output of the first.
createRenderPass
  :: VkDevice
  -> IO VkRenderPass
createRenderPass device = do
  let subpassDescription0 = mkSubpassDescription
                              |- []
                              |- [colorReference 0, colorReference 1]
      subpassDescription1 = mkSubpassDescription
                              |- [inputReference 0, inputReference 1]
                              |- [colorReference 2]
      attachment0 = mkAttachmentDescription
                      |- VK_FORMAT_R16G16_SFLOAT
                      |- (CLEAR, UNDEFINED)
                      |- (STORE, INPUT    )
      attachment1 = mkAttachmentDescription
                      |- VK_FORMAT_R8G8B8A8_UNORM
                      |- (CLEAR, UNDEFINED)
                      |- (STORE, INPUT    )
      attachment2 = mkAttachmentDescription
                      |- VK_FORMAT_B8G8R8A8_UNORM
                      |- (CLEAR, UNDEFINED)
                      |- (STORE, INPUT    )
      subpassDependency01 = mkSubpassDependency
                              |- BY_REGION
                              |- (0, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_READ)
                              |- (1, FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ)
      subpassDependency11 = mkSubpassDependency
                              |- BY_REGION
                              |- (1, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_READ)
                              |- (1, FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ)
      renderPassCreateInfo = mkRenderPassCreateInfo
                               |- [ attachment0, attachment1, attachment2 ]
                               |- [ subpassDescription0, subpassDescription1 ]
                               |- [ subpassDependency01, subpassDependency11 ]
  perform $ vkCreateRenderPass
              |- device
              |- p renderPassCreateInfo
              |- VK_NULL

-- | Creates the second renderpass, with
-- | 1 subpass, 3 attachments and
-- | an external subpass dependency on the
-- | color attachment output of the previous renderpass.
createRenderPass1
  :: VkDevice
  -> IO VkRenderPass
createRenderPass1 device = do
  let subpassDescription0 = mkSubpassDescription
                              |- [inputReference 2]
                              |- [colorReference 0]
      attachment0 = mkAttachmentDescription
                      |- VK_FORMAT_B8G8R8A8_UNORM
                      |- (CLEAR, UNDEFINED)
                      |- (STORE, PRESENT  )
      attachment1 = mkAttachmentDescription
                      |- VK_FORMAT_R16G16_SFLOAT
                      |- (LOAD           , INPUT)
                      |- (STORE_DONT_CARE, INPUT)
      attachment2 = mkAttachmentDescription
                      |- VK_FORMAT_B8G8R8A8_UNORM
                      |- (LOAD , INPUT)
                      |- (STORE, INPUT)
      subpassDependency0 = mkSubpassDependency
                             |- GLOBAL
                             |- (EXTERNAL, COLOR_ATTACHMENT_OUTPUT, COLOR_ATTACHMENT_WRITE)
                             |- (0       , FRAGMENT_SHADER        , INPUT_ATTACHMENT_READ )
      renderPassCreateInfo = mkRenderPassCreateInfo
                               |- [ attachment0, attachment1, attachment2 ]
                               |- [ subpassDescription0 ]
                               |- [ subpassDependency0 ]
  perform $ vkCreateRenderPass
              |- device
              |- p renderPassCreateInfo
              |- VK_NULL
