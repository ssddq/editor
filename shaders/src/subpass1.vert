#version 450

// Vertex shader for renderpass 0, subpass 1.
//
// This subpass should evaluate the winding number written to the color attachment
// in the previous subpass, and use that to determine whether a fragment is colored
// or not.
// The vertex shader in this subpass should use no input data,
// and simply perform a full-screen draw.

layout(push_constant) uniform constants
{ uint xWindowSize;
  uint yWindowSize;
} PushConstants;

layout(binding = 0) uniform UniformBufferObject
{ mat4 model;
  mat4 view;
  mat4 proj;
} ubo;

layout(location = 0) in int  xCoordinate;
layout(location = 1) in int  yCoordinate;
layout(location = 2) in uint flags;

void main()
{
  gl_Position = vec4(  xCoordinate
                    , -yCoordinate
                    , 0
                    , 1
                    ) ;
}
