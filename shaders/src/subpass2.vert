#version 450

// Vertex shader for renderpass 1, subpass 0.
//
// This subpass should antialias the result of the previous renderpass
// by averaging in the fragment shader. Accessing adjacent fragment data
// introduces a synchronization point which requires that occur in a new renderpass.
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

layout(location = 0) out vec2 vertex;
layout(location = 1) out uint xWindowSize;
layout(location = 2) out uint yWindowSize;

void main() {
  float x =  xCoordinate;
  float y =  yCoordinate;
  xWindowSize = PushConstants.xWindowSize;
  yWindowSize = PushConstants.yWindowSize;
  vertex  = vec2(  (x + 1) * 0.5
                ,  (y + 1) * 0.5
                ) ; 
  gl_Position = vec4(x,y,0,1);
}
