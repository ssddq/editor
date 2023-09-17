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
layout(location = 3) in float xPos;
layout(location = 4) in float yPos;
layout(location = 5) in float xx;
layout(location = 6) in float xy;
layout(location = 7) in float yx;
layout(location = 8) in float yy;
layout(location = 9)  in float fontSize;
layout(location = 10) in float red;
layout(location = 11) in float green;
layout(location = 12) in float blue;
layout(location = 13) in float alpha;

layout(location = 0) out vec4 color;

void main()
{
  float xWindowSize = float(PushConstants.xWindowSize);
  float yWindowSize = float(PushConstants.yWindowSize);
  float xP =  (float(xCoordinate) ) / xWindowSize;
  float yP = -(float(yCoordinate) ) / yWindowSize;

  gl_Position = vec4( xx*xP + 2.0 * xPos / xWindowSize - 1.0
                    , yy*yP + 2.0 * yPos / yWindowSize - 1.0
                    , 0
                    , 1
                    );

  color = vec4(red, green, blue, alpha);
}
