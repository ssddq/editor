#version 450

// Vertex shader for renderpass 0, subpass 0.
//
// This subpass should write to the color attachment with additive blending
// to (later) calculate the total oriented winding number of the glyph contours
// around a given point or fragment.
// The vertex shader in this subpass processes glyph contour data.

layout(push_constant) uniform constants
{ uint  xWindowSize;
  uint  yWindowSize;
  uint  unitsPerEmX2;
  float ppi;
} PushConstants;

layout(binding = 0) uniform UniformBufferObject
{ mat4 model;
  mat4 view;
  mat4 proj;
} ubo;

layout(location = 0)  in int   xCoordinate;
layout(location = 1)  in int   yCoordinate;
layout(location = 2)  in uint  flags;
layout(location = 3)  in float xPos;
layout(location = 4)  in float yPos;
layout(location = 5)  in float xx;
layout(location = 6)  in float xy;
layout(location = 7)  in float yx;
layout(location = 8)  in float yy;
layout(location = 9)  in float fontSize;
layout(location = 10) in float red;
layout(location = 11) in float green;
layout(location = 12) in float blue;
layout(location = 13) in float alpha;

void main()
{
  gl_Position = vec4( xCoordinate
                    , -yCoordinate
                    , 0
                    , 1
                    );
}
