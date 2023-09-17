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

layout(location = 0) out vec2  coordinates;
layout(location = 1) out float fill;
layout(location = 2) out float orientationBase;
layout(location = 3) out float orientationControl;

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

  coordinates = vec2( bitfieldExtract(flags, 0, 1)
                    , bitfieldExtract(flags, 1, 1)
                    );

  // Extract bit 2 from flags to determine if the vertex is part of a 'base' triangle.
  // If bit 2 is set and the vertex is part of a 'base' triangle, fill is set to
  // 1.0 to indicate that the fragment shader should fill the entire triangle.
  // If bit 2 is not set and the vertex is part of a 'control' triangle, fill is set to
  // 0.0 to indicate that the fragment shader should use the vertex as a control point
  // and only fill the area enclosed by the associated parabola.

  fill = bitfieldExtract(flags, 2, 1);

  // Extract bit 3 from flags to determine the orientation of the 'base' triangle:
  // the triangle is oriented counterclockwise if bit 3 is set, and clockwise otherwise.
  // orientationBase should evaluate to
  // 1.0 if the vertex is part of a draw call
  // where the 'base' triangle is oriented counterclockwise, and
  // 0.0 if the vertex is part of a draw call
  // where the 'base' triangle is oriented clockwise.

  orientationBase = bitfieldExtract(flags, 3, 1);

  // Extract bit 4 from flags to determine the orientation of the 'control' triangle:
  // the triangle is oriented counterclockwise if bit 4 is set, and clockwise otherwise.
  // orientationControl should evaluate to
  // 1.0 if the vertex is part of a draw call
  // where the 'control' triangle is oriented counterclockwise, and
  // 0.0 if the vertex is part of a draw call
  // where the 'control' triangle is oriented clockwise.

  orientationControl = bitfieldExtract(flags, 4, 1);
}
