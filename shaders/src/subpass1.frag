#version 450

// Fragment shader for renderpass 0, subpass 1
//
// This subpass should evaluate the winding number written to the color attachment
// in the previous subpass, and use that to determine whether a fragment is colored
// or not.

layout(input_attachment_index = 0, set = 0, binding = 1) uniform subpassInput inWind;

layout(location = 0) in vec4 color;

layout(location = 0) out vec4 outColor;


void main()
{
  vec2 wind  = subpassLoad(inWind).rg;

  // Evaluate the winding number using the components
  // of the color attachment output of the previous subpass (renderpass 1, subpass 0).
  // The x-coordinate counts the number of times the curve was moving
  // counterclockwise around the fragment being drawn, while
  // the y-coordinate counts the number of times the curve was moving
  // clockwise around the fragment being drawn.

  int windingNumber = int(round(wind.x))
                    - int(round(wind.y));

  // Only fragments with a positive windingNumber are ultimately shaded.

  outColor = abs(sign(windingNumber)) * color;
}
