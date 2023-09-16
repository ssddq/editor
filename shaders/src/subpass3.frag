#version 450

// Fragment shader for renderpass 0, subpass 1
//
// This subpass should evaluate the winding number written to the color attachment
// in the previous subpass, and use that to determine whether a fragment is colored
// or not.

layout(location = 0) out vec4 outColor;
layout(location = 1) out vec4 outWind;


void main()
{
  outColor = vec4(0, 0, 0, 0);
  outWind  = vec4(0, 0, 0, 0);
}
