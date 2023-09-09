#version 450

layout(binding  = 1) uniform sampler2D inTexture;

layout(location = 0) in  vec2 vert;
layout(location = 1) flat in uint xWindowSize;
layout(location = 2) flat in uint yWindowSize;

layout(location = 0) out vec4 outColor;


void main() {
 vec2 sampleScale = vec2(1.0, 1.0) - vec2(float(xWindowSize), float(yWindowSize))/textureSize(inTexture, 0);
 vec2 x = vec2(1.0,0.0) * sampleScale;
 vec2 y = vec2(0.0,1.0) * sampleScale;
 vec2 pos = vert * textureSize(inTexture,0);
 float avgColor = 1.0/6.0 * ( textureLod(inTexture, pos - 2.5 * x + 1.5 * y, 0)
                            + textureLod(inTexture, pos - 1.5 * x - 1.5 * y, 0)
                            + textureLod(inTexture, pos - 0.5 * x + 2.5 * y, 0)
                            + textureLod(inTexture, pos + 0.5 * x - 0.5 * y, 0)
                            + textureLod(inTexture, pos + 1.5 * x + 0.5 * y, 0)
                            + textureLod(inTexture, pos + 2.5 * x - 0.5 * y, 0)
                            ).r;
// avgColor = textureLod(inTexture, pos, 0).r;
// if ( avgColor > 5.0/6.0 )
// {
//   avgColor = 1.0;
// }
// else if ( avgColor < 1.0/6.0 )
// {
//   avgColor = 0.0;
// }
 outColor = vec4(avgColor,avgColor,avgColor,1.0);
}
