#version 450

// Fragment shader for renderpass 0, subpass 0.
//
// This subpass should write to the color attachment with additive blending
// to (later) calculate the total oriented winding number of the glyph contours
// around a given point or fragment.

layout(location = 0) in vec2  coordinates;
layout(location = 1) in float fill;
layout(location = 2) in float orientationBase;
layout(location = 3) in float orientationControl;

layout(location = 0) out vec2 outWind;


void main()
{
  // This fragment shader writes to the color attachment with additive blending,
  // to record a winding number around the fragment.
  // Curves traced counterclockwise add to the x-coordinate of the fragment,
  // while curves traced clockwise add to the y-coordinate.
  // The sum of the (counterclockwise) winding numbers of all curves around
  // a given fragment can be computed in the following subpass as
  // 256*(outColor.x - outColor.y).

  // For 'base' triangles,
  // signBase = 1 indicates a counterclockwise orientation, and
  // signBase = 0 indicates a clockwise orientation.

  float signBase = sign(orientationBase);

  vec2  fillBase = vec2( 1.0 * signBase
                       , 1.0 * (1.0 - signBase)
                       ) ;

  // For 'control' triangles,
  // the (u,v)-coordinates are the normalized coordinates
  // of the first and third points in the draw call
  // measured from the control point.
  // underQuadratic = 1 indicates that the fragment lies underneath
  // the quadratic defined by the control point.
  // underQuadratic = -1 indicates that the fragment lies above
  // the quadratic defined by the control point.

  float u = coordinates.x;
  float v = coordinates.y;

//  float underQuadratic = -sign( pow(v - u - 1.0, 2.0)
//                              - 4.0*u
//                              );

  float underQuadratic = -sign(float(dot(vec2(v-u-1.0, 4.0*u), vec2(v-u-1.0, -1.0))));

  // For 'control' triangles,
  // signControl = 1 indicates a counterclockwise orientation, and
  // signControl = 0 indicates a clockwise orientation.
  // However, the contributions are interchanged since
  // for 'control' triangles the area in the interior of the curve
  // lies on the opposite side of the contour (straight-line) edge segment.

  float signControl = sign(orientationControl);

  vec2 fillControl  = max ( underQuadratic, 0 )
                    * vec2( 1.0 * (1.0 - signControl)
                          , 1.0 * signControl
                          ) ;

  // Use fill as an indicator to either
  // use fillControl to draw the fragment as a portion of a 'control' triangle, or
  // use fillBase to draw the fragment as a portion of a 'base' triangle.

  float drawFull = sign(fill);

  outWind =        drawFull  * fillBase
          + (1.0 - drawFull) * fillControl;
}


