import Distribution.Simple
import System.Process

main = do
  callCommand "glslc src/subpass0.vert -o data/vert0.spv"
  callCommand "glslc src/subpass0.frag -o data/frag0.spv"
  callCommand "glslc src/subpass1.vert -o data/vert1.spv"
  callCommand "glslc src/subpass1.frag -o data/frag1.spv"
  callCommand "glslc src/subpass2.vert -o data/vert2.spv"
  callCommand "glslc src/subpass2.frag -o data/frag2.spv"
  callCommand "glslc src/subpass3.vert -o data/vert3.spv"
  callCommand "glslc src/subpass3.frag -o data/frag3.spv"
  defaultMain
