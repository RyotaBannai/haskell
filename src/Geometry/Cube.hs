module Geometry.Cube
  ( cubeVolume,
    cubeArea,
  )
where

import Geometry.Cuboid (cuboidArea)

cubeVolume :: Float -> Float
cubeVolume side = cuboidArea side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side