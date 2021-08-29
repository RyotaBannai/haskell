module Lyah.Geometry.Cube
  ( cubeVolume,
    cubeArea,
  )
where

import Lyah.Geometry.Cuboid (cuboidArea)

cubeVolume :: Float -> Float
cubeVolume side = cuboidArea side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side