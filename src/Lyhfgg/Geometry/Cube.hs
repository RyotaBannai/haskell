module Lyhfgg.Geometry.Cube
  ( cubeVolume,
    cubeArea,
  )
where

import Lyhfgg.Geometry.Cuboid (cuboidArea)

cubeVolume :: Float -> Float
cubeVolume side = cuboidArea side side side

cubeArea :: Float -> Float
cubeArea side = cuboidArea side side side