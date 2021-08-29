module Lyah.Geometry.Sphere
  ( sphereVolume,
    sphereArea,
  )
where

sphereVolume :: Float -> Float
sphereVolume rad = (4.0 / 3.0) * pi * (rad ^ 3)

sphereArea :: Float -> Float
sphereArea rad = 4 * pi * (rad ^ 2)