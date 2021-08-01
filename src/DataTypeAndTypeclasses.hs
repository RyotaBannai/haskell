module DataTypeAndTypeclasses (Point (..), Shape (..), surface, nudge, baseCircle, baseRect) where

-- export all Type contructors wiht `..`
-- We could also opt not to export any value constructors for Shape by just writing Shape in the export statement.

data Point = Point Float Float deriving (Show)

-- Value constructors(Circle and Rectangle) are actually functions that ultimately return a value of a data type(Shape).
-- Circle and Rectangle are not Type, but Shape is.
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- Pattern match against Value contructors!
surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

{-
surface $ Circle (Point 0 0) 10        -- 314.15927
surface $ Rectangle (Point 0 0) (Point 100 100) -- 10000.0
-}

-- move Shape to (+-x,+-y)
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

{-
Rectangle (Point 10.0 10.0) (Point 110.0 110.0)
-}

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect x y = Rectangle (Point 0 0) (Point x y)