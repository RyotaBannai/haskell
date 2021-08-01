module DataTypeAndTypeclasses
  ( Point (..),
    Shape (..),
    surface,
    nudge,
    baseCircle,
    baseRect,
    Vector (..),
    vplus,
    vectMult,
    scalarMult,
    IntMap,
    LockerState (..),
    LockerMap,
    Code,
    lockerLookup,
    lockers,
  )
where

import qualified Data.Map
import qualified Data.Map as Map

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

-- Type parameter `a` before "="
data Vector a = Vector a a a deriving (Show)

{-
These functions can operate on types of Vector Int, Vector Integer, Vector Float, whatever, as long as the a from Vector a is from the Num typeclass.
-}
vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i * m) (i * j) (i * k)

scalarMult :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `scalarMult` (Vector l m n) = Vector (i * l) (j * m) (k * n)

-- Type synonyms
-- Partially applied type

type IntMap = Map.Map Int

-- Locker example
data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

-- Possible failure is either Takne or doesn't exist.
{-
case clause can be considered as Scala's pattern match:
Scala: x match { ... }
Haskell: case x of ...
-}
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " don't exist."
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left $ "Locker " ++ show lockerNumber ++ " is already taken."

lockers :: LockerMap
lockers =
  Map.fromList
    [ (100, (Taken, "ZD39I")),
      (101, (Free, "JAH3I")),
      (103, (Free, "IQSA9")),
      (105, (Free, "QOTSA")),
      (109, (Taken, "893JJ")),
      (110, (Taken, "99292"))
    ]