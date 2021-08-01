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
    List' (..),
    List'' (..),
    (.++),
    Tree (..),
    singletonTree,
    treeInsert,
    treeElem,
    TrafficLight (..),
  )
where

import qualified Data.Map
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty), singleton)

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

-- Resursive data structures
-- Cons' `:` constructor is right associative.
data List' a = Empty' | Cons' a (List' a) deriving (Show, Read, Eq, Ord)

-- data List' a = Empty | Cons' {listHead :: a, listTail :: List a} deriving (Show, Read, Eq, Ord)

-- Fixity declaration
infixr 5 :-:

data List'' a = Empty'' | a :-: (List'' a) deriving (Show, Read, Eq, Ord)

{-
3 :-: 4 :-: Empty''
-}

-- Stealing List ++ definition.
infixr 5 .++

(.++) :: List'' a -> List'' a -> List'' a
Empty'' .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys) -- we can match here because we define like this in data constructor above

{-
a = (100 :-: Empty'')
b = (3 :-: 4 :-: 5 :-: Empty'')
a .++ b -- 100 :-: (3 :-: (4 :-: (5 :-: Empty'')))
-}

-- Implement Binary Tree(not balanced binary tree)
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- singleton tree (a tree with just one node)
singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a left right)
  | x == a = Node x left right -- If the same value Node exists, then return the same Tree
  | x < a = Node a (treeInsert x left) right
  | x > a = Node a left (treeInsert x right)

{-
foldr treeInsert EmptyTree [5,4,2,6,6,6,3,7,9,1,3]
treeInsert 3 $ treeInsert 4 $ treeInsert 10 $ treeInsert 6 $ treeInsert 5 EmptyTree

Node 5
(Node 4
  (Node 3
    EmptyTree
    EmptyTree)
  EmptyTree) -- i.g. 4.5
(Node 6
  EmptyTree -- i.g. 5.5
  (Node 10
    EmptyTree
    EmptyTree))
-}

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

{-
treeElem 6 myTree -- True
-}

data TrafficLight = Red | Yellow | Green -- Notice we didn't derive any class instances for it, such as `Eq`! because we define it on our own.

-- Using `instance` keyword
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"

{-
Red `elem` [Red, Yellow, Green] -- True
[Red, Yellow, Green] -- [Red light,Yellow light,Green light]
-}