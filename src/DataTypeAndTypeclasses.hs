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
    YesNoJS,
    yesno,
    yesnoIf,
    Tofu,
    tofu,
    Frank (..),
  )
where

import qualified Data.Map
import qualified Data.Map as Map
import Data.Sequence (Seq (Empty))
import GHC.Base (join)

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

-- Javascript-ish bool inference
class YesNoJS a where
  yesno :: a -> Bool

instance YesNoJS Int where
  yesno 0 = False
  yesno _ = True

instance YesNoJS Integer where
  yesno 0 = False
  yesno _ = True

instance YesNoJS Float where
  yesno 0.0 = False
  yesno _ = True

instance YesNoJS Double where
  yesno 0.0 = False
  yesno _ = True

instance YesNoJS [a] where
  yesno [] = False
  yesno _ = True

instance YesNoJS Bool where
  yesno = id -- identity function: takes a paramter and return the same parameter without applying any operations.

instance YesNoJS (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNoJS (Tree a) where
  yesno EmptyTree = False
  yesno _ = False

instance YesNoJS TrafficLight where
  yesno Red = False
  yesno _ = True

yesnoIf :: YesNoJS a => a -> p -> p -> p
yesnoIf yesnoVal yesnoResult noResult = if yesno yesnoVal then yesnoResult else noResult

{-
yesnoIf [] "YES" "NO" -- "NO"
-}

{-
Functor impl:
class Functor f where
  fmap :: (a -> b) -> f a -> f b

map's type signature:
map :: (a -> b) -> [a] -> [b].
:=  In fact, `map` is just a `fmap` that works only on `lists`. Here's how the list is an instance of the Functor typeclass:

instance Functor [] where
  fmap = map

Notice how we didn't write `instance Functor [a] where`, because from `fmap :: (a -> b) -> f a -> f b`, we see that the `f` has to be a type constructor that takes one type. `[a]` is already `a concrete type` (of a list with any type inside it), while `[]` is `a type constructor` that takes one type and can produce types such as `[Int]`, `[String]` or `even [[String]]`.
-}

{-
Definition of Functor for Maybe:
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing
-}
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.") -- Just "Something serious. HEY GUYS IM INSIDE THE JUST"
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing -- Nothing

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

{-
fmap (*2) (foldr treeInsert EmptyTree [5,2,6,7,3,3,1]) -- Node 2 EmptyTree (Node 6 (Node 4 EmptyTree EmptyTree) (Node 14 (Node 12 (Node 10 EmptyTree EmptyTree) EmptyTree) EmptyTree))
-}

-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = Left x

class Tofu t where
  tofu :: j a -> t a j -- the type of a value tofu takes as tis parameter, `j a` has to have a kind of *, we assume * for `a` and so we can infer that `j` has to have a kind of `* -> *`

-- decreases laziness
newtype Frank a b = Frank {frankField :: b a} deriving (Show)

-- :t Frank {frankField = Just "Haha"} -- Frank {frankField = Just "Haha"} :: Frank [Char] Maybe

{-
Functor impl:
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}
instance Tofu Frank where
  tofu x = Frank x

-- tofu (Just 'a') :: Frank Char Maybe -- Frank {frankField = Just 'a'}
data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f Barry {yabba = x, dabba = y} = Barry {yabba = f x, dabba = y}