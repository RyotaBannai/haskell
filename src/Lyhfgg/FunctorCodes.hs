module Lyhfgg.FunctorCodes where

import Control.Applicative
import Data.Char
import qualified Data.Foldable as F
import Data.List
import Data.Monoid
import Lyhfgg.DataTypeAndTypeclasses (Tree (EmptyTree, Node), treeInsert)

-- reverse text
simple :: IO ()
simple = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- Like we can apply a function to something that's inside a Maybe box, we can apply a function to what's inside an `IO box`, only it has to go out into the real world to get something.

-- using fmap
-- fmap :: (a -> b) -> IO a -> IO b
simple' :: IO ()
simple' = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- anc >> C-B-A
moreComplex :: IO ()
moreComplex = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line

{-
Definition of Applicative:
class (Functor f) => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

-- if we want to mkae a type contructor part of the Applicative typeclass,
  it has to be in Functor first.
-- pure := takes a value of any type and return an applicative functor with that value inside it.
-- (<*>)
  := takes a `functor that has a function in it` and `another functor` and sort of extracts that function `from the first functor` and then `maps it over the second one`
  := <*> is left-associative, so `pure (+) <*> Just 3 <*> Just 5` is the same as `(pure (+) <*> Just 3) <*> Just 5`
-}

-- Just concatenation of two strings in Applicative context
myIOAction :: IO String
myIOAction = (++) <$> getLine <*> getLine

-- Functions as Applicative functor
-- applyToAllFunctions 5 >> 508
applyToAllFunctions :: Integer -> Integer
applyToAllFunctions = (+) <$> (+ 3) <*> (* 100)

-- applyToAllFunctions' 5 >> [8.0, 10.0, 2.5]
applyToAllFunctions' :: Double -> [Double]
applyToAllFunctions' = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2)

sampleZipList :: [Integer]
sampleZipList = getZipList $ (*) <$> ZipList [1, 2, 3] <*> ZipList [1, 5, 10]

-- [('d', 'c', 'r'), ('o', 'a', 'a'), ('g', 't', 't')]
sampleZipList' :: [(Char, Char, Char)]
sampleZipList' = getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"

-- liftA2 := takes two functors and a function, and applys the function on the two functors, and return the result of functor.
-- Takes `a list of Functors` and return `a Functor of a list`
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

-- Or
-- (\x -> (<*>) ((:) <$> x)) :: Applicative f => f a -> f [a] -> f [a]
-- <*> :: forall (f :: * -> *) a b. Applicative f => f (a -> b) -> f a -> f b
-- lambda 部分で <*> の第一引数 f(a->b) を構築した <*> を返却し、accum をその第二引数 f a として適用
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (\x -> (<*>) ((:) <$> x)) (pure [])

{-
sequenceA [[1,2],[3,4]]
>> (:) <$> [1,2] <*> sequenceA [[3,4]]
>> (:) <$> [1,2] <*> [[3],[4]]  -- [3:[],4:[]] with an edge case pure
>> [1:[3],1:[4],2:[3],3:[4]]
>> [[1,3],[1,4],[2,3],[2,4]]
-}

-- Easier version to understand the codes
-- sequenceA'' :: Applicative f => [f a] -> f [a]
-- sequenceA'' [] = pure []
-- sequenceA'' (x : xs) = (:) <$> x <*> sequenceA'' xs -- 右結合で計算した結果, f [a] になり最終的に `(:) <$> x <*> f [a]` の計算に適用される

{-
‘Prelude.sequenceA’ has the same method
(and originally defined in ‘Data.Traversable’
-}

-- use `newtype` to get the ability to apply function on only specific component of tuple:
newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- getPair $ fmap (*100) (Pair (2,3)) >> (200, 3)
-- getPair $ fmap reverse  (Pair ("los angeles",3)) >> ("selegna sol",3)

-- Monoids:
re :: Integer
re = getProduct . mconcat $ map Product [3, 4, 5]

-- instead of writing:
lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  let a = length x `compare` length y
      b = x `compare` y
   in if a == EQ then b else a

-- we can do this since we understand how Ordering monoid is implemented:
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y =
  (length x `compare` length y) -- we must add more important criterion first.
  -- `<>` (vowels x `compare` vowels x) -- we can add criteron as many as we want.
    <> (x `compare` y) -- `<>` is an alias for `mappend`

-- get First result >> Just 9
fre :: Maybe Integer
fre = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- gete Last result >> Just "two"
lre :: Maybe [Char]
lre = getLast $ Last (Just "one") <> Last (Just "two")

-- Foldable for Tree
-- We have a Foldable instance for our tree type, we get `foldr` and `foldl` for free!
-- foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
instance F.Foldable Tree where
  foldMap f EmptyTree = mempty -- if a tree is empty, the monoid value it becomes is `mempty`
  foldMap f (Node x l r) =
    F.foldMap f l
      <> f x
      <> F.foldMap f r

treeValues :: [Integer]
treeValues = [5, 4, 2, 6, 3, 7, 8, 1]

testTree :: Tree Integer
testTree = foldr treeInsert EmptyTree treeValues

-- Should equals to `sum treeValues` >> 36
treeValsSum :: Integer
treeValsSum = F.foldl (+) 0 testTree -- F.foldl1 (+) testTree

-- Check if we want to know if any number in our tree is equal to 3, we can do this:
-- `foldMap` applies this function to every element in our tree and then reduces the resulting `monoids` into a single `monoid` with `mappend`
checkTree :: Bool
checkTree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree -- `Any`, `All` from `Data.Monoid`

getBackTreeValues :: [Integer]
getBackTreeValues = F.foldMap (: []) testTree -- (: []) := (\x -> [x])
