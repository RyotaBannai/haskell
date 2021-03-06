module Lyah.FunctorCodes where

import Control.Applicative
  ( Applicative (liftA2),
    ZipList (ZipList, getZipList),
  )
import Control.Monad (ap, liftM2)
import Data.Char (toUpper)
import Data.Foldable (Foldable (toList))
import qualified Data.Foldable as F
import Data.Function (on)
import Data.List (intersperse, sortBy)
import Data.Maybe (fromMaybe)
import Data.Monoid
  ( Any (Any, getAny),
    First (First, getFirst),
    Last (Last, getLast),
    Product (Product, getProduct),
  )
import Lyah.DataTypeAndTypeclasses (Tree (EmptyTree, Node), treeInsert)

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
applyToAllFunctions :: Int -> Int
applyToAllFunctions = (+) <$> (+ 3) <*> (* 100)

-- applyToAllFunctions' 5 >> [8.0, 10.0, 2.5]
applyToAllFunctions' :: Double -> [Double]
applyToAllFunctions' = (\x y z -> [x, y, z]) <$> (+ 3) <*> (* 2) <*> (/ 2)

sampleZipList :: [Int]
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
-- lambda ????????? <*> ??????????????? f(a->b) ??????????????? <*> ???????????????accum ????????????????????? f a ???????????????
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
-- sequenceA'' (x : xs) = (:) <$> x <*> sequenceA'' xs -- ??????????????????????????????, f [a] ????????????????????? `(:) <$> x <*> f [a]` ???????????????????????????

{-
???Prelude.sequenceA??? has the same method
(and originally defined in ???Data.Traversable???
-}

-- use `newtype` to get the ability to apply function on only specific component of tuple:
newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

-- getPair $ fmap (*100) (Pair (2,3)) >> (200, 3)
-- getPair $ fmap reverse  (Pair ("los angeles",3)) >> ("selegna sol",3)

-- *** Monoids ***

re :: Int
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
fre :: Maybe Int
fre = getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]

-- get Last result >> Just "two"
lre :: Maybe [Char]
lre = getLast $ Last (Just "one") <> Last (Just "two")

-- *** Foldable ***

-- Foldable for Tree
-- We have a Foldable instance for our tree type, we get `foldr` and `foldl` for free!
-- foldMap' :: (Foldable f, Monoid m) => (a -> m) -> f a -> m
instance F.Foldable Tree where
  foldMap f EmptyTree = mempty -- if a tree is empty, the monoid value it becomes is `mempty`
  foldMap f (Node x l r) =
    F.foldMap f l
      <> f x
      <> F.foldMap f r

treeValues :: [Int]
treeValues = [5, 4, 2, 6, 3, 7, 8, 1]

treeValues' :: [Bool]
treeValues' = [True, False, True]

treeValues'' :: [Maybe Int]
treeValues'' = [Just 1, Just 2, Just 3]

testTree :: Tree Int
testTree = foldr treeInsert EmptyTree treeValues

testTree' :: Tree Bool
testTree' = foldr treeInsert EmptyTree treeValues'

{-
Prelude.sequenceA := t (f a) -> f (t a)
testTree'' := Node (Just 3) (Node (Just 2) (Node (Just 1) EmptyTree EmptyTree) EmptyTree) EmptyTree

?? Prelude.sequenceA testTree''
Just (Node 3 (Node 2 (Node 1 EmptyTree EmptyTree) EmptyTree) EmptyTree)
-}
testTree'' :: Tree (Maybe Int)
testTree'' = foldr treeInsert EmptyTree treeValues''

-- ?? sum treeValues # 36
-- treeValsSum = 36
treeValsSum :: Int
treeValsSum = F.foldl (+) 0 testTree -- F.foldl1 (+) testTree

-- treeValsSum' = Just 6
-- If any item of treeValues'' is Nothing, then result is Nothing.
treeValsSum' :: Maybe Int
treeValsSum' =
  F.foldl1
    ( \m n -> do
        m' <- m
        n' <- n
        return (m' + n')
    )
    testTree''

-- conv = 6
conv :: Int
conv = case treeValsSum' of
  Nothing -> 0
  Just i -> i

-- conv' = 6
conv' :: Int
conv' = fromMaybe 0 treeValsSum'

-- Check if we want to know if any number in our tree is equal to 3, we can do this:
-- `foldMap` applies this function to every element in our tree and then reduces the resulting `monoids` into a single `monoid` with `mappend`
checkTree :: Bool
checkTree = getAny $ F.foldMap (\x -> Any $ x == 3) testTree -- `Any`, `All` from `Data.Monoid`

getBackTreeValues :: [Int]
getBackTreeValues = F.foldMap (: []) testTree -- (: []) := (\x -> [x])

getBackTreeValues' :: [Int]
getBackTreeValues' = toList testTree

-- ?? average testTree # 4
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

-- ?? all even testTree  # False
-- ?? and testTree'      # False

-- *** Traversable ***

-- ?? traverse dec [1,2,3] # Just [0,1,2]
-- ?? traverse dec [1,2,0] # Nothing
dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n -1) else Nothing

-- ?? traverse dec testTree # Just (...)
-- ?? traverse dec testTree # Nothing when `treeValues` with 0
-- class (Functor t, Foldable t) => Traversable t where
instance Traversable Tree where
  -- traverse :: Applicative f => (a -> t b) -> Tree a -> f (Tree b)
  traverse _ EmptyTree = pure EmptyTree
  traverse g (Node x l r) =
    Node <$> g x
      <*> traverse g l
      <*> traverse g r

-- ?? mapM_ print testTree

re1 :: Tree Int
re1 = fmap (+ 1) testTree

re2 :: Maybe (Tree (Maybe Int))
re2 = mapM Just testTree''

-- `re3 = Nothing` when any of `Node value is Nothing`.
re3 :: Maybe (Tree Int)
re3 = mapM id testTree'' -- sequence testTree''

-- Just (Node 4 (Node 3 (Node 2 EmptyTree EmptyTree) EmptyTree) EmptyTree)
re4 :: Maybe (Tree Int)
re4 =
  traverse
    ( \m -> do
        x <- m
        return (x + 1)
    )
    testTree''

maybeAddOne :: Int -> Maybe Int
maybeAddOne x = Just (x + 1)

re4' :: Maybe (Tree Int)
re4' = mapM (>>= maybeAddOne) testTree'' -- (\m -> m >>= maybeAddOne)

re4'' :: Maybe (Tree Int)
re4'' = mapM (\a -> (+ 1) <$> a) testTree''

re4''' :: Maybe (Tree Int)
re4''' = mapM (fmap (+ 1)) testTree''

re4'''' :: Maybe (Tree Int)
re4'''' = mapM (liftM2 (+) (Just 1)) testTree''

re5 :: Tree (Maybe Int)
re5 = fmap (\a -> (+ 1) <$> a) testTree''

-- Container ????????????????????????????????? fmap ??????????????????:
-- outer  _ :: (Maybe Int -> Maybe Int) -> Tree (Maybe Int) -> Tree (Maybe Int)
-- innter _ :: (Int -> Int) -> Maybe Int -> Maybe Int
re5' :: Tree (Maybe Int)
re5' = fmap (fmap (+ 1)) testTree''

re5'' :: Tree (Maybe Int)
re5'' = (fmap . fmap) (+ 1) testTree''

re5''' :: Tree (Maybe Int)
re5''' = fmap (>>= maybeAddOne) testTree''

-- re6 = [[1,1],[1,2]] -- non-deterministic ops
re6 :: [[Int]]
re6 = (:) <$> [1] <*> [[1], [2]]

-- re7 = Just [1,1] -- cons
re7 :: Maybe [Integer]
re7 = (:) <$> Just 1 <*> Just [1]

-- *** S' Combinator (with liftM2 or starling', phoneix from Data.Aviary.Birds)***

findGCD :: (Integral r, Foldable t) => t r -> r
findGCD = liftM2 gcd minimum maximum

findGCD' :: (Integral r, Foldable t) => t r -> r
findGCD' = liftA2 gcd minimum maximum

findGCD'' :: (Integral r, Foldable t) => t r -> r
findGCD'' = return gcd `ap` minimum `ap` maximum

findGCD''' :: (Integral r, Foldable t) => t r -> r
findGCD''' = gcd <$> minimum <*> maximum

{-
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
compare :: (b -> b -> c)
fst :: (a -> b)
a :: (f, s)
c :: Ordering
-}
sortList :: Ord a => [(a, b)] -> [(a, b)]
sortList = sortBy (compare `on` fst)

{-
($) application operator :
f $ g $ h x  =  f (g (h x))
It is also useful in higher-order situations, such as `map ($ 0) xs`, or `zipWith ($) fs xs`.
-}