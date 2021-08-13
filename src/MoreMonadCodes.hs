module MoreMonadCodes where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Data.Semigroup
import GHC.Base
import System.Random

isBigBang :: (Ord a, Num a) => a -> (Bool, String)
isBigBang x = (x > 9, "Compared gang size to 9.")

get16UsdIfTrue :: (Ord a1, Num a1, Num a2) => a1 -> (Bool, [a2])
get16UsdIfTrue x = let result = x > 5 in (result, if result then [1, 5, 10] else [])

-- takes (a, log) and joined together with the log the value that results from the funciton (f x)
-- (3, "Smallish gang.") `applyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- a := a `value`, and String is a `log`.
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- Now Works on any `Monoid`
-- (3, "Smallish gang.") `betterApplyLog` isBigBang # (False, "Smallish gang.Compared gang size to 9.")
-- (3, []) `betterApplyLog` get16UsdIfTrue # (False,[1,5,10])
betterApplyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
betterApplyLog (x, log) f = let (y, newLog) = f x in (y, log <> newLog)

type Food = String

type Price = Sum Int

-- let (_, result) = ("beans", Sum 10) `betterApplyLog` addDrink in getSum result # 35
addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

{-
-- Just wrap of a normal tuple.
newtype Writer w a = Writer {runWriter :: (a, w)}
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  (Writer (x, v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

-- Few examples:
runWriter (return 3 :: Writer String Int)        # (3,"")
runWriter (return 3 :: Writer (Sum Int) Int)     # (3,Sum {getSum = 0})
runWriter (return 3 :: Writer (Product Int) Int) # (3,Product {getProduct = 1})
-}

{-
Reader and Writer:
- https://blog.ssanj.net/posts/2018-01-12-stacking-the-readert-writert-monad-transformer-stack-in-haskell.html
- https://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
-}
-- runWriter multWithLog to get tuple inside
-- runWriterT multWithLog to get `Identity`
logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  -- tell := takes a monoid value, like that list and creates a `Writer` value that presents the dummy value () as its result but has our desired monoid value attached. (attach an extra log message to each logs)
  -- if we define `[String]`, `Writer` stacks logs as `[String]`, but if we define just `String`, `Writer` concats strings as it should be.
  return (a * b)

gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | otherwise = gcd' b (a `mod` b)

-- Debug final logs by `mapM_ putStrLn $ snd $ runWriter (gcd'WithWriter 8 3)`
{-
Append a log on the right `a ++ (b ++ (c ++ (d ++ (e ++ f))))`
Lists are a data structure that's constructed from left to right, and this is efficient because we first fully construct the left part of a list and only then `add a longer list on the right`.
-}
gcd'WithWriter :: Int -> Int -> Writer [String] Int
gcd'WithWriter a b
  | b == 0 =
    writer (a, ["Finished with " ++ show a])
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
    gcd'WithWriter b (a `mod` b)

-- difference list
-- fromDiffList (toDiffList [1..4] <> toDiffList [1..3])
newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  (DiffList f) <> (DiffList g) = DiffList (f . g) -- \xs -> f (g x)

instance Monoid (DiffList a) where
  mempty = DiffList ([] ++) -- \xs -> [] ++ xs
  (DiffList f) `mappend` (DiffList g) = DiffList (f . g)

-- mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34
gcdReverse :: Int -> Int -> Writer (DiffList String) Int
gcdReverse a b
  | b == 0 = do
    tell (toDiffList ["Finished with " ++ show a])
    return a
  | otherwise = do
    result <- gcdReverse b (a `mod` b)
    tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
    return result

-- Comparing performance

-- Run by `mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 50000`
finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x -1)
  tell (toDiffList [show x])

-- too slow impl with left associative of list
-- Run by `mapM_ putStrLn . snd . runWriter $ finalCountDown' 500000`
finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x -1)
  tell [show x]

-- Run by `mapM_ putStrLn . snd . runWriter $ finalCountDown'' 500000`
-- The log order is reversed tho (print desc ord),
-- it's faster than finalCountDown', but slower than finalCountDown
finalCountDown'' :: Int -> Writer [String] ()
finalCountDown'' 0 = do
  tell ["0"]
finalCountDown'' x = do
  tell [show x]
  finalCountDown'' (x -1)

-- (+) <$> (*2) <*> (+10)
-- This is the same thing as the `applicative` expression above (with `Applicative functor`), only now it relies on functions being `monads`.
-- A `do expression` always results in `a monadic value` and this one is no different.
-- `The result of this monadic value is a function` a and b are the result of that each functions are applied to incoming number.
addStuff :: Int -> Int
addStuff = do
  a <- (* 2)
  b <- (+ 10)
  return (a + b)

{-
newtype State s a = State { runState :: s -> (a,s) }

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                  in  g newState
-}
type Stack = [Int]

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

-- Think about what is and how to set State value. In this case, RandomGen g is our state, and value is Random.
-- runState randomSt (mkStdGen 33)
randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

-- runState threeConins (mkStdGen 33)
threeConins :: State StdGen (Bool, Bool, Bool)
threeConins = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)