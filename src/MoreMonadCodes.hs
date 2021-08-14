module MoreMonadCodes where

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import qualified Control.Monad.Fail as Fail
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Monoid
import Data.Ratio
import Data.Semigroup
import GHC.Base
import KnightsQueat (KnightPos, moveKnight)
import MonadCodes (Birds, Pole (..), landLeft, landRight)
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

-- rework with Either

-- reworking
-- (0,0) -: landLeft' 10 # Nothing
-- return (0,0) >>= landLeft' 1 >>= landRight' 1 # Just (1,1)
-- Nothing >>= landLeft' 2 # `fail` will be propaged.
landLeft' :: Birds -> Pole -> Either String Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Right $ landLeft n (left, right)
  | otherwise = leftMsg (left, right)

landRight' :: Birds -> Pole -> Either String Pole
landRight' n (left, right)
  | abs (left - (right + n)) < 4 = Right $ landRight n (left, right)
  | otherwise = leftMsg (left, right)

leftMsg :: Pole -> Either String Pole
leftMsg (left, right) = Left $ "Pierre fell off the rope!! birds on the left: " ++ show left ++ ", birds on the right: " ++ show right

banana :: Pole -> Either String Pole
banana = leftMsg

routine :: Either String Pole
routine = do
  let start = (0, 0)
  first <- landLeft' 2 start
  second <- landRight' 2 first
  third <- landLeft' 1 second
  banana third

-- liftM := fmap for Monads
re1 :: (Bool, [Char])
re1 = runWriter $ liftM not $ writer (True, "chickpeas")

re2 :: (Int, Stack)
re2 = runState (liftM (+ 100) pop) [1 .. 4]

-- liftM2 := binary operator
-- Just [4,4]
re3 :: Maybe [Integer]
re3 = Prelude.foldr (liftM2 (:)) (return []) [Just 4, Just 4]

-- join function := flatten a `nested monadic value` to a `monadic value`.
-- ((),[10,1,2,0,0,0])
re4 :: ((), Stack)
re4 = runState (join (state $ \s -> (push 10, 1 : 2 : s))) [0, 0, 0]

-- filterM
-- use Predicate with returning Monad(:= Monadic predicate)
-- mapM_ putStrLn . snd . runWriter $ filterM keepSmall [0,6,2,3,7,9,1]
keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
    tell ["Keeping: " ++ show x]
    return True
  | otherwise = do
    tell [show x ++ " is too large, throwing it away"]
    return False

{-
powerset [1,2,3] # [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]
1 -> [True, False] -> [[1], []]
2 -> [True, False] -> [[1,2], [1], [2]]
3 -> [True, False] -> [[1,2,3], [1,2], [1,3], [1], [2,3], [2], [3], []]
-}
powerset :: [a] -> [[a]]
powerset = filterM (const [True, False]) -- \x -> [True, False]

-- Safe RPN
solveRPN :: String -> Maybe Double
solveRPN st = do
  {-
  We pattern match here, so if the list has more than one value or none at all, the pattern match fails and a Nothing is produced.
  -}
  [result] <- foldM foldingFunction [] (words st)
  return result

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction (x : y : ys) "-" = return ((x - y) : ys)
foldingFunction (x : y : ys) "/" = return ((x / y) : ys)
foldingFunction (x : y : ys) "^" = return ((x ** y) : ys)
foldingFunction (x : xs) "ln" = return (log x : xs)
foldingFunction xs "sum" = return [sum xs]
{-
If the stack xs is [1.0,2.0] and readMaybe numberString results in a Just 3.0, the result is Just [3.0,1.0,2.0]. If readMaybe numberString results in a Nothing then the result is Nothing.
-}
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

-- readMaybe "1" :: Maybe Int           # Just 1
-- readMaybe "GO TO HELL" :: Maybe Int  # Nothing
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

-- Just normal function composition. f 1
f :: Integer -> Integer
f = GHC.Base.foldr (.) id [(+ 1), (* 100), (+ 1)]

-- General impl for KnightsQueat in3
-- We use `return` to make a starting position, which is a `singleton list`
inMany :: Int -> KnightPos -> [KnightPos]
inMany x = Data.List.foldr (<=<) return (replicate x moveKnight)

-- inMany x start = return start >>= Data.List.foldr (<=<) return (replicate x moveKnight)

-- canReachIn 3 (1,1) (1,3) # True
canReachIn :: Int -> KnightPos -> KnightPos -> Bool
canReachIn x start end = end `elem` inMany x start

-- Impl Monad
thisSituation :: Prob (Prob Char)
thisSituation =
  Prob
    [ (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4),
      (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

-- fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])
instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs -- Data.Bifunctor.first f

-- concat := the concatenation of all the elements of a cointainer of lifes. i.g. concat ([[1, 2, 3], [3..4]])
-- https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Foldable.html#g:5
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multAll xs
  where
    -- multAll :: (Prob a, Rational) -> [(a, Rational)]
    multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]

-- Prob f <*> Prob [(x, p)] = Prob [(x, p)]
-- key in this chap := how to impl `>>=`
-- Notice that its type is Prob (Prob Char) above `thisSituation`. So now that we've figure out how to flatten a nested probability list, all we have to do is write the code for this and then we can write `>>=` simply as join (fmap f m)
instance Monad Prob where
  -- (fmap f m) := (Prob b), but just return (Prob b) becomes Prob (Prob b)
  m >>= f = flatten (fmap f m)

instance Fail.MonadFail Prob where
  fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

-- All three of them will land tails nine times out of forty
flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])
