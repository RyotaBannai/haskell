module Pih.Common where

import Data.Char (chr, ord)
import System.IO (hSetEcho, stdin)

-- import Control.Monad.Loops
type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

newline :: IO ()
newline = putChar '\n'

getCh :: IO Char
getCh = do
  hSetEcho stdin False -- echo back 機能を False (入力した文字を repl に表示しない).
  x <- getChar
  hSetEcho stdin True
  return x

readLine' :: IO String
readLine' = readLineCore' ""

readLineCore' :: String -> IO String
readLineCore' xs = do
  x <- getCh
  case x of
    '\n' -> return xs
    '\DEL' ->
      if null xs
        then readLineCore' ""
        else do
          -- putChar '\b'
          putStr "\b \b"
          readLineCore' $ init xs
    _ -> do
      putChar x
      readLineCore' (xs ++ [x])

readLine :: IO String
readLine = dropWhile (== '\DEL') <$> readLineCore

readLineCore :: IO String
readLineCore = do
  x <- getCh
  if x == '\n'
    then return []
    else do
      putOrDel x
      xs <- readLineCore
      return $ delete (x : xs)
  where
    putOrDel x
      | x == '\DEL' = putStr "\b \b"
      | otherwise = putChar x
    delete (x : '\DEL' : xs)
      | x /= '\DEL' = xs
    delete xs = xs

-- the same as prelude function `getLine`
getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

type Assoc k v = [(k, v)]

aFind :: Eq k => k -> Assoc k v -> [v]
aFind k t = [v | (k', v) <- t, k == k']

aFindFirst :: Eq k => k -> Assoc k v -> v
aFindFirst k t = head $ aFind k t

aPositions :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
aPositions x xs = aFind x (zip xs [0 ..])

type Bit = Int

-- iterate := 引数の関数を引数の値に繰り返し適用し、無限リストを生成
-- bin2int [1,0,1,1] # 13 # 右に進むにつれ重みが 2 倍になる
bin2int :: [Bit] -> Int
bin2int bits = let weights = iterate (* 2) 1 in sum [x * b | (x, b) <- zip weights bits]

-- (1*a) + (2*b) + (4*c) + (8*d) を整理　p86
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit n = n `mod` 2 : int2bit (n `div` 2)

int2bit' :: Int -> [Bit]
int2bit' = unfold (== 0) (`mod` 2) (`div` 2)

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

-- p: predicate
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

map' :: (b -> a) -> [b] -> [a]
map' f = unfold null (f . head) tail

-- take 10 $ iterate' (*2) 1
iterate' :: (b -> b) -> b -> [b]
iterate' f = unfold (const False) f f

-- tail without exception when empty list
safetail :: [a] -> [a]
safetail [] = []
safetail (x : xs) = xs

safetail' :: [a] -> [a]
safetail' xs
  | null xs = []
  | otherwise = tail xs

safetail'' :: [a] -> [a]
safetail'' xs = if null xs then [] else tail xs

-- take third element in the list
third :: [a] -> a
third xs = xs !! 2

third' :: [a] -> a
third' = head . tail . tail

third'' :: [a] -> a
third'' (_ : _ : x : _) = x

-- 多重再帰
-- fib := returns nth fibonacci value, where n > 0
fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n -2) + fib (n -1) -- n >= 2

-- with `unfold`
fib' :: [Integer]
fib' = [0, 1] ++ unfold (const False) (\[x, y] -> x + y) (\[x, y] -> [y, x + y]) [0, 1]

-- 相互再帰
even' :: Integral a => a -> Bool
even' 0 = True
even' n = odd' (n -1)

odd' :: Integral a => a -> Bool
odd' 0 = False
odd' n = even' (n -1)

evens :: [a] -> [a]
evens [] = []
evens (x : xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_ : xs) = evens xs

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = let mid = (`div` 2) . length $ xs in splitAt mid xs

-- 1. singleton list どうしの比較であれば確実にソート + マージ可能
-- 2. ソート済みの 2 つのリストであればマージ + ソート可能
-- merge [1,6,2] [9,5,1] # [1,6,2,9,5,1]
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys)
  | x < y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

-- msort [1,6,2,9,5,1] # [1,1,2,5,6,9]
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = let (left, right) = halve xs in merge (msort left) (msort right)

-- Experiment `data`
data Nat = Zero | Succ Nat deriving (Show)

-- nat2int $ Succ (Succ (Succ Zero))
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- int2nat 3
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n -1))

-- addNat (Succ (Succ Zero)) (Succ (Succ Zero))
addNat :: Nat -> Nat -> Nat
addNat n m = int2nat (nat2int n + nat2int m)

addNat' :: Nat -> Nat -> Nat
addNat' Zero n = n
addNat' (Succ m) n = Succ (addNat' m n) -- Succ m := Succ の中身を取り出す.

{-
addNat' (Succ (Succ Zero)) (Succ Zero)
Succ (addNat' (Succ Zero) (Succ Zero))
Succ (Succ (addNat' Zero (Succ Zero)))
Succ (Succ (Succ Zero))
-}