module Ph2.MiscCodes where

import Data.Char
import qualified Data.List as List

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

-- Luhn algorithm
luhnDouble :: (Ord p, Num p) => p -> p
luhnDouble x = let doubled = x * 2 in if doubled > 9 then doubled -9 else doubled

firsts :: [(a, b)] -> [a]
firsts xs = [fst x | x <- xs]

-- apply luhnDoulbe to even index from right
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = remain == 0
  where
    zipped = zipWith (\i x -> (if even i then (i, luhnDouble x) else (i, x))) [1 ..] [z, y, x, w]
    remain = (`mod` 10) . sum . firsts $ zipped

-- `evens` starts from index 0, which is even.
luhn' :: Int -> Int -> Int -> Int -> Bool
luhn' w x y z = let xs = [z, y, x, w] in ((`mod` 10) . sum $ evens xs ++ (map luhnDouble . odds $ xs)) == 0

-- Caesar cryption
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr $ ord 'a' + n

shift :: Int -> Char -> Char
shift n c
  | isLower c = int2let ((let2int c + n) `mod` 26)
  | otherwise = c

encode :: Int -> [Char] -> [Char]
encode n xs = [shift n x | x <- xs]

lowers :: [Char] -> Int
lowers xs = length [x | x <- xs, isAsciiLower x]

count :: Eq a => a -> [a] -> Int
count x xs = length [x' | x' <- xs, x == x']

find :: Eq k => k -> Assoc k v -> [v]
find k t = [v | (k', v) <- t, k == k']

findFirst :: Eq k => k -> Assoc k v -> v
findFirst k t = head $ find k t

positions :: (Num a1, Enum a1, Eq a2) => a2 -> [a2] -> [a1]
positions x xs = find x (zip xs [0 ..])

percent :: (Fractional a1, Integral a2, Integral a3) => a2 -> a3 -> a1
percent n m = (fromIntegral n / fromIntegral m) * 100

freqs :: Fractional a => [Char] -> [a]
freqs xs = let n = lowers xs in [percent (count x xs) n | x <- ['a' .. 'z']]

-- 計算された値が小さければ小さいほど２つのリストはよく似ている.
chisqr :: Fractional a => [a] -> [a] -> a
chisqr os es = sum [(o - e) ^ 2 / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

table :: [Double]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0, 0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0, 6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

crack :: [Char] -> [Char]
crack xs = encode (- factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs

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

--
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

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

stringTobits :: String -> [Bit]
stringTobits = concatMap (make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = let (unit, rest) = splitAt 8 bits in unit : chop8 rest

chop8' :: [a] -> [[a]]
chop8' = unfold null (take 8) (drop 8)

bitsTostring :: [Bit] -> String
bitsTostring = map (chr . bin2int) . chop8

-- test
-- transmit "haskell is fun." # "haskell is fun."
transmit :: String -> String
transmit = bitsTostring . channel . stringTobits

channel :: [Bit] -> [Bit]
channel = id

-- simple voting system
votes :: [String]
votes = ["Red", "Blue", "Green", "Red", "Blue", "Blue"]

countVote :: Eq a => a -> [a] -> Int
countVote x = length . filter (== x)

-- same as `nub`
-- rmdups :: Eq a => [a] -> [a]
-- rmdups [] = []
-- rmdups (x : xs) = x : rmdups (filter (/= x) xs)

-- [(1,"Green"),(2,"Red"),(3,"Blue")]
result :: Ord b => [b] -> [(Int, b)]
result vs = List.sort [(count v vs, v) | v <- List.nub vs]

winner :: Ord a => [a] -> a
winner = snd . last . result

-- Yet another simple voting system := 第一の選択として名前を書かれた投票が最低だった候補者を取り除く. これを候補者が１人になるまで繰り返す.

ballots :: [[String]]
ballots = [["Red", "Green"], ["Blue"], ["Green", "Red", "Blue"], ["Blue", "Green", "Red"], ["Green"]]

rmempty :: Eq a => [[a]] -> [[a]]
rmempty = filter (/= [])

elim :: Eq a => a -> [[a]] -> [[a]]
elim x = map (filter (/= x))

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner' :: Ord a => [[a]] -> a
winner' bs = case rank (rmempty bs) of
  [c] -> c
  (c : cs) -> winner' (elim c bs)

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

data Nat = Zero | Succ Nat deriving (Show)

-- int2nat 3
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

-- nat2int $ Succ (Succ (Succ Zero))
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

-- Tautology(恒真式) := always returns True
-- 命題が恒真式であるかを判断する関数を定義する第一歩は、命題を表す型 Prop の宣言である.
-- 命題を構成する五種類の要素について、それぞれ構成子を定義する:
data Prop
  = Const Bool
  | Var Char
  | Not Prop -- ¬
  | And Prop Prop -- ∧
  | Imply Prop Prop -- ⇒

-- A ∧ ¬A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A ∧ B) ⇒ A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A ⇒ (A ∧ B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A ∧ (A ⇒ B)) ⇒ B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

type Assoc k v = [(k, v)]

-- 置換表 := 変数名 <-> 真理値
type Subst = Assoc Char Bool

-- [('A', False), ('B', True)]

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = findFirst x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q