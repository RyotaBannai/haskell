module Ph2.MiscCodes where

import Data.Char

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

find :: Eq a1 => a1 -> [(a1, a2)] -> [a2]
find k t = [v | (k', v) <- t, k == k']

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