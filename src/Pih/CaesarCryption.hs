module Pih.CaesarCryption where

import Data.Char (chr, isAsciiLower, isLower, ord)
import Pih.Common (aPositions, count)

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
    factor = head (aPositions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0 .. 25]]
    table' = freqs xs