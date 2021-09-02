module Pih.LuhnAlgo where

import Data.Char (chr, ord)
import Pih.Common (Bit, bin2int, evens, int2bit, odds, unfold)

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

-- Encrypt String with converting each to bytes
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