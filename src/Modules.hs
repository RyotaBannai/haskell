module Modules where

import Data.Function
import Data.List

{-
If you're in GHCI and you want to be able to call the functions exported by Data.List, do this:
:m + Data.List
-}
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- nub weeds out duplicate elements

haha :: IO ()
haha = putStr . (++ "\n") . intercalate "\n" . take 10 . iterate (++ "haha") $ "haha"

resultSentence :: [Char]
resultSentence = let (fw, rest) = span (/= ' ') "this is a sentence" in "First word:'" ++ fw ++ "', the rest:'" ++ rest ++ "'"

fib :: (Eq a, Num a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib n = fib (n -1) + fib (n -2)

groupByNumOfAppearance :: [(Integer, Int)]
groupByNumOfAppearance = map (\l@(x : xs) -> (x, length l)) . group . sort $ [1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 2, 2, 2, 5, 6, 7]

{-
[[1,1,1,1],[2,2,2,2,2,2,2],[3,3],[5],[6],[7]]
x: xs := 1: [1,1,1]
l := [1,1,1,1]
-}

getFirstStockOverX :: (Ord a, Num a) => a -> Maybe (a, a, a, a)
getFirstStockOverX x = let stock = [(900, 2000, 1, 1), (1000, 2001, 1, 1), (1100, 2002, 1, 1)] in find (\(val, y, m, d) -> val > x) stock

useOnSimple :: [[Double]]
useOnSimple = let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3] in groupBy ((==) `on` (> 0)) values

useOnCompare = let xs = [[5, 4, 5, 4, 4], [1, 2, 3], [3, 5, 4, 3], [], [2], [2, 2]] in sortBy (compare `on` length) xs