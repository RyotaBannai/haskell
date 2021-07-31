module Modules where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map

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

verboseWordsAlternative = filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"

-- Map
-- fromList := dupliated key will be discarded.
myMap = Map.fromList [("betty", "555-2938"), ("bonnie", "452-2928"), ("lucille", "205-2928")]

phoneBook =
  [ ("betty", "555-2938"),
    ("betty", "342-2492"),
    ("bonnie", "452-2928"),
    ("patsy", "493-2928"),
    ("patsy", "943-2929"),
    ("patsy", "827-9162"),
    ("lucille", "205-2928"),
    ("wendy", "939-8282"),
    ("penny", "853-2492"),
    ("penny", "555-2111")
  ]

-- Store as concatenated String
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ "," ++ number2)

-- Store as List
phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

{-
insertWith := applies function when map already has the same key.
-}