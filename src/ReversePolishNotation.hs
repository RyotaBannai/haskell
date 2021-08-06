module ReversePolishNotation where

import Data.List
import GHC.Float

solveRPN :: (Num a, Floating a, Read a) => String -> a
solveRPN = head . foldl foldingFunction [] . words
  where
    -- to make fault tolerance one, use reads and check if it's empty
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (x - y) : ys
    foldingFunction (x : y : ys) "/" = (x / y) : ys
    foldingFunction (x : y : ys) "^" = (x ** y) : ys
    foldingFunction (x : xs) "ln" = log x : xs -- NOTE: `log of x` : list of `xs` joined togther.
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs

{-
"10 4 - 6 - 5 2 2 * -" -- -1 : the last number first comes so 4 sits in `x` spot and 5 sits in `y` spot.
-}