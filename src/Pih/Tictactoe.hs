module Pih.Tictactoe where

import Data.Char
import Data.List
import Pih.Common (Pos, cls, goto, newline, readLine)
import System.IO

{-
 For test, d=[[B,O,O],[O,B,O],[O,O,B]]
 d = [
   [B, O, O],
   [O, B, O],
   [O, O, B]]
-}

size :: Int
size = 3

-- 内側と外側のリストは同じ長さとする.
type Grid = [[Player]]

-- B := blank
data Player = O | B | X deriving (Show, Ord, Eq)

-- Returns next Player
next :: Player -> Player
next O = X
next B = B
next X = O

empty :: Grid
empty = replicate size (replicate size B) -- [[B,B,B],[B,B,B],[B,B,B]]

-- check if Grid is full
full :: Grid -> Bool
full = notElem B . concat -- notElem B:= all (/= B)

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
    ps = concat g

-- takes diagonal elements as a list
-- diag [[1,2],[3,4]] # [1,3], diag . reverse $ [[1,2],[3,4]] # [2,4], diag $ reverse [[B,O,O],[O,B,O],[O,O,B]]
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0 .. size -1]]

-- check if either of them win
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
  where
    line = all (== p)
    rows = g
    cols = transpose g -- [[1,2],[3,4]] -> [[1,3],[2,4]]
    dias = [diag g, diag (map reverse g)]

won :: Grid -> Bool
won g = wins O g || wins X g

showRow :: [Player] -> [String]
showRow = beside . interleave vbar . map showPlayer
  where
    beside = foldr1 (zipWith (++)) -- foldr1 (zipWith (++)) [["a","b"], ["|","|"]] # ["a|","b|"]
    vbar = replicate 3 "|"

showPlayer :: Player -> [String]
-- 上下を１行分空けたいため先頭と末尾に空白用の空文字を追加.
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave hbar . map showRow
  where
    hbar = [replicate ((size * 4) -1) '-']

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size ^ 2 && concat g !! i == B

-- [[True] | True] # [[True]], [[True] | False] # []
-- move := returns null if invalid
move :: Grid -> Int -> Player -> [Grid]
move g i p = [chop size (xs ++ [p] ++ ys) | valid g i]
  where
    (xs, B : ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt =
  do
    putStrLn prompt
    xs <- readLine
    newline
    if xs /= [] && all isDigit xs
      then return (read xs)
      else do
        putStrLn "Error: Invalid number"
        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p =
  do
    cls
    goto (1, 1)
    putGrid g
    run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!"
  | wins X g = putStrLn "Player X wins!"
  | full g = putStrLn "It's draw!"
  | otherwise =
    do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "Error: Invalid move"
          run' g p
        [g'] -> run g' (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "
