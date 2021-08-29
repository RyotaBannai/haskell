module Pih.Nim where

import Data.Char (digitToInt, isDigit)
import Pih.Common (newline)

-- returns next player (in number)
next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row -1) >= num

-- move initial 1 3 # [2,4,3,2,1]
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1 ..] board]
  where
    update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do
  putStr (show row)
  putStr ": "
  putStrLn (concat (replicate num "* "))

-- putBoard initial
putBoard :: Board -> IO ()
putBoard [a, b, c, d, e] =
  do
    putRow 1 a
    putRow 2 b
    putRow 3 c
    putRow 4 d
    putRow 5 e

putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow n x | (n, x) <- zip [1 ..] xs]

getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x
    then return (digitToInt x)
    else do
      putStrLn "ERROR: Invalid digit"
      getDigit prompt

-- localize and minimize side effects in the few functions and extracts as much helper functions as possible out of it.
play :: Board -> Int -> IO ()
play board player =
  do
    newline
    putBoard board
    if finished board
      then do
        newline
        putStr "Player "
        putStr (show (next player))
        putStrLn " wins!!"
      else do
        newline
        putStr "Player "
        print player
        row <- getDigit "Enter a row number: "
        num <- getDigit "Stars to remove: "
        if valid board row num
          then play (move board row num) (next player)
          else do
            newline
            putStrLn "Error: Invalid move"
            play board player -- fp is immutable, therefore pass state on the next call.
