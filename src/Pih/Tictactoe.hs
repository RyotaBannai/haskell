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

-- Returns current Player
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

-- vs PC
data Tree a = Node a [Tree a] deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0 .. ((size ^ 2) -1)]]

{-
gametree d X
Node [[B,O,O],[O,B,O],[O,O,B]] [

  Node [[X,O,O],[O,B,O],[O,O,B]] [
    Node [[X,O,O],[O,O,O],[O,O,B]] [],
    Node [[X,O,O],[O,B,O],[O,O,O]] []
  ],
  Node [[B,O,O],[O,X,O],[O,O,B]] [
    Node [[O,O,O],[O,X,O],[O,O,B]] [],
    Node [[B,O,O],[O,X,O],[O,O,O]] []
  ],
  Node [[B,O,O],[O,B,O],[O,O,X]] [
    Node [[O,O,O],[O,B,O],[O,O,X]] [],
    Node [[B,O,O],[O,O,O],[O,O,X]] []
  ]
]

-- null means a leaf
-}

-- 枝刈り for saving memory and time
-- prune 0 $ gametree d X # Node [[B,O,O],[O,B,O],[O,O,B]] []
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n -1) t | t <- ts]

depth :: Int
depth = size ^ 2

-- minimax
{-
1. leaf ならばその時点の winner (O or X) を選択するか、winner がまだ決定していない場合は B (:= blank) を選択
2. node ならば一階層下にある子（children, nodes(or leafs)）の全ラベルから最小または最大を選択（min or max）
   - min or max の選定は、その時点（ラベルを決定したい node）の Player によって決める
     - O の場合は children のうちから min を選択
     - X の場合は children のうちから max を選択
   - この大小判定は data に宣言した順で機械的に決定（O < B < X）

現在の根のラベルと同じラベル付をされた選択が Best choice となる.
別のラベルを選択する == 別の Player が勝つ機会を与える
-}
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts'] -- 得られた木々の根のラベルを選び出す

-- depth 先のパターンまで計算し、その手までのベストを選択
bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where
    tree = prune depth (gametree g p)
    Node (_, best) ts = minimax tree

-- デフォルトでは出力がバッファリングされるため、それを止めるために関数 `hSetBuffering` を使用.
-- NOTE: commend out when needs a compile
-- main :: IO ()
-- main =
--   do
--     hSetBuffering stdout NoBuffering
--     play empty O

play :: Grid -> Player -> IO ()
play g p =
  do
    cls
    goto (1, 1)
    putGrid g
    play' g p

-- `$!` := Strict (call-by-value) application operator. This calculates `bestmove g p` when `play` is called as opposed to Lazy evaluation, which calculates the function when it's needed.
play' :: Grid -> Player -> IO ()
play' g p
  | wins O g = putStrLn "Player O wins!"
  | wins X g = putStrLn "Player X wins!"
  | full g = putStrLn "It's draw!"
  | p == O =
    do
      i <- getNat (prompt p)
      case move g i p of
        [] -> do
          putStrLn "Error: Invalid move"
          play' g p
        [g'] -> play g' X
  | p == X =
    do
      putStr "Player X is thinking..."
      (play $! bestmove g p) O
