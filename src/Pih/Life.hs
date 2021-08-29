module Pih.Life where

import Data.List (nub)
import Pih.Common (newline)

-- *** 左上 (1,1)を原点とする

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs =
  do
    goto p
    putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

-- `生きているセル`の位置をリストとしてボードを表す.
type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

spaceship :: Board
spaceship = [(3, 3), (6, 3), (7, 4), (3, 5), (7, 5), (4, 6), (5, 6), (6, 6), (7, 6)]

line10 :: Board
line10 = [(6, 6 + n) | n <- [0 .. 9]]

fourisland :: Board
fourisland = [(6, 1), (6, 2), (6, 3), (2, 5), (3, 5), (4, 5), (8, 5), (9, 5), (10, 5), (6, 7), (6, 8), (6, 9)]

-- sequence_ :: [IO a] -> IO () はアクションのリストの要素を順に実行し、結果の値を破棄してユニットを返すプレリュード関数.
showcells :: Board -> IO ()
showcells b =
  do
    showcells' b
    newline

showcells' :: [Pos] -> IO ()
showcells' b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isDead :: Board -> Pos -> Bool
isDead b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x, y) =
  map
    wrap
    [ (x -1, y -1),
      (x, y -1),
      (x + 1, y -1),
      (x -1, y),
      (x + 1, y),
      (x -1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

-- wrap (11,11) -> (1,1)
wrap :: Pos -> Pos
wrap (x, y) = (((x -1) `mod` width) + 1, ((y -1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

-- Generate next gen live cells
-- b := current board
survivors :: Board -> [Pos]
survivors b = [p | p <- b, liveneighbs b p `elem` [2, 3]]

-- 新たにセルを生み出す可能性のあるのは生きているセルの隣のみなので、その周りの cell だけに絞り探索する.
-- neighbs に map すると cell が重複する可能性があるため unique に絞り込む(List.nub).
-- births' と違い膨大なセルでも効率良く探索
births :: Board -> [Pos]
births b =
  [ p
    | p <- nub $ concatMap neighbs b,
      isDead b p,
      liveneighbs b p == 3
  ]

-- slower impl
births' :: Board -> [Pos]
births' b =
  [ (x, y)
    | x <- [1 .. width],
      y <- [1 .. height],
      isDead b (x, y),
      liveneighbs b (x, y) == 3
  ]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do
  cls
  showcells b
  wait 100000
  life (nextgen b)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1 .. n]]