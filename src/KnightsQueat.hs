module KnightsQueat where

import Control.Arrow (Arrow (first))
import Control.Monad

type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <-
    -- all possible moves for a Knight
    [ (c + 2, r -1),
      (c + 2, r + 1),
      (c - 2, r -1),
      (c + 2, r + 1),
      (c + 1, r -2),
      (c + 1, r + 2),
      (c - 1, r -2),
      (c - 1, r + 2)
      ]
  -- check if a Knight is still on the board.
  guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
  return (c', r')

-- in3 (6,2) # [(5,2),(8,1),(8,5),(6,1),(6,5),..]
in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start