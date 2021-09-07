module Archive.ArrowExpr where

import Control.Arrow ((&&&), (***), (>>>))

-- *** 状態変換 ***

-- λ add (*2) (+2) 2
add :: (a -> Int) -> (a -> Int) -> (a -> Int)
add f g a = f a + g a

type State s a b = (s, a) -> (s, b)

addST :: State s a Int -> State s a Int -> State s a Int
-- addST :: Num b1 => ((a1, b2) -> (a2, b1)) -> ((a2, b2) -> (a3, b1)) -> (a1, b2) -> (a3, b1)
addST f g (s, b) =
  let (s', x) = f (s, b)
      (s'', y) = g (s', b)
   in (s'', x + y)

initialState :: ([Char], Int)
initialState = ("", 3)

consumer :: (Int -> Int) -> String -> State String Int Int
consumer g msg (s, b)
  | b > 0 = ("", g b)
  | otherwise = (msg ++ show b, b)

consumer1 :: State String Int Int
consumer1 = consumer (* 2) "consumer1 needs b to be more than 0, but got "

consumer2 :: State String Int Int
consumer2 = consumer (+ 2) "consumer2 needs b to be more than 0, but got "

use :: (String, Int)
use = addST consumer1 consumer2 initialState

-- *** 非決定性 ***

-- non determinant
type NonDet a b = a -> [b]

-- λ addND (\x -> [x * 2]) (\x -> [x + 2]) 3 # [11]
-- λ addND (\x -> [x * 2, - (x * 2)]) (\x -> [x + 2, - (x + 2)]) 3 # [11,1,-1,-11]
-- これはそれぞれの出力の要素を足し合わせているに過ぎない
addND :: NonDet a Int -> NonDet a Int -> NonDet a Int
addND f g a = [x + y | x <- f a, y <- g a]
