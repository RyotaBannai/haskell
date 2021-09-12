module Archive.State where

-- import Control.Monad.Trans.State (modify)

import Control.Arrow
import Control.Monad.State

{-
| <https://xtech.nikkei.com/it/article/COLUMN/20070109/258229/>

* State s という型変数を一つ保持する

State:
return a = State $ \s -> (a, s)
m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s'

MonadState:
get   = State $ \s -> (s, s)
put s = State $ \_ -> ((), s)

* get/put は前の状態を捨てるような処理（新しい値を置き換える）
* 処理する対象に含まれない値が欲しい場合は、lambda を活用. \s -> function (+ 1) >> return s

* evalState: 評価した結果の「値」だけを返す 12
* execState: 実行された「状態」のみを返す   11
-}

-- test = (12,11)
test :: (Int, Int)
test = runState (return 12) 11

-- test = (11,11)
test1 :: (Int, Int)
test1 = runState (return 12 >> get) 11 -- get は引数を取らないため >>= はできない (runState m s) 部分が通らない.

test1' :: (Int, Int)
test1' = runState get 11

test2 :: ((), Int)
test2 = runState (put 10) 11

-- test3 = (12,10) -- discard 11
test3 :: (Int, Int)
test3 = runState (put 10 >> return 12) 11

-- test4 = ((),13)
{-
return 12 0 = (12,0)

m >>= k  = State $ \s -> let
		(a, s') = runState m s
		in runState (k a) s' -- a == 12, s' == 0

`>>=` は`評価した結果`に対して関数を適用
-}
test4 :: ((), Int)
test4 = runState (return 12 >>= \s -> put $ s + 1) 0

-- test5 = (1,2)
test5 :: (Int, Int)
test5 =
  runState
    ( do
        return 12
        s <- get
        put $ s + 1
        return s
    )
    1

-- test6 = (12,0) {- (12,0), s == 12, (id s, 0), (12, 0) -}
test6 :: (Int, Int)
test6 = runState (return 12 >>= \s -> return . id $ s) 0

-- test6' = (13,0)
test6' :: (Int, Int)
test6' = runState (return 12 >>= \s -> return . (+ 1) $ s) 0

-- test6' = ((),14)
test6'' :: ((), Integer)
test6'' = runState (return 12 >>= \s -> return (s + 1) >>= \s -> put $ s + 1) 0

{-
gets f = do
	s <- get
	return (f s)

modify f = do
	s <- get
	put (f s)

* gets, modify は新しい処理に置き換え + 関数適用
* いずれも引数を取らない
-}

-- test7 = (1,0)
test7 :: (Int, Int)
test7 = runState (return 12 >> gets (+ 1)) 0

-- test8 = ((),2)
test8 :: ((), Integer)
test8 = runState (return 12 >> modify (+ 1)) 1

-- test8' = (11,22)
test8' :: (Integer, Integer)
test8' = runState (get >>= \s -> modify (+ s) >> return s) 11

--  test8'' = ((),22)
test8'' :: ((), Integer)
test8'' = runState (get >>= \s -> modify (+ s)) 11

-- test9 = (12,23) -- (12,11), \12 -> (11+12), (0,23), (12,23)
test9 :: (Integer, Integer)
test9 = runState (gets (+ 1) >>= \s -> modify (+ s) >> return s) 11

-- test9' = ((),24) -- (12,11), \12 -> (11+12), (0,23), (0,23)
test9' :: ((), Integer)
test9' = runState (gets (+ 1) >>= \s -> modify (+ s) >> modify (+ 1)) 11

{-
* mapState 「値」だけではなく，「状態」のほうに対しても変化を加えることができる
* withState 「状態」に対してのみ関数を適用したいという場合
-}

-- test10 = (13,12) -- (12,11), (13,12) -- runState の初回引数は `return 12` が適用
test10 :: (Integer, Integer)
test10 = flip runState 11 $ mapState (\(x, y) -> (x + 1, y + 1)) (return 12)

-- test11 = (13,11)
test11 :: (Integer, Integer)
test11 = flip runState 11 $ mapState (first (+ 1)) (return 12)

test11' :: (Integer, Integer)
test11' = flip runState 11 $ fmap (+ 1) (return 12) -- fmap は値に対して適用

test11'' :: (Integer, Integer)
test11'' = flip runState 11 $ return ((+ 1) 12) -- ok

-- test12 = (12,12) -- (12,11), (12,12)
test12 :: (Integer, Integer)
test12 = flip runState 11 $ withState (+ 1) (return 12) -- fmap は値に対してではなく、状態に対して適用

test12' :: (Integer, Integer)
test12' = flip runState 11 $ mapState (second (+ 1)) (return 12)