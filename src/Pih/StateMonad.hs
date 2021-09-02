{-# LANGUAGE TupleSections #-}

module Pih.StateMonad where

import Control.Monad.Identity (Functor)

type State = Int

-- State Transformer
newtype ST a = S (State -> (a, State))

-- print stPlusOne # S (State -> (a, State))
instance Show (ST a) where
  show (S st) = "S (State -> (a, State))"

-- State 型変数に特化した helper := 構成子を取り除き State を ST に適用した結果（a, State）を返す関数.
-- （a, State） a := 変換の結果, State := 更新された状態
-- λ app stPlusOne (1 :: State) # (2,2)
app :: ST a -> State -> (a, State)
app (S st) = st

-- 関手（functor）
-- λ app (fmap plusOne stPlusOne) (1 :: State) # (3,2)
instance Functor ST where
  -- 「純粋関数」と ST を受け取り、ST の結果に「純粋関数」を適用した結果を ST の結果として返す.
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

-- Applicative Functor
-- λ app (pure plusOne <*> stPlusOne) (1 :: State) # (3,2)
-- λ app (plusOne <$> stPlusOne) (1 :: State)
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (x,) -- (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s -- `pure f` から `f` を取り出して、s を s' としてそのまま返すだけ
              (x, s'') = app stx s'
           in (f x, s'')
      )

-- Monad
-- λ app (stPlusOne >>= plusOneSt) (1 :: State)       # (3,2)
-- λ app (stPlusOne >>= (makeFnSt (*2))) (1 :: State) # (4,2)
-- `f` は`結果`を適用した結果、 ST になる`関数`
instance Monad ST where
  -- Expected type: ST b, Actual type: ST (ST b)
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')

plusOne :: Int -> Int
plusOne = (+ 1)

stPlusOne :: ST Int
stPlusOne = S (\n -> let newN = plusOne n in (newN, newN))

-- `pure` でなくても`結果`を受け取って（State ではない）それを関数が適用できるような ST であればなんでも良い.
-- 例えば, `makeFnSt`
plusOneSt :: Int -> ST Int
plusOneSt n = pure $ plusOne n

makeFnSt :: (t -> a) -> t -> ST a
makeFnSt g n = S (g n,)

-- No need! use `pure` instead
-- stPlusOne' :: ST (Int -> Int)
-- stPlusOne' = S (plusOne,)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

-- test
ctree :: Tree Char
ctree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- λ fst (rlabel ctree 0) # Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n') = rlabel l n
    (r', n'') = rlabel r n'

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

-- λ app (alable ctree) 0   # (Node (Node (Leaf 0) (Leaf 1)) (Leaf 2),3)
-- λ app (Leaf <$> fresh) 0 # (Leaf 0,1)
-- λ app (Node <$> (Leaf <$> fresh) <*> (Leaf <$> fresh)) 0 # (Node (Leaf 0) (Leaf 1),2)
alable :: Tree a -> ST (Tree Int)
alable (Leaf _) = Leaf <$> fresh
alable (Node l r) = Node <$> alable l <*> alable r

-- With using Monad
-- λ app (mlable ctree) 0
mlable :: Tree a -> ST (Tree Int)
mlable (Leaf _) = do
  g <- fresh
  return (Leaf g)
mlable (Node l r) = do
  n <- mlable l
  m <- mlable r
  return (Node n m)