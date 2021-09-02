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
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S (x,) -- (\s -> (x, s))

  -- <*> :: ST (a -> b) -> ST a -> ST b
  stf <*> stx =
    S
      ( \s ->
          let (f, s') = app stf s
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
