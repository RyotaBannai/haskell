module Pih.AbstractMachien where

-- *** Abstract machine ***

{-
eval := 後で評価する右の式を制御スタックに記録しながら、次の構造を表す木において最も左の式である整数まで下っていく.
exec := 制御を eval に戻したり、加算を実行したりしながら、記録された軌跡を上に戻っていく.

evaluation details described in p108
-}
-- 式の評価の順序を制御するため`制御スタック`を導入する
data Expr = Val Int | Add Expr Expr

-- Evaluation without a `Control Stack`
-- value (Add (Add (Val 2) (Val 3)) (Val 4)) # 9
value' :: Expr -> Int
value' (Val n) = n
value' (Add x y) = value' x + value' y

-- value' := 式を評価して整数にする関数. 与えられた式と空の制御スタックを引数に指定して eval を実行.
-- value (Add (Add (Val 2) (Val 3)) (Val 4)) # 9
value :: Expr -> Int
value e = eval e []

type Cont = [Op]

data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c) m = exec c (n + m)

-- folde id (+) (Add (Add (Val 2) (Val 3)) (Val 4)) # 9
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val n) = f n
folde f g (Add n m) = g (folde f g n) (folde f g m)

folde' :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde' f g = go
  where
    go (Val n) = f n
    go (Add n m) = g (go n) (go m)

-- eval' (Add (Add (Val 2) (Val 3)) (Val 4)) # 9
eval' :: Expr -> Int
eval' = folde id (+)

-- *** size := 式の中に整数がいくつあるか数える ***

-- size (Add (Add (Val 2) (Val 3)) (Val 4)) # 3
size :: Expr -> Integer
size = folde (const 1) (+)