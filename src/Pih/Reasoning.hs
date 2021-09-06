{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Pih.Reasoning where

data Nat = Zero | Succ Nat deriving (Show)

add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add n m)

{-
*** 論証1: ***
基底部は、`p := 全ての自然数 m に対して add Zero m = m が成り立つ`ことを意味する
・全ての自然数 n に対して`加算の単位元の法則` add n Zero = n
add Zero Zero
Zero

再帰部は、`全ての自然数 n に対して p が成り立つならば、p (Succ n) も成り立つ`ことを意味する
  - すなわち、`add n Zero = n` が成り立つと仮定し、`add (Succ n) Zero = Succ n` が成り立つことを示す.

add (Succ n) Zero -- add を適用
Succ (add n Zero) -- add n Zero = n を適用
Succ n
□

*** 論証2: ***
自然数の加算に対する結合則
任意の自然数 x,y,z に対し、`add x (add y z) = add (add x y) z` が成立する
x,y,z のうちどれに対して数学的帰納法wもちいれば良いか? - add は第一引数に対するパターンマッチを用いて定義されており、その引数として x が２回、y が一回使われている. そのため、add の結合則を証明するには、x について数学的帰納法を使うのが自然である. (Pih234)

add Zero (add y z)
add y z        -- 外側の add を適用
add (add Zero y) z -- add を`逆適用`

add (Succ x) (add y z)
Succ (add x (add y z)) -- 再帰部を適用
Succ (add (add x y) z) -- 仮定を適用
add (Succ (add x y)) z -- 外側の再帰部を逆適用
add (add (Succ x) y) z -- 内側の再帰部を逆適用
□
-}

-- *** Compiler's Correctneess ***

--- Also check AbstractMachine.hs
data Expr = Val Int | Add Expr Expr deriving (Show)

data Op = PUSH Int | ADD deriving (Show)

type Stack = [Int]

type Code = [Op]

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

-- Executor := スタック上に翻訳結果を実行する
exec :: Code -> Stack -> Stack
exec [] s = s
exec (PUSH n : c) s = exec c (n : s) -- スタック上に新しい整数をおく
exec (ADD : c) (m : n : s) = exec c (n + m : s) -- スタック上の二つの整数をその和で置き換える

-- Compiler := code をコンパイルして、スタック上に翻訳結果をおく
-- comp :: Expr -> Code
-- comp (Val n) = [PUSH n]
-- comp (Add x y) = comp x ++ comp y ++ [ADD]

comp :: Expr -> Code
comp e = comp' e []

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add x y) c = comp' x (comp' y (ADD : c))

testExpr :: Expr
testExpr = Add (Add (Val 2) (Val 3)) (Val 4)

{-
-- λ eval e # 9
-- λ flip exec [] $ comp testExpr # [9]s

`exec (comp e) [] = [eval e]` := Hypothesis
`exec (comp e) s = eval e : s` -- 上記の性質を証明するために初期値を空のスタックではなく任意のスタックに拡張しておく

Base:
exec (comp (Val n)) s
exec [PUSH n] s  -- comp 適用
exec [] (n : s)  -- exec 適用
n : s            -- exec 適用
eval (Val n) : s -- eval 逆適用
□

Recursive:
exec (comp (Add x y)) s
exec (comp x ++ comp y ++ [ADD]) s       -- comp 適用
exec (comp x ++ (comp y ++ [ADD])) s     -- ++ の結合則
exec (comp y ++ [ADD]) (exec (comp x) s) -- 分配則
exec (comp y ++ [ADD]) (eval x : s)      -- x に対する仮定より
exec [ADD] (exec (comp y) (eval x : s))  -- 分配則
exec [ADD] (eval y : eval x : s)         -- y に対する仮定
exec [] (eval y + eval x : s)            -- exec 適用
(eval x + eval y) : s                    -- exec 適用
eval (Add x y) : s                       -- eval 逆適用
□

After removing ++ (連結演算子) with 構造的機能法
`exec (comp' e c) s = exec c (eval e : s)` := Hypothesis
...
-}