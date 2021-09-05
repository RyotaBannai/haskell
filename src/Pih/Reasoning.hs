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