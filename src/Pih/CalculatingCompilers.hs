{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Pih.CalculatingCompilers where

import Pih.Reasoning (Expr (..), Stack, testExpr)

type Cont = Stack -> Stack

data Code = HALT | PUSH Int Code | ADD Code deriving (Show)

{-
`eval' e s = eval e : s` が成立するような eval の定義を考える:

Base:
eval' e s
eval' (Val n) s
eval (Val n) : s -- eval' の仕様
n : s            -- eval を適用
push n s         -- `push n s = n : s`と定義
ここで、Val n という形をした数式に対する eval' の定義を発見できる
eval' (Val n) s = push n s
□

Recursive:
eval' (Add x y) s
eval (Add x y) :s   -- eval' の仕様
(eval x + eval y) :s -- eval を適用

-- 数式 x  y についての帰納法の仮定を使用
eval' x s' = eval x : s'
eval' y s' = eval y : s'

(eval x + eval y) :s
add (eval y : eval x : s)  -- `add (m : n : s) = n + m : s` と定義
add (eval y : (eval' x s)) -- x に対する仮定 (eval') x を先にスタックに積むのは、`加算`が`引数を左から右へ評価する`ことに対応している. (Pih251)
add (eval' y (eval' x s))  -- y に対するの仮定 (eval')
□

ここまで計算すると、eval' を定義することができる.
-}

-- *** 評価関数 ***

-- calc = 9
calc :: Int
calc = eval testExpr

eval :: Expr -> Int
-- old impl before `eval'` exists
-- eval :: Expr -> Int
-- eval (Val n) = n
-- eval (Add x y) = eval x + eval y

-- even old as well
-- eval e = head (eval' e [])

eval e = head $ exec (comp e) []

eval' :: Expr -> Stack -> Stack
-- これが成立することを仮定し、上記の論証を行って算出した結果で実装をする
-- eval' e s = eval e : s := ? に対する仮定

-- old impl before `eval''` exists
-- eval' (Val n) s = push n s
-- eval' (Add x y) s = add (eval' y (eval' x s))

eval' e = eval'' e id

comp :: Expr -> Code
comp e = comp' e HALT

-- push, add は仮定通りそのまま定義
push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = n + m : s

{-
Base:
eval'' e c s
eval'' (Val n) c s
c (eval' (Val n) s) -- eval'' の定義
c (push n s)        -- eval' の定義
□

Recursive:
eval'' e c s
eval'' (Add x y) c s
c (eval' (Add x y) s)           -- eval'' の定義
c (add (eval' y (eval' x s)))   -- eval' の定義
(c . add) (eval' y (eval' x s)) -- . を逆適用
eval'' y (c . add) (eval' x s)  -- y に対する仮定 (eval'')
eval'' x (eval'' y (c . add)) s -- x に対する仮定 (eval'')
□
-}

eval'' :: Expr -> Cont -> Cont
-- eval'' e c s = c (eval' e s) := ? に対する仮定
eval'' (Val n) c s = c (push n s)
eval'' (Add x y) c s = eval'' x (eval'' y (c . add)) s

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

-- *** 脱高階関数 ***

-- 必要な種類の継続を生成するための`コンビネーター`を定義
haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

exec :: Code -> Cont
-- old impl
-- exec HALT = haltC
-- exec (PUSH n c) = pushC n (exec c)
-- exec (ADD c) = addC (exec c)

exec HALT s = s
exec (PUSH n c) s = exec c (n : s)
exec (ADD c) (n : m : s) = exec c (n + m : s)