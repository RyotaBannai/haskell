module Pih.TautologyProver where

import qualified Data.List as List
import Pih.Common (Assoc, aFindFirst, int2bit)

-- *** Tautology(恒真式) := always returns True. 命題が恒真式であるかを決定するには、命題に含まれる変数に対して可能な置換を全て考える必要がある. ***

-- 命題が恒真式であるかを判断する関数を定義する第一歩は、命題を表す型 Prop の宣言である.
-- 命題を構成する五種類の要素について、それぞれ構成子を定義する:
data Prop
  = Const Bool
  | Var Char
  | Not Prop -- ¬
  | And Prop Prop -- ∧
  | Xor Prop Prop --  ⊻ or ⊕ (Unicode: U+2295)
  | Imply Prop Prop -- ⇒
  | Iff Prop Prop -- ⇔

-- A ∧ ¬A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A ∧ B) ⇒ A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A ⇒ (A ∧ B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A ∧ (A ⇒ B)) ⇒ B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- 同値 - 反対称律
-- ((A ⇒ B) ∧ (B ⇒ A)) ⇒ (A ⇔ B)
p5 :: Prop
p5 = Imply (And (Imply (Var 'A') (Var 'B')) (Imply (Var 'B') (Var 'A'))) (Iff (Var 'A') (Var 'B'))

-- 同値 - 推移律
-- ((A ⇔ B) ∧ (B ⇔ C)) ⇒ (A ⇔ C)
p6 :: Prop
p6 = Imply (And (Iff (Var 'A') (Var 'B')) (Iff (Var 'B') (Var 'C'))) (Iff (Var 'A') (Var 'C'))

-- (A ⇔ B) ⇔ ¬(A ⊻ B)
p7 :: Prop
p7 = Iff (Iff (Var 'A') (Var 'B')) (Not (Xor (Var 'A') (Var 'B')))

-- 置換表 := 変数名 <-> 真理値
type Subst = Assoc Char Bool

-- [('A', False), ('B', True)]

-- *** eval := s には ('A', False) などの Subst が in ***

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = aFindFirst x s -- converts to pure Bool value here.
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Xor p q) = eval s p /= eval s q -- 排他的論理和
eval s (Imply p q) = eval s p <= eval s q -- 論理包含 p 102 の真理値表を確認.
eval s (Iff p q) = eval s p == eval s q -- 同値

-- 命題にある全ての変数をリストとして返す関数を定義(Prop を連続的に分割し String として返却する)
-- i.g. vars p2 = ['A', 'B', 'A'] or "ABA"
vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Xor p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Iff p q) = vars p ++ vars q

-- *** bools := n 数の Bool の全く見合わせを生成. i.g. n=3 の場合 8 通りの False/True の組み合わせを作成. ***

-- i.g. int2bit 1 # [1], int2bit 3 # [1,1]
-- reverse := int2bit は bits の並びを反対として捉えているため(i.g. `1 == 1000`) reverse で一般的な並びに戻す.
bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . fill n . int2bit) range
  where
    range = [0 .. (2 ^ n) -1]
    fill n bs = take n (bs ++ repeat 0) -- 足りない `length bs - n` 分を 0 で補う.
    conv 0 = False
    conv 1 = True

-- n -1 の結果にそれぞれ False/True を追加しているというパターンを定式化したバージョン
bools' :: Int -> [[Bool]]
bools' 0 = [[]]
bools' n = let bss = bools' (n -1) in map (False :) bss ++ map (True :) bss

-- *** substs := 命題にある変数の数分パターンを作成する ***

substs :: Prop -> [Subst]
substs p = let vs = List.nub (vars p) in map (zip vs) (bools (length vs))

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
