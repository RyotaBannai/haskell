module Ph2.CountdownProb where

-- remove module declaration when compile `ghc CountdownProb.hs`

-- These ops are valid as long as the result is positive integer.
data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- add, mul は常に正の整数を生成.
-- sub, div は場合によっては、負の整数、分数を生成してしまうため不正.
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub a b = a - b > 0
valid Mul _ _ = True
valid Div a b = a `mod` b == 0

apply :: Op -> Int -> Int -> Int
apply Add a b = a + b
apply Sub a b = a - b
apply Mul a b = a * b
apply Div a b = a `div` b

-- 数式の型 := 整数の値 | 演算子を二つの式に適用することを表現する構成子
data Expr = Val Int | App Op Expr Expr

-- *** Show Expr := 式を読める形式へ変換し、IO で print できるように(文字列形式に)する ***

-- show (App Add (Val 1) (App Mul (Val 2) (Val 3))) # "1+(2*3)"
instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where
      brak (Val n) = show n
      brak e = "(" ++ show e ++ ")" -- App Mul (Val 2) (Val 3) -> "(2*3)"

-- *** values := 式中の整数だけを取り出し、配列で返す. ***

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

-- eval (App Add (Val 1) (App Mul (Val 2) (Val 3))) # [7] singleton list = success
-- eval (App Add (Val 1) (Sub Mul (Val 2) (Val 3))) # []  empty list     = failure
-- 失敗するかもしれない eval を Maybe 型を使って表現しても良い.
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- subs [1,2] # [[],[2],[1],[1,2]]
-- subs [1,2,3] # [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]] -- 一番後ろの数値が先に base pattern に到達するため先にくる.
subs :: [a] -> [[a]]
subs [] = [[]]
subs (x : xs) = let yss = subs xs in yss ++ map (x :) yss

-- interleave 1 [2,3,4] # [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y :) (interleave x ys)

{-
1 [2,3,4] = [1,2,3,4] : map (2:) (?) # <- ? := [[1,3,4],[3,1,4],[3,4,1]] -> [[1,2,3,4],[2,1,3,4],[2,3,1,4],[2,3,4,1]]
1 [3,4]   = [1,3,4]   : map (3:) (?) # <- ? := [[1,4],[4,1]]             -> [[1,3,4],[3,1,4],[3,4,1]]
1 [4]     = [1,4]     : map (4:) (?) # <- ? := [[1]]                     -> [[1,4],[4,1]]
1 []      = [[1]]
-}

perms :: [a] -> [[a]]
-- perms [] = [[]]
-- perms (x : xs) = concatMap (interleave x) (perms xs)
perms = foldr (concatMap . interleave) [[]]

{-
1:[2,3] = concatMap (interleave 1) ([[2,3],[3,2]]) -- [[[1,2,3],[2,1,3],[2,3,1]],[[1,3,2],[3,1,2],[3,2,1]]] -> concatenated [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
2:[3] = concatMap (interleave 2) ([[3]])     -- [[[2,3],[3,2]]] -> concatenated [[2,3],[3,2]]
3:[] = concatMap (interleave 3) ([[]])       -- [[[3]]]         -> concatenated [[3]]
[] = [[]]
-}

-- *** choices := produces all possible patterns from given set, including all subsets and ordering differences. ***

-- chocies [1,2,3] # [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]
choices :: [a] -> [[a]]
choices = concatMap perms . subs

-- *** colusion := checks if the given expression result in the given result ***

-- e = App Mul (App Add(Val 1) (Val 50)) (App Sub(Val 25) (Val 10))
-- solution e [1,3,7,10,25,50] 765 # True
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

-- *** split := あるリストを空でない２つのリストに分割 ***

-- split [1..4] # [([1], [2, 3, 4]), ([1, 2], [3, 4]), ([1, 2, 3], [4])]
split :: [a] -> [([a], [a])]
split [] = [] -- return null when nulls
split [_] = [] -- base
split (x : xs) = ([x], xs) : [(x : ls, rs) | (ls, rs) <- split xs] -- recursive logic
{-
split [1..4]
split 1: [2,3,4] = ([1],[2,3,4]) : [(1:ls,rs) <- (ls,rs) <- split [2,3,4]]  # returns [([1],[2,3,4]),([1,2],[3,4]),([1,2,3],[4])]
split 2: [3,4] = ([2],[3,4]) : [(2:ls,rs) <- (ls,rs) <- split [3,4]]        # returns [([2],[3,4]),([2,3],[4])]
split 3: [4] = ([3],[4]) : [(3:ls,rs) <- (ls,rs) <- split [4]]              # returns [([3],[4])]
split [4] = []                                                              # returns []
-}

-- *** exprs := generates all possible combinations with numbers and opeartions. ***

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

-- Add when compile as a single file
-- main :: IO ()
-- main = print (solutions [1, 3, 7, 10, 25, 50] 765)