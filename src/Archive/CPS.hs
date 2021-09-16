module Archive.CPS where

{-
CPS(Continuation Passing Style)

Continuation Monad :=「関数待受状態」のコンテナ
-}
infixr 5 =:

(=:) :: a -> (a -> b) -> b
x =: f = f x

add :: Int -> Int -> (Int -> r) -> r
add x y k = k (x + y)

square :: Int -> (Int -> r) -> r
square x k = k (x * x)

squareSquare :: Int -> (Int -> r) -> r
squareSquare n = square =: square n

-- pythagras := 平方和
pythagoras :: Int -> Int -> (Int -> r) -> r
pythagoras n m = (\x -> add x =: square m) =: square n

test :: Int
test = (* 10) =: add 1 2

test2 :: Int
test2 = (* 10) =: square 2

{-
squareSquare 2 (* 10)
(square 2 square) (* 10) -- 展開
square (2 * 2) (* 10)
square 4 (* 10) -- 展開
(* 10) (4 * 4)
(* 10) 16
160
-}
test3 :: Int
test3 = (* 10) =: squareSquare 2

test4 :: Int
test4 = (* 10) =: pythagoras 2 3

{-
* Cont Monad を自作

* `runCont` で取り出したいものは `関数を喰う関数` である 「関数受付状態」
-}
newtype Cont r a = Cont {runCont :: (a -> r) -> r}

-- 関数　square は「関数受付状態」
s3 :: Cont r Int
s3 = Cont (square 3)

test5 :: Int
test5 = (* 10) =: runCont s3

-- https://stackoverflow.com/questions/59122852/how-to-write-functor-instance-of-continuation-monad
instance Functor (Cont r) where
  fmap f (Cont k2) = Cont (\k1 -> k2 (k1 . f))

instance Applicative (Cont r) where
  pure x = Cont (\k -> k x)

-- g <*> f = Cont (\k -> runCont f (\x -> runCont (x) k))
{-
* Remember : (>>=) に渡したい関数 f は non Monad の引数を受付て、Monad を返却する関数
-}
instance Monad (Cont r) where
  return = pure
  m >>= f = Cont (\k -> (\x -> k =: runCont (f x)) =: runCont m)

-- return 3 :: Cont r Int
-- test6 = 9
test6 :: IO ()
test6 = print =: runCont (return 3 >>= \x -> Cont (square x)) -- s3 3
{-
print =: runCont (return 3 >>= \x -> Cont (square x))
print =: runCont (return 3 >>= \x -> Cont (square x))
print =: runCont (Cont (\k' -> k' 3) >>= \x -> Cont (square x))
print =: runCont (Cont (\k -> (\x' -> k =: square x'))) =: runCont (Cont (\k' -> k' 3))) -- 展開
print =: runCont (Cont (\k -> (\x' -> k =: square x'))) =: runCont (Cont (\k' -> k' 3))) -- 展開

print =: runCont (Cont (\k -> (\k -> (\x' -> k =: square x'))) =: (runCont (Cont (\k' -> k' 3))))
print =: runCont (Cont (\k -> (\k -> (\x' -> k =: square x'))) =: (\k' -> k' 3))
print =: (\k -> (\x' -> k =: square x'))) =: (\k' -> k' 3)
(\x' -> print =: square x') =: (\k' -> k' 3)
(\x' -> square x' print) =: (\k' -> k' 3)
(\x' -> square x' print) 3
square 3 print
-}

expr :: (Int -> r) -> r
expr = \k -> k =: (\x -> square x) =: (\k' -> k' 3)

-- square 3 == (\x -> runCont (f x)) =: runCont m -- ? square 3 は「状態受付状態」であるため、関数を受付けるようにする
-- square 3 == \k -> k =: (\x -> runCont (f x)) =: runCont m -- ? 受取った引数は、最終計算結果「square 3」 に喰わせる
-- ここで欲しいのは、Cont (square 3) == m >>= f なので, Cont で包む
-- m >>= f = Cont (\k -> k =: (\x -> runCont (f x)) =: runCont m)

squareCont :: Num a => a -> Cont r a
squareCont x = Cont (\k -> k (x * x))

addCont :: Num a => a -> a -> Cont r a
addCont x y = Cont (\k -> k (x + y))

pytagoras2 :: Num b => b -> b -> Cont r b
pytagoras2 n m = do
  x <- squareCont n
  y <- squareCont m
  addCont x y

-- test7 = 13
test7 :: IO ()
test7 = print =: runCont (pytagoras2 2 3)

-- test7 = 16
test7' :: Integer
test7' = (+ 3) =: runCont (pytagoras2 2 3)