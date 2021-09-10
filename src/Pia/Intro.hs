{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE ViewPatterns #-}

module Pia.Intro where

import Control.Arrow
import qualified Control.Category as Cat
import Control.Monad ((<=<))

-- import Debug.Trace

-- λ count "hero" "i'm a hero"
count :: String -> String -> Int
count w = length . filter (== w) . words

{-
readFile :: String -> IO String
print :: Show a => a -> IO ()

Doesn't work because of types are different. readFile return `IO Monad`, but words acccepts `String`
count' w = print . length . filter (== w) . words . readFile
-}
-- fmap/ liftM := change function (a -> b) to function (m a -> m b)
count' :: String -> FilePath -> IO ()
count' w = print <=< fmap (length . filter (== w) . words) . readFile

{-
We call this type Kleisli because functions with this type are arrows in the Kleisli category of the monad m. 圏の射 (arrow)

type Kleisli m a b = a -> m b
readFile :: Kleisli IO String String -- こうなるように Kleisli でラップする
print :: Show a => Kleisli IO a ()

(>>>) :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
(f >>> g) a = do b <- f a
                g b

arr :: Monad m => (a->b) -> Kleisli m a b
arr f = return . f

-- REF: https://tnomura9.exblog.jp/18517156/
-}

readFile' :: Kleisli IO FilePath String
readFile' = Kleisli readFile

print' :: (Show a) => Kleisli IO a ()
print' = Kleisli print

printFile :: Kleisli IO FilePath ()
printFile = readFile' >>> print'

-- Kleisli 型の中の IO モナドを取り出す場合は、runKleisli
testPrintFile :: IO ()
testPrintFile = runKleisli printFile "resources/girlfriends.txt"

-- λ runKleisli (count'' "teapot") "resources/haiku.txt"
count'' :: String -> Kleisli IO FilePath ()
count'' w = readFile' >>> arr words >>> arr (filter (== w)) >>> arr length >>> print'

count''' :: String -> Kleisli IO FilePath ()
count''' w = readFile' >>> arr (length . filter (== w) . words) >>> print'

-- `.` compose function can be replaced with a sequencing operator for Arrow `>>>` and the data flow is reversed
count'''' :: String -> Kleisli IO FilePath ()
count'''' w = readFile' >>> arr (words >>> filter (== w) >>> length) >>> print'

-- *** stream functions ***

{-
まず、Control.Category モジュールを qualified import する。
Arrow クラスでは、Prelude の . 演算子と id 演算子が隠蔽され、Control.Category.Category クラスの多相関数 . 演算子と id 演算子に置き換えられている。
SF 型のデータも Control.Category.Category クラスのインスタンスとして宣言されていないと、Arrow クラスのインスタンスにすることができない。
-}

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Cat.Category SF where
  id = SF Cat.id
  SF f . SF g = SF (f Cat.. g)

instance Arrow SF where
  arr f = SF (map f)
  first (SF f) = SF (unzip >>> first f >>> uncurry zip)
  SF f &&& SF g = SF (f &&& g >>> uncurry zip) -- λ uncurry zip ([1,2], [3,4]) # [(1,3),(2,4)]

instance ArrowChoice SF where
  left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
    where
      combine (Left y : xs) (z : zs) = Left z : combine xs zs
      combine (Right y : xs) zs = Right y : combine xs zs
      combine [] zs = []

{-
The ~ in the definition of stream indicates Haskell’s lazy pattern matching:
it delays matching the argument of stream against the pattern (x:xs) until the bound variables x and xs are actually used.
Thus stream returns an infinite list without evaluating its argument
Semantically, stream ⊥=⊥:⊥:⊥: . . .. As a result, provided as is defined, then so is zip as (stream ⊥) — it is a list of pairs with undefined second components. Since neither f nor unzip needs these components to deliver a defined result, we now obtain defined values for bs and cs in the second approximation, and indeed the limit of the approximations is the result we expect. The reader who finds this argument difficult should work out the sequence of approximations in the call `runSF (loop (arr swap)) [1,2,3]` — it is quite instructive to do so.

λ runSF (loop (arr swap)) [1,2,3]          # [1,2,3]
λ runSF (loop (arr id)) [1,2,3]            # [1,2,3]
λ runSF (loop (first (arr (+1)))) [1,2,3]  # [2,3,4]
λ runSF (loop (second (arr (+1)))) [1,2,3] # [1,2,3]

-}
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

instance ArrowLoop SF where
  -- loop :: SF (b, d) (c, d) -> SF b c
  -- old
  -- loop (SF f) = SF $ \as ->
  --   let (bs, cs) = unzip (f (zip as cs)) in bs

  loop (SF f) = SF $ \as -> let (bs, cs) = unzip (f (zip as (stream cs))) in bs
    where
      stream ~(x : xs) = x : stream xs

{-
λ unzip [(1,2)]         # ([1],[2])
λ zip [1] [2]           # [(1,2)]
λ uncurry zip ([1],[2]) # [(1,2)]
-}

testSF :: [Integer]
testSF = runSF (arr (+ 1) >>> arr (flip (-) 1)) [1 .. 5] -- or subtract 1

{-
λ :t (\x -> (x :))
(\x -> (x :)) :: a -> [a] -> [a]

λ runSF (delay 0) [1..5] # [0,1,2,3,4,5] -- added 0 in the beginning.
λ runSF (delay' 0) [1..5] # [0,1,2,3,4] -- added 0 and keep the length of inp list
(x:) [] => [x]
(x:) [1, 2] => [x, 1, 2]
It's a function which takes a list (whose items are the same type as x) and outputs the same list with x added at the start.
-}
delay :: b -> SF b b
delay x = SF (x :) -- [a] -> [a]

delay' :: b -> SF b b
delay' x = SF (init . (x :))

{-
delay :: (Show b) => b -> SF b b
delay x = SF (\xs -> traceShow xs x : xs) -- [a] -> [a]
-}

-- If we could make a pair of their outputs,
-- then we could supply that to arr (uncurry (+)) to sum the components.
{-
intance Arror (->) where
  (f &&& g) a = (f a, g a)

instance Monad m =< Arrorw (Kleisli m) where
  Kleisli f &&& Kleisli g = Kleisli $
    \a -> do b <- f a
             c <- g a
             return (b, c)
-}
addA :: (Arrow arr, Num c) => arr a c -> arr a c -> arr a c
addA f g = f &&& g >>> arr (uncurry (+))

-- λ  runKleisli testAddA "resources/haiku.txt" # read the same file twice and combine them together.
testAddA :: Kleisli IO FilePath [Char]
testAddA = readFile' &&& readFile' >>> arr (uncurry (++))

-- λ runSF pairPred [1..5] # [(1,0),(2,1),(3,2),(4,3),(5,4)]
pairPred :: SF Integer (Integer, Integer)
pairPred = arr id &&& delay 0

{-
Impl of first of each:

instance Arrow (->) where
  ...
  first f (a,c) = (f a,c)

instance Monad m => Arrow (Kleisli m) where
  ...
  first (Kleisli f) = Kleisli (
    \(a,c) -> do b <- f a
    return (b,c))

We can define `***` op in terms of `first` and `second` by first defining second:

second :: Arrow arr => arr a b -> arr (c,a) (c,b)
  second f = arr swap >>> first f >>> arr swap
    where swap (x,y) = (y,x)

f *** g = first f >>> second g

This clearly shows that `f` takes first elem of inp and `g` takes second elem of inp
-}

-- λ runKleisli testAst (1,2) # (2,4)
testAst :: Kleisli IO (Int, Int) (Int, Int)
testAst = arr (* 2) *** arr (+ 2)

-- testFirst := [(2,1),(4,2),(6,3)]
testFirst :: [(Integer, Integer)]
testFirst = runSF (first (arr (* 2))) [(1, 1), (2, 2), (3, 3)]

{-
Arrows and conditions:

ifte :: Arrow arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte p f g

ifte p f g = p &&& arr id >>> f ||| g
First of all, we can easiy factor out `p` by computing its result before the choice:
we can do so with p &&& arr id, which outputs a pair of the boolean(the result of `p`) and the original input(with identity function). `f ||| g` chooses between f and g on the basis of the first component of the pait in its input, passing the second component on `f` or `g`
But we can do better than this with `Either`

class Arrow arr => ArrowChoice arr where
  (|||) :: arr a c -> arr b c -> arr (Either a b) c
-}

listcase :: [a] -> Either () (a, [a])
listcase [] = Left ()
listcase (x : xs) = Right (x, xs)

-- λ mapA (+1) [1..5] # [2,3,4,5,6]
-- arr listcase := Predicate in this case, which return either `Left` or `Right`.
mapA :: ArrowChoice arr => arr a b -> arr [a] [b]
mapA f = arr listcase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))

{-
To check `a rising edge` (which is the first rising point in the wave)
by `delay` the original signals.
[F, T, T, F, F] -- original
[F, F, T, T, F] -- delayed
The second will be the rising edge(or, a pulse).
-}
edge :: SF Bool Bool
edge = arr id &&& delay False >>> arr detect
  where
    detect (a, b) = a && not b

{-
λ addOneToTuple(False,[]) # ([],[1])
λ f (f (f ([],[])))       # ([1,1],[1,1,1]), f = addOneToTuple

first は前の引数だから無視して、 second が新しいためそれを元に関数を構築
-}
addOneToTuple :: Num a1 => (a2, [a1]) -> ([a1], [a1])
addOneToTuple (a, b) = (b, 1 : b)

{-
instance ArrowLoop (->) where
  -- 引数として与えた b に何らかの処理を加えて、その結果を返す.
  loop f b = let (c,d) = f (b,d) in c

λ take 3 $ loop addOneToTuple [] # [1,1,1]

g f b = let (c,d) = f (b,d) in c
λ take 10 $ g f []
-}

-- ステップごと一つ前のステップの値を fst に新しい値を snd に入れたペア -> ペアの関数を loop の引数にすればいい
fact :: Integer -> Integer
fact = loop (\(n, g) -> (g n, \n -> if n == 0 then 1 else n * g (n - 1)))