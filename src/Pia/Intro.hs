-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE ViewPatterns #-}

module Pia.Intro where

import Control.Arrow
import qualified Control.Category as Cat
-- import Control.Arrow ((&&&), (***), (>>>))
import Control.Monad ((<=<))

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

testSF :: [Integer]
testSF = runSF (arr (+ 1) >>> arr (flip (-) 1)) [1 .. 5] -- or subtract 1

{-
λ :t (\x -> (x :))
(\x -> (x :)) :: a -> [a] -> [a]

λ runSF (delay 0) [1..5] # [0,1,2,3,4,5] -- added 0 in the beginning.
(x:) [] => [x]
(x:) [1, 2] => [x, 1, 2]
It's a function which takes a list (whose items are the same type as x) and outputs the same list with x added at the start.
-}
delay :: b -> SF b b
delay x = SF (x :) -- [a] -> [a]