{-# LANGUAGE LambdaCase #-}

module Ph2.Common where

import Control.Monad (filterM, forM_, guard, unless, void)
import Control.Monad.ListM (spanM, takeWhileM)
import Data.Char (toUpper)
import System.IO (hSetEcho, stdin)

-- import Control.Monad.Loops

newline :: IO ()
newline = putChar '\n'

getCh :: IO Char
getCh = do
  hSetEcho stdin False -- echo back 機能を False (入力した文字を repl に表示しない).
  x <- getChar
  hSetEcho stdin True
  return x

readLine :: IO String
readLine = readLineCore ""

readLineCore :: String -> IO String
readLineCore xs = do
  x <- getCh
  case x of
    '\n' -> return xs
    '\DEL' ->
      if null xs
        then readLineCore ""
        else do
          -- putChar '\b'
          putStr "\b \b"
          readLineCore $ init xs
    _ -> do
      putChar x
      readLineCore (xs ++ [x])

readLine' :: IO String
readLine' = dropWhile (== '\DEL') <$> readLineCore'

readLineCore' :: IO String
readLineCore' = do
  x <- getCh
  if x == '\n'
    then return []
    else do
      putOrDel x
      xs <- readLineCore'
      return $ delete (x : xs)
  where
    putOrDel x
      | x == '\DEL' = putStr "\b \b"
      | otherwise = putChar x
    delete (x : '\DEL' : xs)
      | x /= '\DEL' = xs
    delete xs = xs

-- the same as prelude function `getLine`
getLine' :: IO String
getLine' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine'
      return (x : xs)

fizzbuzz :: IO ()
fizzbuzz = do
  print $
    -- instead of `\ n -> case n of`
    flip map [1 .. 100] $ \case
      n'
        | n' `mod` 15 == 0 -> "FizzBuzz"
        | n' `mod` 5 == 0 -> "Buzz"
        | n' `mod` 3 == 0 -> "Fizz"
        | otherwise -> show n'

-- process [pure 1, pure 3]
process :: [IO Int] -> IO [Int]
process [] = return []
process xs = sequence xs

sample :: IO [Char]
sample = fmap (map toUpper) getLine -- liftM

{-
mapM f is equivalent to sequence . map f.

sequenceA :: forall (t :: * -> *) (f :: * -> *) a.
(Traversable t, Applicative f) =>
t (f a) -> f (t a)

sequence :: forall (t :: * -> *) (m :: * -> *) a.
(Traversable t, Monad m) =>
t (m a) -> m (t a)

sequence_ := 返される値に関心がない場合
-}

wrapIO :: Show b => b -> IO b
wrapIO x = print x >> return x

result :: IO [Integer]
result = takeWhile (< 4) <$> (return [0 .. 5] :: IO [Integer])

{-
First wrap [0..5] with IO via return (with type hint)
Then extract values inside monad from each `takeWhile (< 4)` and `[0..5]` via `<$>`
-}

result2 :: IO ()
result2 = mapM_ wrapIO (takeWhile (< 4) [0 .. 5])

-- `forM_` is `mapM_` with flipped arguments
result0 :: IO ()
result0 = forM_ (takeWhile (< 4) [0 .. 5]) wrapIO

-- result3 :: IO [Integer]
result3 :: IO [IO Integer]
result3 = takeWhileM (fmap (< 4)) (map wrapIO [0 .. 5]) -- [IO b]

-- https://stackoverflow.com/questions/1133800/haskell-monadic-takewhile

-- sequenceUntil (<4) $ map wrapIO [1..10]
-- You can't use `guard` because Monad IO doesn't impl `mzero`, clearly there is no way to imple empty value for IO
sequenceTakeUntil :: (Foldable t, Monad m) => (a -> Bool) -> t (m a) -> m [a]
sequenceTakeUntil p = foldr (myLiftM2 (:) []) (pure [])
  where
    myLiftM2 f z m1 m2 = do
      x1 <- m1
      if p x1
        then do f x1 <$> m2 -- if prediate success return acumm `with` new item
        else return z -- if predicate fails return null and ends foldr

-- Discard the results
sequenceTakeUntil_ :: (Foldable t, Monad f) => (a -> Bool) -> t (f a) -> f ()
sequenceTakeUntil_ p xs = void (sequenceTakeUntil p xs) -- sequenceUntil p xs >> return ()

sequenceDropUntil :: (Foldable t, Monad m) => (a -> Bool) -> t (m a) -> m [a]
sequenceDropUntil p = foldr (myLiftM2 (:) []) (pure [])
  where
    myLiftM2 f z m1 m2 = do
      x1 <- m1
      if p x1
        then m2
        else f x1 <$> m2

untilM :: Monad m => (a -> Bool) -> [m a] -> m ()
untilM _ [] = pure ()
untilM p (x : xs) = do
  y <- x
  unless (p y) $ untilM p xs

result4 :: IO ()
result4 = do
  let as = map wrapIO [1 .. 10]
  ys <- sequenceTakeUntil (< 4) as
  print ys

result5 :: IO ()
result5 = do
  let as = map wrapIO [1 ..]
  sequenceTakeUntil_ (< 4) as

-- result6 $ map wrapIO [1..10]
result6 :: (Monad m, Ord a, Num a) => [m a] -> m ()
result6 xs = do
  untilM (>= 4) xs

-- (expri [1]) 2 # 1
expri :: [b] -> Integer -> [b]
expri = filterM (const (<= 4))

-- function returns m a (in this case `m Bool`)
expri2 :: [[Integer]]
expri2 = filterM (\x -> [x <= 4]) [1, 2, 3]
