{-# LANGUAGE LambdaCase #-}

module Ph2.Common where

import Control.Monad.ListM
import Data.Char (toUpper)
import System.IO (hSetEcho, stdin)

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

-- p :: [IO Int] -> IO Bool
-- p as = do
--   taken <- takeWhileM (>>= return . (<= 0)) as
--   return (length taken >= length as - 1)

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
