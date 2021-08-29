{-# LANGUAGE LambdaCase #-}

module Ph2.Common where

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
readLine' = readLineCore'

readLineCore' :: IO String
readLineCore' = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- readLineCore'
      if x == '\DEL'
        then putStr "\b \b"
        else pure ()
      return $ delete (x : xs)

delete :: [Char] -> [Char]
delete [] = []
delete ('\DEL' : '\DEL' : xs) = '\DEL' : '\DEL' : xs
delete (_ : '\DEL' : xs) = xs
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