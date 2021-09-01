module Pih.Common where

import System.IO (hSetEcho, stdin)

-- import Control.Monad.Loops
type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

newline :: IO ()
newline = putChar '\n'

getCh :: IO Char
getCh = do
  hSetEcho stdin False -- echo back 機能を False (入力した文字を repl に表示しない).
  x <- getChar
  hSetEcho stdin True
  return x

readLine' :: IO String
readLine' = readLineCore' ""

readLineCore' :: String -> IO String
readLineCore' xs = do
  x <- getCh
  case x of
    '\n' -> return xs
    '\DEL' ->
      if null xs
        then readLineCore' ""
        else do
          -- putChar '\b'
          putStr "\b \b"
          readLineCore' $ init xs
    _ -> do
      putChar x
      readLineCore' (xs ++ [x])

readLine :: IO String
readLine = dropWhile (== '\DEL') <$> readLineCore

readLineCore :: IO String
readLineCore = do
  x <- getCh
  if x == '\n'
    then return []
    else do
      putOrDel x
      xs <- readLineCore
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