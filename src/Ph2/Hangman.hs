module Ph2.Hangman where

import Ph2.Common (getCh)

-- hang man
hangman :: IO ()
hangman = do
  putStrLn "Think of a word:"
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

-- 何が打ち込まれたか隠すためにダッシュ記号 '-' エコーバックする
sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- sgetLine
      return (x : xs) -- `xs` can be null

-- 二人目の player に推測を入力させ、それを秘密の単語と一致するまで繰り返すというゲームのメインループを実現させる.
play :: String -> IO ()
play word = do
  putStr "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it!"
    else do
      putStrLn (match word guess)
      play word

match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]