module Main where
  
import Control.Monad
import Data.Char

import qualified Baby

-- main always has a type signate of main :: IO something
main :: IO ()
main = interact respondPalindromes

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines
  where isPalindrome xs = xs == reverse xs

intra''' = interact $ unlines . filter ((<10) . length) . lines -- even better impl.

intra'' = interact shortLinesOnly -- better impl. `interact` passes contentsto function(String -> String) and wraps the result String in I/O action.

intra = do 
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly contents = 
  let allLines = lines contents
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
  in result


capsLocker = do 
  contents <- getContents
  putStr (map toUpper contents)

{-
`getContents` is I/O lazy, it won't try to read the whole content at once and store it into memory before printing out the capslocked version. 
Rather, it will print out the capslocked version as it reads it, because it will `only read a line from the input when it really needs to`.
-}


fore = forever $ do 
  putStr "Give me some input: "
  l <- getLine
  putStr $ map toUpper l

gl = do
  putStrLn "Hello, what's your name?"
  name <- getLine -- bind its result value to `name`
  putStrLn ("Hey" ++ name ++ ", you rock!") -- in a do block, the last action cannot be bound to a name, but `()`
