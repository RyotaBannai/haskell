module Lyhfgg.MyIOCodes where

import Control.Monad
import Data.Char

hw :: IO ()
hw = putStrLn "hello world"

hw' :: IO ()
hw' = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

in' :: IO ()
in' = do
  line <- getLine
  if null line
    then return () -- this is a `I/O action` -- return is a box as well. you can wrap String like return "haha", which is `IO String` type
    else do
      putStrLn $ reverseWords line
      in'

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

-- `return` is sort of `the opposite to <-`.
-- While `return` takes a value and wraps it up in a box,
-- `<-` takes a box (and performs it) and takes the value out of it, binding it to a name.
re :: IO ()
re = do
  a <- return "hehe" -- replace with `let a = "hehe"`
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

-- read Char one by one
gc :: IO ()
gc = do
  c <- getChar
  if c /= ' '
    then do
      putChar c
      gc
    else return ()

-- improved
gc' :: IO ()
gc' = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    gc'

form :: IO [()]
form = do
  colors <-
    forM
      [1 .. 4]
      ( \a -> do
          putStrLn $ "Which color do you associate wiht hte number " ++ show a ++ "?"
          getLine
      )
  putStrLn "The colors that you associate with 1, 2, 3, and 4 are: "
  mapM putStrLn colors