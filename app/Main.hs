module Main where

import qualified Baby

-- main always has a type signate of main :: IO something
main :: IO ()
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine -- bind its result value to `name`
  putStrLn ("Hey" ++ name ++ ", you rock!") -- in a do block, the last action cannot be bound to a name, but `()`

-- main = print (Baby.doubleMe 2)
