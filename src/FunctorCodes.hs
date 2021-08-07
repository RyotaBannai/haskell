module FunctorCodes where

import Data.Char
import Data.List

-- reverse text
simple :: IO ()
simple = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

-- Like we can apply a function to something that's inside a Maybe box, we can apply a function to what's inside an `IO box`, only it has to go out into the real world to get something.

-- using fmap
-- fmap :: (a -> b) -> IO a -> IO b
simple' :: IO ()
simple' = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said " ++ line ++ " backwards!"

-- anc >> C-B-A
moreComplex :: IO ()
moreComplex = do
  line <- fmap (intersperse '-' . reverse . map toUpper) getLine
  putStrLn line