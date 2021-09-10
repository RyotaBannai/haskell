module Pia.Unsafe where

import System.IO.Unsafe

unsafePrint :: (Show a) => a -> a
unsafePrint xs = unsafePerformIO $ do
  print xs
  return xs

testAppPrint :: String -> String
testAppPrint xs = let xs' = unsafePrint xs in xs' ++ " after printed."