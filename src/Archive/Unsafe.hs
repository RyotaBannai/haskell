module Archive.Unsafe where

import Control.Monad.ST
import Data.Foldable
import Data.STRef
import System.IO.Unsafe

-- import Control.Monad.Trans.State (modify)

unsafePrint :: (Show a) => a -> a
unsafePrint xs = unsafePerformIO $ do
  print xs
  return xs

testAppPrint :: String -> String
testAppPrint xs = let xs' = unsafePrint xs in xs' ++ " after printed."

{-
| <https://en.wikibooks.org/wiki/Haskell/Mutable_objects>
| <https://xtech.nikkei.com/it/article/COLUMN/20070109/258229/>

At first, that is a shocking type signature. If ST involves mutability, how come we can simply extract a values from the monad?
The answer lies in the `forall s.` part of the type. Having a `forall s.` enclosed within the type of an argument amounts to telling the type checker "`s` could be anything. Don't make any assumptions about it". Not making any assumptions, however, means that `s` cannot be matched with anything else − even with the `s` from another invocation of `runST`
For these reasons following codes are errors:

λ ref = runST $ newSTRef (4 :: Int)
λ x = runST $ readSTRef =<< runST (newSTRef (4 :: Int))
-}

-- The fact that it destructively updates its accumulator `n` is a mere implementation detail,
-- and there is no way information about `n` could leak other than through the final result.
-- λ sumST [1..5] # 15
-- sumST :: (Num a, Foldable t) => t a -> a

sumST :: (Num a, Foldable t) => t a -> a
sumST xs = runST $ do
  n <- newSTRef 0 -- n :: STRef RealWorld a
  for_ xs $ \x -> -- for_ is traverse_ with its arguments flipped
    modifySTRef n (+ x) -- modifySTRef (STRef s a) (a -> a) -- `s` is just an artificial marker.
  readSTRef n

-- `stToIO` Converting ST to IO (runST converts ST to `a`)