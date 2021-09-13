module Archive.PipeExpr where

{-
What is pipes/conduit trying to solve | <https://stackoverflow.com/questions/22742001/what-is-pipes-conduit-trying-to-solve>
conduit | <https://github.com/snoyberg/conduit
-}

import Control.Monad
import Pipes
import qualified Pipes.Prelude as P -- Pipes.Prelude already has 'stdinLn'
import System.IO

{-
As you connect components their types will change to reflect inputs and outputs that you've fused away.
You know that you're done connecting things when you get an `Effect`, meaning that you have handled all inputs and outputs.
You run this final `Effect` to begin streaming.
-}

-- Every monad transformer has a base monad. This time the base monad is 'IO'
-- Every monadic action has a return value. This action returns '()' when finished
stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF -- 'lift' an 'IO' action from the base monad
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn -- Loop

loop :: Effect IO ()
-- Read this like: `for str in stdinLn`
loop = for stdinLn $ \str -> do
  lift $ putStrLn str -- The body of the `for` loop

{-
`runEffect :: Monad m => Effect m r -> m r`
This is the real type signature of `runEffect`, which refuses to accept anything other than an `Effect`.
This ensures that we handle all inputs and outputs before streaming data.

for :: Monad m => Producer a m r -> (a -> Producer b m ()) -> Producer b m r

each :: Monad m => [a] -> Producer a m ()
each as = mapM_ yield as
-}

test :: IO ()
test = runEffect loop

-- convert list to Producer with `each`
test2 :: IO ()
test2 = runEffect $ for (each [1 .. 4]) (lift . print)

test2' :: IO ()
test2' = runEffect $ for (each (Just 1)) (lift . print)

{-
* Composability
-}
duplicate :: Monad m => a -> Producer a m ()
duplicate x = do
  yield x
  yield x

loop' :: Producer String IO ()
loop' = for P.stdinLn duplicate

test3 :: IO ()
test3 = runEffect $ for loop' (lift . putStrLn)

{-
for (for s f) g = for s (\x -> for (f x) g)
-- s :: Monad m =>      Producer a m ()  -- i.e. 'P.stdinLn'
-- f :: Monad m => a -> Producer b m ()  -- i.e. 'duplicate'
-- g :: Monad m => b -> Producer c m ()  -- i.e. '(lift . putStrLn)'

(f ~> g) x = for (f x) g

* Left Identity
yield ~> f = f
* Right Identity
f ~> yield = f

yield and (~>) form a Category, specifically the generator category,
where `(~>)` plays the role of the `composition operator` and `yield` is the `identity`

Notice that if we translate `the left identity law` to use for instead of (~>) we get:
for (yield x) f = f x

If we translate the `right identity law` to use for instead of (~>) we get:
for s yield = s

-}

-- Condensed version of test3
test4 :: IO ()
test4 = runEffect $ for P.stdinLn (duplicate ~> lift . putStrLn)