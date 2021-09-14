module Archive.PipeExpr where

{-
What is pipes/conduit trying to solve | <https://stackoverflow.com/questions/22742001/what-is-pipes-conduit-trying-to-solve>
conduit | <https://github.com/snoyberg/conduit>
Pipes tutorial | <https://hackage.haskell.org/package/pipes-4.1.0/docs/Pipes-Tutorial.html>
-}

-- Pipes.Prelude already has 'stdinLn' and 'stdoutLn'

import Control.Applicative ((<$)) -- (<$) modifies return values
import Control.Exception
import Control.Monad
import Data.Functor (($>))
import qualified GHC.IO.Exception as G
import Pipes
import qualified Pipes.Prelude as P
import System.IO
import Prelude hiding (take)

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

 for :: Monad m => Producer a m r -> (a -> Producer b    m ()) -> Producer b    m r
 -- Specialize 'b' to 'X'
 for :: Monad m => Producer a m r -> (a -> Producer X m ()) -> Producer X m r
 -- Producer X = Effect
 for :: Monad m => Producer a m r -> (a -> Effect        m ()) -> Effect        m r

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

* `~>` into operator for `Producer` to yield a value
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

test4' :: IO ()
test4' = runEffect $
  for P.stdinLn $ \str1 ->
    for (duplicate str1) $ \str2 ->
      lift $ putStrLn str2

-- Not working because `~>` can only simplify `\str -> Proxy` lambda
-- test4'' :: IO ()
-- test4'' = runEffect $ P.stdinLn ~> (duplicate ~> lift . putStrLn)

{-
next :: Monad m => Producer a m r -> m (Either r (a, Producer a m r))

Think of next as pattern matching on the head of the Producer.
This `Either` returns a Left if the Producer is done or
it returns a `Right` containing the next value, a, along with the remainder of the Producer.

ttry :: forall e a. Exception e => IO a -> IO (Either e a)

Similar to `catch` , but returns an `Either` result which is (`Right a`) if `no exception of type e was raised`, or
(`Left ex`) if an exception of type e was raised and its value is ex.
If any other type of exception is raised than it will be propogated up to the next enclosing exception handler.

try a = catch (Right `liftM` a) (return . Left)

`await` is the dual of yield: we suspend our `Consumer` until we receive a new value.
If nobody provides a value (which is possible) then `await` never returns. You can think of `await` as having the following type:

await :: Monad m => Consumer a m a

* `>~` feed operator to feed value to `Consumer`

Feed action -> Consumer to feed -> Returns new Effect respectively.
(>~) :: Monad m => Effect m b -> Consumer b m c -> Effect m c
also permits the follwoing type:
(>~) :: Monad m => Consumer a m b -> Consumer b m c -> Consumer a m c

Also, (>~) has an identity, which is `await`
* Left identity
await >~ f = f
* Right Identity
f >~ await = f

-}

stdoutLn :: Consumer String IO ()
stdoutLn = do
  str <- await
  x <- lift $ try $ putStrLn str
  case x of
    Left e@G.IOError {G.ioe_type = t} ->
      lift $ unless (t == G.ResourceVanished) $ throwIO e -- terminates gracefully when receiving a broken pipe error
    Right () -> stdoutLn

{-
(draw ~> consumer) loops over (consumer (stdoutLn)),
substituting each `await` in (consumer) with (draw (lift getLine := Effect m b))
-}
test5 :: IO ()
test5 = runEffect $ lift getLine >~ stdoutLn

doubleUp :: Monad m => Consumer String m String
doubleUp = do
  str1 <- await
  str2 <- await
  return (str1 ++ str2)

test6 :: IO String
test6 = runEffect $ lift getLine >~ doubleUp >~ doubleUp

{-
* (>->) pipe operator
(>->) :: Monad m => Producer a m r -> Consumer a m r -> Effect m r

(>->) is "pull-based" meaning that control flow begins at `the most downstream component` (i.e. stdoutLn in the above example).
Any time a component `awaits` a value it `blocks` and transfers control upstream and every time a component `yields` a value it blocks and restores control back downstream, satisfying the await.
So in the below example, (>->) matches every `await` from `stdoutLn` with a `yield` from `stdinLn`.

`Streaming` stops when either `stdinLn` terminates (i.e. end of input) or `stdoutLn` terminates (i.e. broken pipe).
This is why (>->) requires that both the `Producer` and `Consumer` share `the same type of return value`:
whichever one terminates first provides the return value for `the entire Effect`.

The "pipeline" not only echoes `standard input` to `standard output`,
but also handles both `end of input` and `broken pipe errors`

You can use `Pipes` to transform `Producers`, `Consumers`, or even other `Pipes` using the same (>->) operator:

(>->) :: Monad m => Producer a m r -> Pipe   a b m r -> Producer b m r
(>->) :: Monad m => Pipe   a b m r -> Consumer b m r -> Consumer a m r
(>->) :: Monad m => Pipe   a b m r -> Pipe   b c m r -> Pipe   a c m r

-}

test7 :: IO ()
test7 = runEffect $ P.stdinLn >-> P.stdoutLn

{-
| <https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Functor.html#v:-60--36->

($>) := 左側が成功系であれば、右側の値と中身を入れかえる
成功系: Just, Right
λ Just "aaa" $> "Ok"     # Just "Ok"
λ Nothing $> "OK"        # Nothing
λ Right "aaa" $> "OK"    # Right "OK"
λ Left "aaa" $> "OK"     # Left "aaa"

(<$) := Flipped version of ($>).
右側が成功系であれば、左側の値で中身を入れ替える.
λ "Ok" <$ Just "aaa"      # Just "Ok"
λ "Ok" <$ Nothing         # Nothing
λ "OK" <$ Right "aaa"     # Right "OK"
λ "OK" <$ Left "aaa"      # Left "aaa"
-}

{-
perl options <https://www.tohoho-web.com/perl/app2.htm>
`-e` run perl script

$ ./haskell                         -- test\ntest\n<Ctr+D>End of input!\n
$ ./haskell | perl -e 'close STDIN' -- aaa\nBroken pipe!\n
$ ./haskell | perl -e "print 5 * 3" -- 15\n15\n
-}

test8 :: IO ()
test8 = do
  hSetBuffering stdout NoBuffering
  str <- runEffect $ ("End of input!" <$ P.stdinLn) >-> ("Broken pipe!" <$ P.stdoutLn)
  hPutStrLn stderr str

-- Pipe `await's a's` `yield's a's` IO ()
take :: Int -> Pipe a a IO ()
take n = do
  replicateM_ n $ do
    x <- await -- `await` a value of type `a` -- Pipe の中でどちらも使う時は、Producer-Consumer 間の yield-await の順が逆になる（またはどちらかが先に来ても問題ないのか.）
    yield x -- `yield` a value of type `a`
  lift $ putStrLn "You shall not pass!"

maxInput :: Int -> Producer String IO ()
maxInput n = P.stdinLn >-> take n

test9 :: IO ()
test9 = runEffect $ maxInput 3 >-> P.stdoutLn

maxOutput :: Int -> Consumer String IO ()
maxOutput n = take n >-> P.stdoutLn

test10 :: IO ()
test10 = runEffect $ P.stdinLn >-> maxOutput 3

test11 :: IO ()
test11 = runEffect $ P.stdinLn >-> take 3 >-> P.stdoutLn