module Expr.MonadTrans where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.IO

transMaybeIO :: MaybeT IO (Int -> MaybeT IO Int)
transMaybeIO = return (\x -> return (x + 3) :: MaybeT IO Int) :: MaybeT IO (Int -> MaybeT IO Int)

transMaybeIO' :: MaybeT IO (Int -> Int -> Int)
transMaybeIO' = return (*) :: MaybeT IO (Int -> Int -> Int)

transformedVal :: a -> MaybeT IO a
transformedVal = return

transformedVal' :: MaybeT IO (Int -> Int)
transformedVal' = return (* 3) :: MaybeT IO (Int -> Int)

res :: MaybeT IO (Int -> Int)
res = liftA2 (<*>) transMaybeIO' transformedVal'

--
--
a :: MaybeT IO String
a = return "aa"

b :: MaybeT IO [Char -> Char]
b = return [id, id]

-- https://blog.ssanj.net/posts/2014-08-10-boosting-liftA2.html
-- match expected type ‘MaybeT IO [Char -> Char]’
-- "aaaa"
re :: MaybeT IO String
re = liftA2 (<*>) b a

a' :: MaybeT IO [Int]
a' = return [2, 3]

b' :: MaybeT IO [Int -> Int]
b' = return [(+ 3), (+ 4)]

-- Just [5,6,6,7]
re' :: MaybeT IO [Int]
re' = liftA2 (<*>) b' a'

--
--

-- https://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
-- liftIO :: IO a -> MaybeT IO a
-- (m :: * -> *) a. MonadIO m => IO a -> m a
mgreet :: MaybeT IO ()
mgreet = do
  liftIO $ putStr "What is your name?" -- MaybeT IO ()
  n <- liftIO getLine -- MaybeT IO String
  liftIO $ putStrLn $ "Hello, " ++ n -- MaybeT IO ()

-- re0 = liftA2 (<*>) (Just (* 5)) (IO (Just 6) :: MaybeT IO Int)

re1 :: [Maybe Integer]
re1 = getZipList $ liftA2 (<*>) (ZipList [Just (* 5), Just (* 4)]) (ZipList [Just 6, Just 6])