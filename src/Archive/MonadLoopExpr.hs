module Archive.MonadLoopExpr where

import Control.Monad
import Control.Monad.Loops

{-
Module reference | <https://pursuit.purescript.org/packages/purescript-monad-loops/0.5.0/docs/Control.Monad.Loops>
Tutorial | <https://conscientiousprogrammer.com/blog/2015/12/11/24-days-of-hackage-2015-day-11-monad-loops-avoiding-writing-recursive-functions-by-refactoring/>
-}

-- *** Login ***

logIn :: IO ()
logIn = do
  putStrLn "% Enter password:"
  go
  putStrLn "$ Congrautulations!"
  where
    go = do
      guess <- getLine
      when (guess /= "password") $ -- use `when` when using `else return ()`
        do
          putStrLn "% Wrong password!\nTry again:"
          go

{-
whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ p f = go
    where go = do
            x <- p
            if x
                then f >> go
                else return ()
-}

logIn' :: IO ()
logIn' = do
  putStrLn "% Enter password:"
  whileM_ -- takes `Predicate` and `body`
    ( do
        guess <- getLine
        return (guess /= "password")
    )
    ( do
        putStrLn "% Wrong password!\nTry again:"
    )
  putStrLn "$ Congrautulations!"

-- Monadic way
logIn'' :: IO ()
logIn'' = do
  putStrLn "% Enter password:"
  whileM_ (getLine >>= \guess -> return (guess /= "password")) $
    do
      putStrLn "% Wrong password!\nTry again:"
  putStrLn "$ Congrautulations!"

-- Functor way
logIn''' :: IO ()
logIn''' = do
  putStrLn "% Enter password:"
  whileM_ (fmap (/= "password") getLine) $
    do
      putStrLn "% Wrong password!\nTry again:"
  putStrLn "$ Congrautulations!"

-- Applicative way
logIn'''' :: IO ()
logIn'''' = do
  putStrLn "% Enter password:"
  whileM_ ((/= "password") <$> getLine) $
    do
      putStrLn "% Wrong password!\nTry again:"
  putStrLn "$ Congrautulations!"

-- *** readLines ***

readLinesUntilQuit :: IO [String]
readLinesUntilQuit = do
  line <- getLine
  if line /= "quit"
    then do
      restOfLines <- readLinesUntilQuit
      return (line : restOfLines)
    else return []

readLinesUntilQuit' :: IO [String]
readLinesUntilQuit' = unfoldM maybeReadLine
  where
    maybeReadLine :: IO (Maybe String)
    maybeReadLine = do
      line <- getLine
      return
        ( if line /= "quit"
            then Just line
            else Nothing
        )

readLinesUntilQuit'' :: IO [String]
readLinesUntilQuit'' = unfoldM (notQuit <$> getLine)
  where
    notQuit :: String -> Maybe String
    notQuit line = if line /= "quit" then Just line else Nothing