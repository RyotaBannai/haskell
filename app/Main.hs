module Main where

import qualified Baby
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO

-- main always has a type signate of main :: IO something
main :: IO ()
main = todoList

todoFilePath :: [Char]
todoFilePath = "resources/todo.txt"

todoList :: IO ()
todoList = do
  handle <- openFile todoFilePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp" -- create temp file at current dir
  contents <- hGetContents handle
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
  putStrLn "Which one do you want to delete?"
  numberString <- getLine
  let number = read numberString -- bind "1" to 1
      newTodoItems = delete (todoTasks !! number) todoTasks -- delete := deletes first occurance in list
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile todoFilePath -- takes a filePath not handle.
  renameFile tempName todoFilePath

blockBuffering :: IO ()
blockBuffering = do
  withFile
    "some_bianry.txt"
    ReadMode
    ( \handle -> do
        hSetBuffering handle $ BlockBuffering (Just 2048) -- doen't read line by line but reads the whole file in chuncks of 2048 bytes.
        contents <- hGetContents handle
        putStr contents
    )

readfwf :: IO ()
readfwf = do
  withFile
    "resources/girlfriends.txt"
    ReadMode
    -- withFile's lambda takes a `handle` and return a I/O action
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

readf :: IO ()
readf = do
  -- several I/O actions glued together with a do block
  handle <- openFile "resources/girlfriends.txt" ReadMode -- if we don't bind that read to a handle, we wouldn't be able to do anything with the file. So in our case, we bound the `handle` to handle
  contents <- hGetContents handle -- doesn't read it all and stores all. but reads a line as it needs.
  putStr contents
  hClose handle

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines
  where
    isPalindrome xs = xs == reverse xs

intra''' :: IO ()
intra''' = interact $ unlines . filter ((< 10) . length) . lines -- even better impl.

intra'' :: IO ()
intra'' = interact shortLinesOnly -- better impl. `interact` passes contentsto function(String -> String) and wraps the result String in I/O action.

intra :: IO ()
intra = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly contents =
  let allLines = lines contents
      shortLines = filter (\line -> length line < 10) allLines
      result = unlines shortLines
   in result

capsLocker :: IO ()
capsLocker = do
  contents <- getContents
  putStr (map toUpper contents)

{-
`getContents` is I/O lazy, it won't try to read the whole content at once and store it into memory before printing out the capslocked version.
Rather, it will print out the capslocked version as it reads it, because it will `only read a line from the input when it really needs to`.
-}

fore :: IO b
fore = forever $ do
  putStr "Give me some input: "
  l <- getLine
  putStr $ map toUpper l

gl :: IO ()
gl = do
  putStrLn "Hello, what's your name?"
  name <- getLine -- bind its result value to `name`
  putStrLn ("Hey" ++ name ++ ", you rock!") -- in a do block, the last action cannot be bound to a name, but `()`
