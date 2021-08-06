module Main where

import qualified Baby
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import System.Random

-- main always has a type signate of main :: IO something
main :: IO ()
main = toTry `catch` betterHandler

toTry :: IO ()
toTry = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

betterHandler :: IOError -> IO ()
betterHandler e
  -- isDoesNotExistError and ioError are from System.IO.Error
  -- ioe https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3
  | isDoesNotExistError e = case ioeGetFileName e of
    Just path -> putStrLn $ "The file (" ++ path ++ ") doesn't exist"
    Nothing -> putStrLn "Whoops! File doesn't exist at unknon location."
  | otherwise = ioError e

{-
We re-throw the exception that was passed by the handler with the `ioError` function. It has a type of `ioError :: IOException -> IO a`, so it takes an `IOError` and produces an `I/O action` that will throw it. The `I/O action` has `a` type of `IO a`, because it never actually yields a result, so it can act as `IO anything`.
-}

handler :: IOError -> IO ()
handler e = putStrLn "The file doesn't exist"

checkLineLength :: IO ()
checkLineLength = do
  (fileName : _) <- getArgs
  fileExists <- doesFileExist fileName --  We can't just use doesFileExist in an if expression directly.
  if fileExists
    then do
      contents <- readFile fileName
      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
    else do putStrLn "The file doesn't exist"

guessNumber :: IO ()
guessNumber = do
  gen <- getStdGen
  askForNumber gen

askForNumber :: StdGen -> IO ()
askForNumber gen = do
  let (randomNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
  putStrLn "Which number in the range from 1 to 10 am I thinking of?"
  numberString <- getLine
  let parsedList = reads numberString :: [(Int, String)]
  -- unless (null numberString)
  -- if doesn't satisfied `when` clause, `returns ()`, which is empty I/O
  when (not $ null parsedList) $ do
    let [(number, _)] = parsedList
    if number == randomNumber
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randomNumber
    askForNumber newGen

getRandStr :: IO ()
getRandStr = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)

{-
:run with arguments
`cabal run --help`
$ cabal run haskell -O2 delete "resources/todo.txt" 1
-}

gate :: IO ()
gate = do
  (command : args) <- getArgs
  let (Just action) = lookup command dispatch
  action args

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", addTodo), ("view", view), ("delete", deleteTodo)]

view :: [String] -> IO ()
view [todoFilePath] = do
  contents <- readFile todoFilePath
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0 ..] todoTasks
  putStrLn "These are your TO-DO items:"
  putStr $ unlines numberedTasks
view _ = return ()

addTodo :: [String] -> IO ()
addTodo [todoFilePath, todo] = appendFile todoFilePath (todo ++ "\n")
addTodo _ = return ()

deleteTodo :: [String] -> IO ()
deleteTodo [todoFilePath, numberString] = do
  handle <- openFile todoFilePath ReadMode
  (tempName, tempHandle) <- openTempFile "." "temp"
  contents <- hGetContents handle
  let number = read numberString
      todoTasks = lines contents
      newTodoItems = delete (todoTasks !! number) todoTasks
  hPutStr tempHandle $ unlines newTodoItems
  hClose handle
  hClose tempHandle
  removeFile todoFilePath
  renameFile tempName todoFilePath
deleteTodo _ = return ()

-- todoFilePath :: [Char]
-- todoFilePath = "resources/todo.txt"
deleteTodo' :: String -> IO ()
deleteTodo' todoFilePath = do
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
