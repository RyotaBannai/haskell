{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Archive.AttoparsecExpr where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
-- HTTP protocol to perform downloads

import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Lazy.Char8 (toChunks)
import qualified Data.Char as C
import Data.Either
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid hiding (Product)
import Data.String (IsString (fromString))
import Data.Time
import Data.Word
import Network.HTTP.Conduit
import System.IO

{-
Parsing Log Files in Haskell | <https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec>
Conduit Http | <https://hackage.haskell.org/package/http-conduit-2.3.8/docs/Network-HTTP-Conduit.html>
Parse Datetime format | <https://stackoverflow.com/questions/4174372/haskell-date-parsing-and-formatting>
Type の違い | <https://sites.google.com/site/toriaezuzakki/haskell#TOC-Word8-Char->
-}

-- The Word8 type represents 8-bit unsigned integer values.
data IP = IP Word8 Word8 Word8 Word8 deriving (Show)

data Product = Mouse | Keyboard | Monitor | Speakers deriving (Eq, Enum, Show)

data LogEntry = LogEntry
  { entryTime :: LocalTime, -- `yyyy-MM-dd hh:mm:ss`i.g. 2013-06-29 11:16:23
    entryIP :: IP,
    entryProduct :: Product,
    source :: Source
  }
  deriving (Show)

data Source = Internet | Friend | NoAnswer deriving (Show)

data File = URL String | Local FilePath deriving (Show)

type Log = [LogEntry]

type Sales = [(Product, Int)]

instance Eq LogEntry where
  le1 == le2 = entryTime le1 == entryTime le2

instance Ord LogEntry where
  le1 `compare` le2 = entryTime le1 `compare` entryTime le2

logFile :: FilePath
logFile = "resources/sellings.log"

logFile2 :: FilePath
logFile2 = "resources/sellings_french.log"

logFiles :: [File]
logFiles =
  [ Local logFile,
    Local logFile2,
    URL "http://daniel-diaz.github.io/misc/sellings3.log"
  ]

csvFile :: FilePath
csvFile = "resources/log.csv"

getFile :: File -> IO B.ByteString
getFile (URL str) = mconcat . toChunks <$> simpleHttp str
getFile (Local fp) = B.readFile fp

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  IP d1 d2 d3 <$> decimal

timeParser :: Parser LocalTime
timeParser = do
  -- parser format `2013-06-30 14:33:29`
  y <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d <- count 2 digit
  char ' '
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  char ':'
  s <- count 2 digit
  return $
    LocalTime
      { localDay = fromGregorian (read y) (read mm) (read d),
        localTimeOfDay = TimeOfDay (read h) (read m) (read s)
      }

productParser :: Parser Product
productParser =
  (string "mouse" >> return Mouse)
    <|> (string "keyboard" >> return Keyboard)
    <|> (string "monitor" >> return Monitor)
    <|> (string "speakers" >> return Speakers)

logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  char ' '
  ip <- parseIP
  char ' '
  p <- productParser
  return (LogEntry t ip p NoAnswer)

{-
The `endOfLine` parser succeeds only when the remaining input starts with an end of line.
The `<*` combinator applies the parser from the left, then the parser from the right, and then returns the result of the first parser.
-}

logParser :: Parser Log
logParser = some $ logEntryParser <* endOfLine -- some := One or more. many := Zero or more, thus Jure [] when Zero

testParser :: IO ()
testParser = B.readFile logFile >>= print . parseOnly logParser

productFromID :: Int -> Product
productFromID n = toEnum (n -1)

productToID :: Product -> Int
productToID p = fromEnum p + 1

-- A parser of products would accept a single digit
productParser2 :: Parser Product
productParser2 = productFromID . read . (: []) <$> digit -- checkout digit type definition(`digit :: Parser Char`). also, `read` reads number from list of Char

timeParser2 :: Parser LocalTime
timeParser2 = do
  -- parser format `29/06/2013 20:30:13`
  d <- count 2 digit
  char '/'
  mm <- count 2 digit
  char '/'
  y <- count 4 digit
  char ' '
  h <- count 2 digit
  char ':'
  m <- count 2 digit
  char ':'
  s <- count 2 digit
  return $
    LocalTime
      { localDay = fromGregorian (read y) (read mm) (read d),
        localTimeOfDay = TimeOfDay (read h) (read m) (read s)
      }

sourceParser :: Parser Source
sourceParser =
  (string "internet" >> return Internet)
    <|> (string "friend" >> return Friend)
    <|> (string "noanswer" >> return NoAnswer)

logEntryParser2 :: Parser LogEntry
logEntryParser2 = do
  ip <- parseIP
  char ' '
  t <- timeParser2
  char ' '
  p <- productParser2
  char ' '
  LogEntry t ip p <$> sourceParser

logParser2 :: Parser Log
logParser2 = some $ logEntryParser2 <* endOfLine

testParser2 :: IO ()
testParser2 = B.readFile logFile2 >>= print . parseOnly logParser2

rowParser :: Parser LogEntry
rowParser = do
  let spaceSkip = many $ satisfy $ inClass [' ', '\t'] -- skips space char b/a CSV sep char(',')
      sepParser = spaceSkip >> char sepChar >> spaceSkip
  spaceSkip -- 前後の空白文字列を除去
  t <- timeParser
  sepParser
  ip <- parseIP
  sepParser
  p <- productParser
  sepParser
  s <- sourceParser
  spaceSkip
  return $ LogEntry t ip p s

csvParser :: Parser Log
csvParser = many $ rowParser <* endOfLine

{-
* Use `renderLog` and `Data.ByteString.Char8.writeFile` to write a CSV table using your log information. However, if you are using a character set different from `ASCII` or `ISO-8859-15`, you should consider using the type `Text` instead of `ByteString`. Almost the only change you have to do is to change the import of `Data.Attoparsec.Char8` to `Data.Attoparsec.Text` (both modules export similar interfaces and are interchangeable) and adapt the types of the renderer.
-}

-- merge sort
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x : xs) (y : ys) =
  if x <= y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys

mergeLog :: IO (Either String Log)
mergeLog = do
  file1 <- B.readFile logFile
  file2 <- B.readFile logFile2
  return $ do
    xs <- parseOnly logParser file1
    ys <- parseOnly logParser2 file2
    return $ merge xs ys

salesOf :: Product -> Sales -> Int
salesOf p xs = fromMaybe 0 $ lookup p xs -- lookup key assocs

addSale :: Product -> Sales -> Sales
addSale p [] = [(p, 1)]
addSale p ((x, n) : xs) = if p == x then (x, n + 1) : xs else (x, n) : addSale p xs

mostSold :: Sales -> Maybe (Product, Int)
mostSold [] = Nothing
mostSold xs = Just $ maximumBy (\x y -> snd x `compare` snd y) xs

sales :: Log -> Sales
sales = foldr (addSale . entryProduct) []

sepChar :: Char
sepChar = ','

nl :: Char
nl = '\n'

renderIP :: IP -> B.ByteString
renderIP (IP a b c d) =
  fromString (show a)
    <> "."
    <> fromString (show b)
    <> "."
    <> fromString (show c)
    <> "."
    <> fromString (show d)

renderEntry :: LogEntry -> B.ByteString
renderEntry le =
  fromString (show $ entryTime le)
    <> BC.singleton sepChar
    <> renderIP (entryIP le)
    <> BC.singleton sepChar
    <> fromString (fmap C.toLower $ show $ entryProduct le)
    <> BC.singleton sepChar
    <> fromString (fmap C.toLower $ show $ source le)

renderLog :: Log -> B.ByteString
renderLog = foldMap $ \le -> renderEntry le <> BC.singleton nl

testMain :: IO ()
testMain =
  mergeLog >>= \case
    Left err -> putStrLn $ "A parsing error was found:" ++ err
    Right log -> mapM_ print log

testMain2 :: IO ()
testMain2 =
  mergeLog >>= \case
    Left err -> putStrLn $ "A parsing error was found:" ++ err
    Right log -> case mostSold (sales log) of
      Nothing -> putStrLn "We didn't sell anything yet."
      Just (p, n) -> putStrLn $ "The product with more sales is " ++ show p ++ " with " ++ show n ++ " sales."

testMain3 :: IO ()
testMain3 =
  mergeLog >>= \case
    Left err -> putStrLn $ "A parsing error was found:" ++ err
    Right log -> BC.putStr . renderLog $ log

testMain4 :: IO ()
testMain4 = do
  file <- B.readFile csvFile
  case parseOnly csvParser file of
    Left err -> putStrLn $ "Error while parsing CSV file:" ++ err
    Right log -> mapM_ print log

testMain5 :: IO ()
testMain5 = do
  files <- mapM getFile logFiles
  let logs = rights $ fmap (parseOnly logParser) files
      mergedLog = foldr merge [] logs
  BC.putStrLn $ renderLog mergedLog

testMain6 :: IO ()
testMain6 = do
  files <- mapM getFile logFiles
  let logs = zipWith (curry (second contParse)) logFiles files
  forM_ logs $ \(f, maybeLog) -> do
    print f
    BC.putStrLn $ case maybeLog of
      Nothing -> fromString "Error while parsing"
      Just log -> renderLog log
  where
    contParse cont =
      let parse :: Parser Log -> Maybe Log
          parse p = eitherToMaybe $ parseOnly p cont
       in parse logParser <|> parse logParser2 <|> Nothing

{-
λ testMain6
Local "resources/sellings.log"
2013-06-29 11:16:23,124.67.34.60,keyboard,noanswer
2013-06-29 11:32:12,212.141.23.67,mouse,noanswer
2013-06-29 11:33:08,212.141.23.67,monitor,noanswer
2013-06-29 12:12:34,125.80.32.31,speakers,noanswer
2013-06-29 12:51:50,101.40.50.62,keyboard,noanswer

Local "resources/sellings_french.log"
2013-06-29 15:32:23,154.41.32.99,speakers,internet
2013-06-29 16:56:45,76.125.44.33,monitor,noanswer
2013-06-29 18:44:29,123.45.67.89,speakers,friend
2013-06-29 19:01:09,100.23.32.41,mouse,internet

URL "http://daniel-diaz.github.io/misc/sellings3.log"
"Error while parsing"
-}

-- test = Right (IP 131 45 68 123)
test :: IO ()
test = print $ parseOnly parseIP "131.45.68.123"

test2 :: IO ()
test2 = print $ parseOnly timeParser "2013-06-30 14:33:29"

test3 :: IO ()
test3 = do
  print $ parseOnly productParser "mouse"
  print $ parseOnly productParser "mouze"
  print $ parseOnly productParser "monitor"
  print $ parseOnly productParser "keyboard"

test4 :: IO ()
test4 = print $ parseOnly logEntryParser "2013-06-29 11:16:23 124.67.34.60 keyboard"

test5 :: IO ()
test5 = do
  withFile logFile ReadMode $
    \handle -> do
      contents <- hGetContents handle
      print . parseOnly logParser $ fromString contents

test6 :: IO ()
test6 = do
  print $ productFromID 1
  print $ productFromID 3
  print $ productToID Keyboard
  print $ productToID $ productFromID 4

-- test7 = Right Speakers
test7 :: IO ()
test7 = print $ parseOnly productParser2 "4"

test8 :: IO ()
test8 = print $ parseOnly timeParser2 "29/06/2013 15:32:23"

test9 :: IO ()
test9 = print $ parseOnly logEntryParser2 "54.41.32.99 29/06/2013 15:32:23 4 internet"

test10 :: IO ()
test10 = print $ merge [1, 3, 5, 7] [2, 4, 6, 8]
