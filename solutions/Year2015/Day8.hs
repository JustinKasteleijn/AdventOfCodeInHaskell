module Year2015.Day8
  ( run,
  )
where

import Benchmark (timeIt)
import qualified Data.ByteString as BS
import Parser
import Types.IntegerTypes (U8)

-- ----------- Parse ------------
parseWords :: Parser BS.ByteString [BS.ByteString]
parseWords = map BS.pack <$> lines1 parseLine

-- ----------- Logic ------------
memLength :: BS.ByteString -> Int
memLength bs
  | BS.length bs < 2 = 0
  | otherwise = go (BS.drop 1 (BS.init bs)) 0
  where
    backslash, quote, xChar :: U8
    backslash = 92 --
    quote = 34 -- '"'
    xChar = 120 -- 'x'
    go :: BS.ByteString -> Int -> Int
    go s acc
      | BS.null s = acc
      | BS.head s == backslash =
          case BS.uncons (BS.tail s) of
            Just (c, rest)
              | c == backslash || c == quote -> go rest (acc + 1)
              | c == xChar -> go (BS.drop 2 rest) (acc + 1)
              | otherwise -> go rest (acc + 1)
            Nothing -> acc + 1
      | otherwise = go (BS.tail s) (acc + 1)

totalDiff :: [BS.ByteString] -> Int
totalDiff = sum . map diff
  where
    diff s = BS.length s - memLength s

encodeLiteral :: BS.ByteString -> BS.ByteString
encodeLiteral bs = BS.concat [BS.singleton quote, encodeBody bs, BS.singleton quote]
  where
    backslash, quote :: U8
    backslash = 92
    quote = 34

    encodeBody :: BS.ByteString -> BS.ByteString
    encodeBody = BS.concatMap escapeChar

    escapeChar :: U8 -> BS.ByteString
    escapeChar c
      | c == backslash = BS.pack [backslash, backslash] -- "\\"
      | c == quote = BS.pack [backslash, quote] -- "\""
      | otherwise = BS.singleton c

encodedDiff :: [BS.ByteString] -> Int
encodedDiff = (+ (-2)) . sum . map diff
  where
    diff s = BS.length (encodeLiteral s) - BS.length s

-- ----------- Test Cases ------------

-- ----------- Solve ------------

run :: IO ()
run = do
  input <- BS.readFile "solutions/Year2015/inputs/day8.txt"
  parsed <- timeIt "Parsing" (unwrapParser parseWords input)

  putChar '\n'

  res1 <- timeIt "Part 1" $ totalDiff parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ encodedDiff parsed
  print $ "Part 2: " ++ show res2
