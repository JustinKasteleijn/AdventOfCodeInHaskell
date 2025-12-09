module Year2015.Day4
  ( run,
  )
where

import Benchmark
import Data.Hash.MD5 (Str (..), md5s)

solve :: String -> Int -> Int -> Int
solve base suffix n
  | replicate n '0' == take n hash = suffix
  | otherwise = solve base (suffix + 1) n
  where
    hash = md5s (Str $ base ++ show suffix)

-- ----------- Test Cases ------------
test1 :: Bool
test1 = solve1 "abcdef" == 609043

-- ----------- Solve ------------
solve1 :: String -> Int
solve1 str = solve str 0 5

solve2 :: String -> Int
solve2 str = solve str 0 6

run :: IO ()
run = do
  print $ "Testing example input: " ++ show test1

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 "iwrupvqb"
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ solve2 "iwrupvqb"
  print $ "Part 2: " ++ show res2
