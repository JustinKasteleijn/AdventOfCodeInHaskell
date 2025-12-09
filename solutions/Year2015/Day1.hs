module Year2015.Day1
  ( run,
  )
where

import Benchmark
import Data.List (elemIndex, foldl', scanl')
import Data.Maybe (fromJust)

calc :: Char -> (Int -> Int)
calc '(' = (+ 1)
calc ')' = (+ (-1))
calc _ = (+ 0)

test1 :: Bool
test1 =
  solve1 "(())" == 0
    && solve1 "()()" == 0

solve1 :: String -> Int
solve1 = foldl' (flip calc) 0

solve2 :: String -> Int
solve2 =
  fromJust
    . elemIndex (-1)
    . scanl' (flip calc) 0

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day1.txt"
  print $ "Testing example input: " ++ show test1

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 input
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ solve2 input
  print $ "Part 2: " ++ show res2
