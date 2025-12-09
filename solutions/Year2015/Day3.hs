module Year2015.Day3
  ( run,
  )
where

import Benchmark
import Data.List (foldl')
import qualified Data.Set as S

data V2 a = V2 !a !a
  deriving (Show, Eq, Ord)

origin :: V2 Int
origin = V2 0 0
{-# INLINE origin #-}

data FoldState = FS
  { visited :: !(S.Set (V2 Int)),
    _currentPos :: !(V2 Int)
  }

deliver :: FoldState -> Char -> FoldState
deliver (FS acc (V2 x y)) c =
  let (dx, dy) = case c of
        '^' -> (0, 1)
        'v' -> (0, -1)
        '>' -> (1, 0)
        '<' -> (-1, 0)
        _ -> (0, 0)
      pos = V2 (x + dx) (y + dy)
   in FS (S.insert pos acc) pos

visitedHouses :: String -> S.Set (V2 Int)
visitedHouses =
  visited
    . foldl' deliver (FS (S.fromList [origin]) origin)

splitEvenOdd :: [a] -> ([a], [a])
splitEvenOdd xs =
  let indexed = zip [0 :: Int ..] xs
      even' = [x | (i, x) <- indexed, even i]
      odd' = [y | (j, y) <- indexed, odd j]
   in (even', odd')

solve1 :: String -> Int
solve1 = length . visitedHouses

solve2 :: String -> Int
solve2 input =
  let (xs, ys) = splitEvenOdd input
      santa = visitedHouses xs
      roboSanta = visitedHouses ys
   in length $ S.union santa roboSanta

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day3.txt"

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 input
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ solve2 input
  print $ "Part 2: " ++ show res2
