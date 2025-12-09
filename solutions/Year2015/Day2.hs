module Year2015.Day2
  ( run,
  )
where

import Benchmark
import Control.DeepSeq (NFData (..))
import Data.List (foldl', sort)
import Parser

data RectangularCuboid = RC
  { length' :: {-# UNPACK #-} !Int,
    width' :: {-# UNPACK #-} !Int,
    height' :: {-# UNPACK #-} !Int
  }
  deriving (Show)

instance NFData RectangularCuboid where
  rnf (RC l w h) = l `seq` w `seq` h `seq` ()

mkRectangularCuboid :: Int -> Int -> Int -> RectangularCuboid
mkRectangularCuboid = RC

parseRectangularCuboid :: Parser RectangularCuboid
parseRectangularCuboid = do
  [x, y, z] <- sepBy1 int (char 'x')
  return $ mkRectangularCuboid x y z

parseRectangularCuboids :: Parser [RectangularCuboid]
parseRectangularCuboids = lines1 parseRectangularCuboid

area :: RectangularCuboid -> Int
area (RC l w h) = 2 * (l * w + l * h + w * h)

volume :: RectangularCuboid -> Int
volume (RC l w h) = l * w * h

wrappingPaper :: RectangularCuboid -> Int
wrappingPaper rc@(RC l w h) =
  area rc + minimum [l * w, l * h, w * h]

ribbon :: RectangularCuboid -> Int
ribbon rc@(RC l w h) =
  let (a, b) = case sort [l, w, h] of
        [x, y, _] -> (x, y)
        _ -> error "Impossible: should always have 3 elements"
   in 2 * (a + b) + volume rc

test1 :: Bool
test1 =
  wrappingPaper (RC 2 3 4) == 58
    && wrappingPaper (RC 1 1 10) == 43

test2 :: Bool
test2 =
  ribbon (RC 2 3 4) == 34
    && ribbon (RC 1 1 10) == 14

solve :: (RectangularCuboid -> Int) -> [RectangularCuboid] -> Int
solve f = foldl' (\acc cube -> acc + f cube) 0

solve1 :: [RectangularCuboid] -> Int
solve1 = solve wrappingPaper

solve2 :: [RectangularCuboid] -> Int
solve2 = solve ribbon

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day2.txt"
  print $ "Testing example input Part 1: " ++ show test1
  print $ "Testing example input Part 1: " ++ show test2

  putChar '\n'

  parsed <- timeIt "Parsing " $ unwrapParser parseRectangularCuboids input

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ solve2 parsed
  print $ "Part 2: " ++ show res2
