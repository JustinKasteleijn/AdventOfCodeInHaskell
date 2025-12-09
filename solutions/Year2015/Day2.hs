module Year2015.Day2
  ( run,
  )
where

import Data.List (foldl', sort)
import Parser

data RectangularCuboid = RC
  { length' :: !Int,
    width' :: !Int,
    height' :: !Int
  }
  deriving (Show)

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

solve1 :: String -> Int
solve1 input =
  let parsed = unwrapParser parseRectangularCuboids input
   in foldl' (\acc cube -> acc + wrappingPaper cube) 0 parsed

solve2 :: String -> Int
solve2 input =
  let parsed = unwrapParser parseRectangularCuboids input
   in foldl' (\acc cube -> acc + ribbon cube) 0 parsed

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day2.txt"
  print $ "Testing example input Part 1: " ++ show test1
  print $ "Testing example input Part 1: " ++ show test2

  print $ "Part 1: " ++ show (solve1 input)
  print $ "Part 2: " ++ show (solve2 input)
