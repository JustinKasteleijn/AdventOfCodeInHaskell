{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Year2025.Day1
  ( run,
  )
where

import Benchmark
import Control.DeepSeq (NFData (..))
import Data.List (scanl')
import GHC.Generics (Generic (..))
import Parser
  ( Parser (..),
    char,
    choice,
    int,
    lines1,
    unwrapParser,
  )

data Rotation
  = L Int
  | R Int
  deriving (Show, Generic, NFData)

parseRotation :: Parser Rotation
parseRotation =
  choice
    [ L <$> (char 'L' *> int),
      R <$> (char 'R' *> int)
    ]

parseRotations :: Parser [Rotation]
parseRotations = lines1 parseRotation

newtype Dial = Dial Int
  deriving (Eq, Show)

mkDial :: Int -> Dial
mkDial n = Dial (n `mod` 100)

unwrap :: Dial -> Int
unwrap (Dial n) = n

initDial :: Dial
initDial = Dial 50

rotate :: Rotation -> Dial -> Dial
rotate (L n) dial = mkDial $ unwrap dial - n
rotate (R n) dial = mkDial $ unwrap dial + n

-- ----------- Test Cases ------------
test1 :: Bool
test1 =
  let parsed = unwrapParser parseRotations testInput
   in solve1 parsed == 3

-- ----------- Solve ------------
solve1 :: [Rotation] -> Int
solve1 =
  length
    . filter ((== 0) . unwrap)
    . scanl' (flip rotate) initDial

solve2 :: String -> Int
solve2 = undefined

run :: IO ()
run = do
  input <- readFile "solutions/Year2025/inputs/day1.txt"
  print $ "Testing example input: " ++ show test1
  parsed <- timeIt "Parsing: " $ unwrapParser parseRotations input

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

-- res2 <- timeIt "Part 2" $ solve2 input
-- print $ "Part 2: " ++ show res2

testInput :: String
testInput =
  unlines
    [ "L68",
      "L30",
      "R48",
      "L5",
      "R60",
      "L55",
      "L1",
      "L99",
      "R14",
      "L82"
    ]
