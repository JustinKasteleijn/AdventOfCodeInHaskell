module Year2015.Day5
  ( run,
  )
where

import Benchmark
import Data.List (group, isInfixOf, tails)
import Parser

type Rule = String -> Bool

parseWords :: Parser [String]
parseWords = lines1 alpha1

threeVowels :: Rule
threeVowels = count3Vowels 0
  where
    count3Vowels :: Int -> String -> Bool
    count3Vowels n _
      | n >= 3 = True
    count3Vowels _ [] = False
    count3Vowels n (c : cs) = count3Vowels (n + if c `elem` vowels then 1 else 0) cs

    vowels :: String
    vowels = "aeiou"

twiceInARow :: Rule
twiceInARow = any ((>= 2) . length) . group

notContain :: Rule
notContain str = not $ any (`isInfixOf` str) forbidden
  where
    forbidden :: [String]
    forbidden = ["ab", "cd", "pq", "xy"]

-- xyxy
-- [xyxy] [xyx]

testVowels :: Bool
testVowels =
  threeVowels "aei"
    && threeVowels "xazegov"
    && threeVowels "aeiouaeiouaeiou"

testTwiceInARow :: Bool
testTwiceInARow =
  twiceInARow "xx"
    && twiceInARow "abcdde"
    && twiceInARow "aabbccdd"

testNotContain :: Bool
testNotContain =
  not $
    notContain "ab"
      && notContain "de"

testExamples :: Bool
testExamples =
  solve1 ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"] == 2

-- ----------- Solve -----------
solve1 :: [String] -> Int
solve1 =
  length
    . filter (\s -> threeVowels s && twiceInARow s && notContain s)

solve2 :: String -> Int
solve2 = undefined

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day5.txt"
  print $ "Testing vowel rule: " ++ show testVowels
  print $ "Testing two in a row rule: " ++ show testTwiceInARow
  print $ "Testing not contain rule: " ++ show testNotContain
  print $ "Testing example input?:" ++ show testExamples

  putChar '\n'

  parsed <- timeIt "Parsing " $ unwrapParser parseWords input

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

  print $ pairOfLetters "xyxy"

-- res2 <- timeIt "Part 2" $ solve2 input
-- print $ "Part 2: " ++ show res2
