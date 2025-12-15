module Year2015.Day5
  ( run,
  )
where

import Benchmark
import Data.List (group, isInfixOf, tails)
import Parser

type Rule = String -> Bool

parseWords :: Parser String [String]
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

pairsOfLetter :: Rule
pairsOfLetter s =
  any hasRepeat (tails s)
  where
    hasRepeat :: String -> Bool
    hasRepeat (a : b : rest) =
      any ([a, b] `isInfixOf`) (tails rest)
    hasRepeat _ = False

repeatingLetterOneInBetween :: Rule
repeatingLetterOneInBetween str =
  any (uncurry (==)) (zip str (drop 2 str))

testRepeatingLetterOneInBetween :: Bool
testRepeatingLetterOneInBetween =
  repeatingLetterOneInBetween "xyx"
    && repeatingLetterOneInBetween "abcdefeghi"
    && repeatingLetterOneInBetween "aaa"

testPairsOfLetter :: Bool
testPairsOfLetter =
  pairsOfLetter "xyxy"
    && pairsOfLetter "aabcdefaa"
    && not (pairsOfLetter "aaa")

testExamplesP2 :: Bool
testExamplesP2 =
  solve2 ["qjhvhtzxzqqjkmpb", "xxyxx", "uurcxstgmygtbstg", "ieodomkazucvgmuy"] == 2

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

testExamplesP1 :: Bool
testExamplesP1 =
  solve1 ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"] == 2

-- ----------- Solve -----------
solve1 :: [String] -> Int
solve1 =
  length
    . filter (\s -> threeVowels s && twiceInARow s && notContain s)

solve2 :: [String] -> Int
solve2 =
  length
    . filter (\s -> repeatingLetterOneInBetween s && pairsOfLetter s)

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day5.txt"

  putStrLn "Part 1:"

  print $ "Testing vowel rule: " ++ show testVowels
  print $ "Testing two in a row rule: " ++ show testTwiceInARow
  print $ "Testing not contain rule: " ++ show testNotContain
  print $ "Testing example input?:" ++ show testExamplesP1

  putChar '\n'

  parsed <- timeIt "Parsing " $ unwrapParser parseWords input

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

  putStrLn "Part 2:"

  print $ "Testing pairs of letter rule: " ++ show testPairsOfLetter
  print $ "Testing letter with one other letter in between: " ++ show testRepeatingLetterOneInBetween
  print $ "Testing examples part 2: " ++ show testExamplesP2

  res2 <- timeIt "Part 2" $ solve2 parsed
  print $ "Part 2: " ++ show res2
