module AoCFramework.AoCTypes where

import Text.Read (readMaybe)

newtype Day = Day Int
  deriving (Show, Eq, Ord, Read)

newtype Year = Year Int
  deriving (Show, Eq, Ord, Read)

mkDay :: String -> Either String Day
mkDay str =
  case readMaybe str of
    Nothing -> Left "Not a number"
    Just n
      | n < 1 || n > 25 -> Left "Day must be 1-25"
      | otherwise -> Right (Day n)

mkYear :: String -> Either String Year
mkYear str =
  case readMaybe str of
    Nothing -> Left "Not a number"
    Just n
      | n < 2015 -> Left "No AoC before 2015"
      | otherwise -> Right (Year n)
