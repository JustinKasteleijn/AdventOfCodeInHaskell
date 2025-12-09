module Main where

import AoCFramework.AoCTypes
import AoCFramework.Dispatch (dispatch)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Left err -> putStrLn $ "Error: " ++ err
    Right (year, day) -> dispatch year day

parseArgs :: [String] -> Either String (Year, Day)
parseArgs ["-year", yStr, "-day", dStr] = do
  year <- mkYear yStr
  day <- mkDay dStr
  Right (year, day)
parseArgs _ = Left "Usage: -year <year> -day <day>"
