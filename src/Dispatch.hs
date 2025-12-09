module Dispatch
  ( dispatch,
  )
where

import AoCTypes
import qualified Data.Map as Map
import Year2015.Day1 as Y2025D1

type SolutionMap = Map.Map (Year, Day) (IO ())

solutions :: SolutionMap
solutions =
  Map.fromList
    [ ((Year 2015, Day 1), Y2025D1.run)
    ]

dispatch :: Year -> Day -> IO ()
dispatch year day =
  case Map.lookup (year, day) solutions of
    Nothing -> putStrLn $ "No solution for year " ++ show year ++ " day " ++ show day
    Just action -> action
