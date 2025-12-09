module AoCFramework.Dispatch
  ( dispatch,
  )
where

import AoCFramework.AoCTypes
import qualified Data.Map as Map
import Year2015.Day1 as Y2015D1
import Year2015.Day2 as Y2015D2
import Year2015.Day3 as Y2015D3

type SolutionMap = Map.Map (Year, Day) (IO ())

solutions :: SolutionMap
solutions =
  Map.fromList
    [ ((Year 2015, Day 1), Y2015D1.run),
      ((Year 2015, Day 2), Y2015D2.run),
      ((Year 2015, Day 3), Y2015D3.run)
    ]

dispatch :: Year -> Day -> IO ()
dispatch year day =
  case Map.lookup (year, day) solutions of
    Nothing -> putStrLn $ "No solution for year " ++ show year ++ " day " ++ show day
    Just action -> do
      putStrLn $ "Running: " ++ show year ++ " " ++ show day
      action
