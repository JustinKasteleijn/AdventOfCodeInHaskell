module Benchmark
  ( timeIt,
  )
where

import           Control.DeepSeq (NFData (..), deepseq)
import           Data.Time.Clock

timeIt :: (NFData a) => String -> a -> IO a
timeIt name action = do
  start <- getCurrentTime
  action `deepseq` return ()
  end <- getCurrentTime
  putStrLn $ name ++ " took: " ++ show (diffUTCTime end start)
  return action
