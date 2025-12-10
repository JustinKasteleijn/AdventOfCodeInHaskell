{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Year2015.Day6
  ( run,
  )
where

import Benchmark
import Control.DeepSeq (NFData (..), force)
import Control.Monad (forM_)
import Control.Monad.ST
import Data.Array.ST
import Data.Functor (($>))
import GHC.Generics (Generic (..))
import Linear.V2 (V2 (..))
import Parser (Parser (..), choice, int, lines1, splitOn, string, unwrapParser)

data Modify
  = Toggle
  | On
  | Off
  deriving (Show, Generic, NFData)

type Grid s a = STUArray s (V2 Int) a

data Instruction
  = Instruction
  { modifier :: !Modify,
    from :: !(V2 Int),
    to :: !(V2 Int)
  }
  deriving (Show, Generic, NFData)

parseModifier :: Parser Modify
parseModifier =
  choice
    [ string "toggle " $> Toggle,
      string "turn on " $> On,
      string "turn off " $> Off
    ]

parseCoords :: Parser (V2 Int)
parseCoords = fmap (uncurry V2) (splitOn ',' int)

parseInstruction :: Parser Instruction
parseInstruction = do
  modifier' <- parseModifier
  from' <- parseCoords
  _ <- string " through "
  Instruction modifier' from' <$> parseCoords

parseInstructions :: Parser [Instruction]
parseInstructions = lines1 parseInstruction

executeInstruction :: (MArray (STUArray s) a (ST s)) => (Modify -> a -> a) -> Grid s a -> Instruction -> ST s ()
executeInstruction act arr (Instruction modifier' (V2 x1 y1) (V2 x2 y2)) =
  forM_ [x1 .. x2] $ \x ->
    forM_ [y1 .. y2] $ \y -> do
      old <- readArray arr (V2 x y)
      writeArray arr (V2 x y) (act modifier' old)

actBool :: Modify -> Bool -> Bool
actBool On _ = True
actBool Off _ = False
actBool Toggle b = not b

solve1 :: [Instruction] -> ST s Int
solve1 instructions = do
  arr <- newArray (V2 0 0, V2 999 999) False :: ST s (Grid s Bool)
  mapM_ (executeInstruction actBool arr) instructions
  elems <- getElems arr
  return $ length $ filter id elems

solve2 :: [Instruction] -> ST s Int
solve2 instructions = do
  arr <- newArray (V2 0 0, V2 999 999) 0 :: ST s (Grid s Int)
  mapM_ (executeInstruction actInt arr) instructions
  elems <- getElems arr
  return $ sum elems

actInt :: Modify -> Int -> Int
actInt On n = n + 1
actInt Off n = max 0 (n - 1)
actInt Toggle n = n + 2

run :: IO ()
run = do
  input <- readFile "solutions/Year2015/inputs/day6.txt"
  parsed <- timeIt "Parsed: " $ unwrapParser parseInstructions input
  -- print $ "Testing example input: " ++ show test1

  putChar '\n'

  res1 <- timeIt "Part 1" $ force $ runST $ solve1 parsed
  print $ "Part 1: " ++ show res1

  putChar '\n'

  res2 <- timeIt "Part 2" $ force $ runST $ solve2 parsed
  print $ "Part 2: " ++ show res2
