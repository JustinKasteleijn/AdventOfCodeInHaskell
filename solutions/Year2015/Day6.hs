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
import Data.Array.Base (MArray (unsafeRead, unsafeWrite))
import Data.Array.ST
import Data.Functor (($>))
import Data.List (foldl')
import GHC.Generics (Generic (..))
import Linear.V2 (V2 (..))
import Parser (Parser (..), choice, int, lines1, splitOn, string, unwrapParser)
import Strict.List (product')

data Modify
  = Toggle
  | On
  | Off
  deriving (Show, Generic, NFData)

type Grid s a = STUArray s Int a

-- (0,0) -> 0   (1,0) -> 1   (2,0) -> 2
-- (0,1) -> 3   (1,1) -> 4   (2,1) -> 5
-- (0,2) -> 6   (1,2) -> 7   (2,2) -> 8
-- Example in 3x3 => flatten 3 (1, 1) -> 1 * 3 + 1 = 4 (middle)
-- Used to Index in a 1-d flatten
flattenV2 :: Int -> V2 Int -> Int
flattenV2 width (V2 x y) = y * width + x

data Instruction
  = Instruction
  { modifier :: {-# UNPACK #-} !Modify,
    from :: {-# UNPACK #-} !(V2 Int),
    to :: {-# UNPACK #-} !(V2 Int)
  }
  deriving (Show, Generic, NFData)

parseModifier :: Parser String Modify
parseModifier =
  choice
    [ string "toggle " $> Toggle,
      string "turn on " $> On,
      string "turn off " $> Off
    ]

parseCoords :: Parser String (V2 Int)
parseCoords = fmap (uncurry V2) (splitOn ',' int)

parseInstruction :: Parser String Instruction
parseInstruction = do
  modifier' <- parseModifier
  from' <- parseCoords
  _ <- string " through "
  Instruction modifier' from' <$> parseCoords

parseInstructions :: Parser String [Instruction]
parseInstructions = lines1 parseInstruction

executeInstruction ::
  (MArray (STUArray s) a (ST s)) =>
  (Modify -> a -> a) ->
  Grid s a ->
  Instruction ->
  ST s ()
executeInstruction act arr (Instruction modifier' (V2 x1 y1) (V2 x2 y2)) =
  let f = act modifier'
   in forM_ [x1 .. x2] $ \x ->
        forM_ [y1 .. y2] $ \y -> do
          let idx = flattenV2 1000 (V2 x y) -- Flatten index for faster lookup
          old <- unsafeRead arr idx
          unsafeWrite arr idx (f old)

actBool :: Modify -> Bool -> Bool
actBool On _ = True
actBool Off _ = False
actBool Toggle b = not b
{-# INLINE actBool #-}

actInt :: Modify -> Int -> Int
actInt On n = n + 1
actInt Off n = max 0 (n - 1)
actInt Toggle n = n + 2
{-# INLINE actInt #-}

solve1 :: [Instruction] -> ST s Int
solve1 instructions = do
  let width = 1000
  let height = 1000
  arr <- newArray (0, (width * height) - 1) False :: ST s (Grid s Bool)
  mapM_ (executeInstruction actBool arr) instructions
  elems <- getElems arr
  return $ foldl' countTrues 0 elems
  where
    countTrues :: Int -> Bool -> Int
    countTrues acc True = acc + 1
    countTrues acc False = acc
    {-# INLINE countTrues #-}

solve2 :: [Instruction] -> ST s Int
solve2 instructions = do
  let width = 1000
  let height = 1000
  arr <- newArray (0, (width * height) - 1) 0 :: ST s (Grid s Int)
  mapM_ (executeInstruction actInt arr) instructions
  elems <- getElems arr
  return $ product' elems

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
