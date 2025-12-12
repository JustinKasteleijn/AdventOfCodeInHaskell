{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Year2015.Day7
  ( run,
  )
where

import Benchmark
import Control.Applicative (Alternative (..))
import Control.DeepSeq (NFData (..))
import Data.Bits (Bits (complement, shiftR), complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.HashMap.Internal.Array as Map
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Parser (Parser (..), alpha1, choice, int, lines1, spaces0, string, u16, unwrapParser)
import Types.IntegerTypes
import Prelude hiding (map)

type Wire = String

type Signals = HM.HashMap Wire U16

data Value
  = Lit {-# UNPACK #-} !U16
  | Ref {-# UNPACK #-} !Wire
  deriving (Eq, Show, Generic, NFData)

data Expr
  = VAL {-# UNPACK #-} !Value
  | AND {-# UNPACK #-} !Value {-# UNPACK #-} !Value
  | OR {-# UNPACK #-} !Value {-# UNPACK #-} !Value
  | NOT {-# UNPACK #-} !Value
  | SHIFTL {-# UNPACK #-} !Int {-# UNPACK #-} !Value
  | SHIFTR {-# UNPACK #-} !Int {-# UNPACK #-} !Value
  deriving (Eq, Show, Generic, NFData)

data Statement
  = Statement {-# UNPACK #-} !Wire {-# UNPACK #-} !Expr
  deriving (Eq, Show, Generic, NFData)

-- Parsing!
parseValue :: Parser Value
parseValue =
  choice
    [ Lit <$> u16,
      Ref <$> alpha1
    ]

parseStatements :: Parser [Statement]
parseStatements = lines1 parseStatement

parseStatement :: Parser Statement
parseStatement =
  parseAnd
    <|> parseOr
    <|> parseLShift
    <|> parseRShift
    <|> parseNot
    <|> parseAssign

parseShift :: (Int -> Value -> Expr) -> String -> Parser Statement
parseShift constructor str = do
  var1 <- parseValue
  _ <- spaces0 *> string str <* spaces0
  by <- int
  _ <- string " -> "
  target <- alpha1
  return $ Statement target (constructor by var1)

parseBinary :: (Value -> Value -> Expr) -> String -> Parser Statement
parseBinary constructor str = do
  var1 <- parseValue
  _ <- spaces0 *> string str <* spaces0
  var2 <- parseValue
  _ <- string " -> "
  target <- alpha1
  return $ Statement target (constructor var1 var2)

parseAnd :: Parser Statement
parseAnd = parseBinary AND "AND"

parseOr :: Parser Statement
parseOr = parseBinary OR "OR"

parseLShift :: Parser Statement
parseLShift = parseShift SHIFTL "LSHIFT"

parseRShift :: Parser Statement
parseRShift = parseShift SHIFTR "RSHIFT"

parseNot :: Parser Statement
parseNot = do
  _ <- string "NOT "
  var <- parseValue
  _ <- string " -> "
  target <- alpha1
  return $ Statement target (NOT var)

parseAssign :: Parser Statement
parseAssign = do
  digits <- u16
  _ <- string " -> "
  var <- alpha1
  return $ Statement var (VAL $ Lit digits)

solve1 :: [Statement] -> U16
solve1 stmts = undefined

run :: IO ()
run = do
  print $ "Testing parsing And: " ++ show testParseAnd
  print $ "Testing parsing Or: " ++ show testParseOr
  print $ "Testing parsing Shift: " ++ show testParseShift
  print $ "Testing parsing Not: " ++ show testParseNot
  print $ "Testing parsing Assign: " ++ show testParseAssign

  putChar '\n'

  input <- readFile "solutions/Year2015/inputs/day7.txt"
  parsed <- timeIt "Parsing" $ unwrapParser parseStatements input

  putChar '\n'

  res1 <- timeIt "Part 1" $ solve1 parsed
  print res1

testParseAnd :: Bool
testParseAnd =
  unwrapParser parseAnd "x AND y -> d" == Statement "d" (AND (Ref "x") (Ref "y"))

testParseOr :: Bool
testParseOr =
  unwrapParser parseOr "x OR y -> d" == Statement "d" (OR (Ref "x") (Ref "y"))

testParseShift :: Bool
testParseShift =
  unwrapParser parseRShift "y RSHIFT 2 -> g" == Statement "g" (SHIFTR 2 (Ref "y"))

testParseNot :: Bool
testParseNot =
  unwrapParser parseNot "NOT x -> d" == Statement "d" (NOT (Ref "x"))

testParseAssign :: Bool
testParseAssign =
  unwrapParser parseAssign "123 -> x" == Statement "x" (VAL (Lit 123))

circuitInput :: String
circuitInput =
  unlines
    [ "123 -> x",
      "456 -> y",
      "x AND y -> d",
      "x OR y -> e",
      "x LSHIFT 2 -> f",
      "y RSHIFT 2 -> g",
      "NOT x -> h",
      "NOT y -> i"
    ]
