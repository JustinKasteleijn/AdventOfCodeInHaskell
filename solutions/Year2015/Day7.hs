{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Year2015.Day7
  ( run,
  )
where

import Benchmark
import Control.DeepSeq (NFData)
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Parser (Parser (..), alpha1, alt, choice, lines1, string, u16, unwrapParser, (<?>))
import Types.IntegerTypes (U16)

-- -------------- Data types and instances ------------------
newtype Wire = Wire String
  deriving (Show, Eq, Generic, NFData, Hashable)

type Signals = HM.HashMap Wire U16

type Circuit = HM.HashMap Wire Expr

data ExprValue
  = Ref {-# UNPACK #-} !Wire
  | Lit {-# UNPACK #-} !U16
  deriving (Show, Eq, NFData, Generic)

data Expr
  = Assign {-# UNPACK #-} !ExprValue
  | AND {-# UNPACK #-} !ExprValue {-# UNPACK #-} !ExprValue
  | OR {-# UNPACK #-} !ExprValue {-# UNPACK #-} !ExprValue
  | SHIFTR {-# UNPACK #-} !ExprValue {-# UNPACK #-} !U16
  | SHIFTL {-# UNPACK #-} !ExprValue {-# UNPACK #-} !U16
  | NOT {-# UNPACK #-} !ExprValue
  deriving (Show, Eq, NFData, Generic)

data Statement
  = Statement {-# UNPACK #-} !Wire {-# UNPACK #-} !Expr
  deriving (Show, Eq, NFData, Generic)

-- ---------------- Parsing --------------------
parseValue :: Parser ExprValue
parseValue =
  alt
    ( Lit <$> u16,
      Ref . Wire <$> alpha1
    )
{-# INLINE parseValue #-}

parseTarget :: Parser Wire
parseTarget =
  Wire
    <$ string " -> "
    <*> alpha1
{-# INLINE parseTarget #-}

parseStatement :: Parser Statement
parseStatement =
  choice
    [ parseAnd,
      parseOr,
      parseShiftL,
      parseShiftR,
      parseNot,
      parseAssign
    ]

parseCircuit :: Parser Circuit
parseCircuit =
  HM.fromList
    . map statementToTuple
    <$> lines1 parseStatement
  where
    statementToTuple :: Statement -> (Wire, Expr)
    statementToTuple (Statement wire expr) = (wire, expr)

parseBinGate ::
  String ->
  (ExprValue -> ExprValue -> Expr) ->
  Parser Statement
parseBinGate str constructor = do
  v1 <- parseValue
  _ <- string str
  v2 <- parseValue
  target <- parseTarget
  return $ Statement target (constructor v1 v2)
{-# INLINE parseBinGate #-}

parseAnd :: Parser Statement
parseAnd = parseBinGate " AND " AND

parseOr :: Parser Statement
parseOr = parseBinGate " OR " OR

parseWire :: Parser ExprValue
parseWire =
  parseValue >>= \case
    r@(Ref _) -> return r
    Lit _ -> fail "Failed to parse wire"
{-# INLINE parseWire #-}

parseLit :: Parser U16
parseLit =
  parseValue >>= \case
    Lit n -> return n
    Ref _ -> fail "Failed to parse literal"
{-# INLINE parseLit #-}

parseShift ::
  String ->
  (ExprValue -> U16 -> Expr) ->
  Parser Statement
parseShift str constructor = do
  wire <- parseWire <?> ("Expected a wire as first argument to:" ++ str)
  _ <- string str
  lit <- parseLit <?> ("Expected a wire as the second argument to:" ++ str)
  target <- parseTarget
  return $ Statement target (constructor wire lit)
{-# INLINE parseShift #-}

parseShiftR :: Parser Statement
parseShiftR = parseShift " RSHIFT " SHIFTR

parseShiftL :: Parser Statement
parseShiftL = parseShift " LSHIFT " SHIFTL

parseNot :: Parser Statement
parseNot = do
  _ <- string "NOT "
  wire <- parseWire <?> "Expected a wire as the argument of NOT (complement)"
  target <- parseTarget
  return $ Statement target (NOT wire)
{-# INLINE parseNot #-}

parseAssign :: Parser Statement
parseAssign = do
  val <- parseValue
  target <- parseTarget
  return $ Statement target (Assign val)
{-# INLINE parseAssign #-}

-- -------------- Evaluators ---------------

evalExpr :: Circuit -> Signals -> Expr -> (U16, Signals)
evalExpr circuit signals = \case
  Assign v -> evalValue circuit signals v
  AND a b ->
    let !(a', signals') = evalValue circuit signals a
        !(b', signals'') = evalValue circuit signals' b
        !res = a' .&. b'
     in (res, signals'')
  OR a b ->
    let !(a', signals') = evalValue circuit signals a
        !(b', signals'') = evalValue circuit signals' b
        !res = (a' .|. b')
     in (res, signals'')
  SHIFTL a n ->
    let !(a', signals') = evalValue circuit signals a
        !res = a' `shiftL` fromIntegral n
     in (res, signals')
  SHIFTR a n ->
    let !(a', signals') = evalValue circuit signals a
        !res = a' `shiftR` fromIntegral n
     in (res, signals')
  NOT a ->
    let !(a', signals') = evalValue circuit signals a
        !res = complement a'
     in (res, signals')

evalValue :: Circuit -> Signals -> ExprValue -> (U16, Signals)
evalValue circuit signals = \case
  Lit n -> (n, signals)
  Ref x -> evalWire circuit signals x
{-# INLINE evalValue #-}

evalWire :: Circuit -> Signals -> Wire -> (U16, Signals)
evalWire circuit signals wire =
  case HM.lookup wire signals of
    Just n -> (n, signals)
    Nothing ->
      let !expr = circuit HM.! wire
          !(n, signals') = evalExpr circuit signals expr
          memo = HM.insert wire n signals'
       in (n, memo)
{-# INLINE evalWire #-}

solve1 :: Circuit -> U16
solve1 circuit =
  let (res, _) = evalWire circuit HM.empty (Wire "a")
   in res

solve2 :: Circuit -> U16 -> U16
solve2 circuit prev =
  let (res, _) = evalWire circuit (HM.fromList [(Wire "b", prev)]) (Wire "a")
   in res

run :: IO ()
run = do
  print $ "Test parsing on example input: " ++ show testParsingExample

  putChar '\n'

  input <- readFile "solutions/Year2015/inputs/day7.txt"
  parsed <- timeIt "Parsing" (unwrapParser parseCircuit input)

  putChar '\n'

  res1 <- timeIt "Part 1" (solve1 parsed)
  print res1

  putChar '\n'

  res2 <- timeIt "Part 2" (solve2 parsed res1)
  print res2

testParsingExample :: Bool
testParsingExample = unwrapParser parseCircuit circuitInput == circuitTest

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

circuitTest :: Circuit
circuitTest =
  HM.fromList
    [ (Wire "x", Assign (Lit 123)),
      (Wire "y", Assign (Lit 456)),
      (Wire "d", AND (Ref (Wire "x")) (Ref (Wire "y"))),
      (Wire "e", OR (Ref (Wire "x")) (Ref (Wire "y"))),
      (Wire "f", SHIFTL (Ref (Wire "x")) 2),
      (Wire "g", SHIFTR (Ref (Wire "y")) 2),
      (Wire "h", NOT (Ref (Wire "x"))),
      (Wire "i", NOT (Ref (Wire "y")))
    ]

-- ------------ Comments ----------------
--
-- Applicative ParseAnd
-- Think do notation is more readable in this case
--
-- parseAnd :: Parser Statement
-- parseAnd =
--   Statement
--     <$> parseTarget
--     <*> parseAnd'
--   where
--     parseAnd' :: Parser Expr
--     parseAnd' =
--       AND
--         <$> parseValue
--         <* string " AND "
--         <*> parseValue
