{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser (..),
    item,
    char,
    string,
    int,
    nat,
    try,
    digits0,
    digits1,
    spaces0,
    spaces1,
    alpha0,
    alpha1,
    alphaNum0,
    alphaNum1,
    newline0,
    newline1,
    sepBy0,
    sepBy1,
    lines1,
    splitOn,
    splitOn',
    choice,
    unwrapParser,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,
  )
where

import Control.Applicative (Alternative (..), asum)
import Control.Monad (void)
import Data.Char (isAlpha, isAlphaNum, isDigit)
import Types.IntegerTypes

type Error = String

newtype Parser a = Parser {parse :: String -> Either Error (a, String)}

-- Instances

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = Parser $
    \input -> do
      (x, rest) <- parse px input
      Right (f x, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $
    \input -> Right (x, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> px = Parser $
    \input -> do
      (f, rest) <- parse pf input
      (x, rest') <- parse px rest
      Right (f x, rest')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  px >>= f = Parser $
    \input -> do
      (x, rest) <- parse px input
      parse (f x) rest

instance MonadFail Parser where
  fail :: String -> Parser a
  fail msg = Parser $ \_ -> Left msg

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left ""

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py = Parser $
    \input -> case parse px input of
      Right t -> Right t
      Left msg -> case parse py input of
        Right t' -> Right t'
        Left msg' -> Left $ msg ++ " or " ++ msg'

--- Parser functions

item :: Parser Char
item = Parser $ \case
  (x : xs) -> Right (x, xs)
  [] -> Left "Unexpected end of input"

char :: Char -> Parser Char
char c = do
  x <- item
  if x == c
    then return x
    else fail $ "Expected: " ++ [c] ++ " Received: " ++ [x]

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  x <- item
  if f x
    then return x
    else fail $ "Input: '" ++ [x] ++ "' does not satisfy predicate"

digit :: Parser Char
digit = satisfy isDigit

digits0 :: Parser [Char]
digits0 = many digit

digits1 :: Parser [Char]
digits1 = some digit

space :: Parser ()
space = void $ satisfy (== ' ')

spaces0 :: Parser ()
spaces0 = void $ many space

spaces1 :: Parser ()
spaces1 = void $ some space

newline :: Parser ()
newline = void $ satisfy (== '\n') <|> satisfy (== '\r')

newline0 :: Parser ()
newline0 = void $ many newline

newline1 :: Parser ()
newline1 = void $ some newline

alpha :: Parser Char
alpha = satisfy isAlpha

alpha0 :: Parser String
alpha0 = many alpha

alpha1 :: Parser String
alpha1 = some alpha

alphaNum :: Parser Char
alphaNum = satisfy isAlphaNum

alphaNum0 :: Parser String
alphaNum0 = many alphaNum

alphaNum1 :: Parser String
alphaNum1 = some alphaNum

string :: String -> Parser String
string = mapM char

nat :: Parser Int
nat = read <$> digits1

int :: Parser Int
int = nat <|> char '-' *> (negate <$> nat)

parseBounded :: (Bounded a, Integral a) => Parser a
parseBounded = do
  n <- int
  let n' = fromIntegral n
  if n' >= minBound && n' <= maxBound
    then return n'
    else fail $ "Value out of bounds: " ++ show n

u8 :: Parser U8
u8 = parseBounded

u16 :: Parser U16
u16 = parseBounded

u32 :: Parser U32
u32 = parseBounded

u64 :: Parser U64
u64 = parseBounded

i8 :: Parser I8
i8 = parseBounded

i16 :: Parser I16
i16 = parseBounded

i32 :: Parser I32
i32 = parseBounded

i64 :: Parser I64
i64 = parseBounded

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 px psep =
  (:)
    <$> px
    <*> many (psep *> px)

sepBy0 :: Parser a -> Parser b -> Parser [a]
sepBy0 px psep = sepBy1 px psep <|> pure []

lines1 :: Parser a -> Parser [a]
lines1 px = sepBy1 px newline

try :: Parser a -> Parser a
try p = Parser $ \input ->
  case parse p input of
    Right result -> Right result
    Left _ -> Left ""

choice :: [Parser a] -> Parser a
choice = asum

splitOn :: Char -> Parser a -> Parser (a, a)
splitOn c px = do
  xs <- sepBy0 px (char c)
  case xs of
    [a, b] -> return (a, b)
    _ -> fail $ "Expected exactly two elements seperated by the delimiter:" ++ [c]

splitOn' :: String -> Parser a -> Parser b -> Parser (a, b)
splitOn' str px py = do
  x <- px
  _ <- string str
  y <- py
  return (x, y)

-- Utilities

unwrapParser :: Parser a -> String -> a
unwrapParser p input =
  case parse p input of
    Right (x, _) -> x
    Left err -> error $ "Parse failed: " ++ err
