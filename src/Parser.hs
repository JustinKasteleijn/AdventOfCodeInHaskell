{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Parser
  ( Parser (..),
    item,
    char,
    string,
    int,
    nat,
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
    parseLine,
    sepBy0,
    sepBy1,
    lines1,
    splitOn,
    splitOn',
    choice,
    alt,
    unwrapParser,
    i8,
    i16,
    i32,
    i64,
    u8,
    u16,
    u32,
    u64,
    (<?>),
  )
where

import           Control.Applicative (Alternative (..), asum)
import           Control.Monad       (void)
import qualified Data.ByteString     as BS
import           Data.Char           (isAlpha, isAlphaNum, isDigit, isSpace,
                                      ord)
import           Data.Data           (Proxy (..))
import           Data.List           (foldl')
import           Types.IntegerTypes

type ParserError = String

newtype Parser s a = Parser {parse :: s -> Either ParserError (a, s)}

-- Instances

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f px = Parser $
    \input -> do
      (x, rest) <- parse px input
      Right (f x, rest)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure x = Parser $
    \input -> Right (x, input)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  pf <*> px = Parser $
    \input -> do
      (f, rest) <- parse pf input
      (x, rest') <- parse px rest
      Right (f x, rest')

instance Monad (Parser s) where
  return :: a -> Parser s a
  return = pure

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  px >>= f = Parser $
    \input -> do
      (x, rest) <- parse px input
      parse (f x) rest

instance MonadFail (Parser s) where
  fail :: String -> Parser s a
  fail msg = Parser $ \_ -> Left msg

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ \_ -> Left ""

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  px <|> py = Parser $
    \input -> case parse px input of
      Right t -> Right t
      Left msg -> case parse py input of
        Right t'  -> Right t'
        Left msg' -> Left $ msg ++ " or " ++ msg'

--- Supported types

class Textual s where
  type Elem s
  uncons :: s -> Maybe (Elem s, s)

instance Textual [a] where
  type Elem [a] = a

  uncons :: [a] -> Maybe (a, [a])
  uncons []       = Nothing
  uncons (x : xs) = Just (x, xs)

instance Textual BS.ByteString where
  type Elem BS.ByteString = U8

  uncons :: BS.ByteString -> Maybe (U8, BS.ByteString)
  uncons bstr =
    if BS.null bstr
      then Nothing
      else Just (BS.head bstr, BS.tail bstr)

class IsDigit t where
  isDigitToken :: t -> Bool

instance IsDigit Char where
  isDigitToken :: Char -> Bool
  isDigitToken = isDigit

instance IsDigit U8 where
  isDigitToken :: U8 -> Bool
  isDigitToken w = w >= 48 && w <= 57

class IsSpace t where
  isSpaceToken :: t -> Bool

instance IsSpace Char where
  isSpaceToken :: Char -> Bool
  isSpaceToken = isSpace

instance IsSpace U8 where
  isSpaceToken :: U8 -> Bool
  isSpaceToken = (== 32)

class IsNewline t where
  isNewlineToken :: t -> Bool

instance IsNewline Char where
  isNewlineToken :: Char -> Bool
  isNewlineToken c = (c == '\n') || (c == '\r')

instance IsNewline U8 where
  isNewlineToken :: U8 -> Bool
  isNewlineToken w = w == 10 || w == 13

class IsAlpha t where
  isAlphaToken :: t -> Bool

instance IsAlpha Char where
  isAlphaToken :: Char -> Bool
  isAlphaToken = isAlpha

instance IsAlpha U8 where
  isAlphaToken :: U8 -> Bool
  isAlphaToken w =
    (w >= 65 && w <= 90) -- 'A'..'Z'
      || (w >= 97 && w <= 122) -- 'a'..'z'

class IsAlphaNum t where
  isAlphaNumToken :: t -> Bool

instance IsAlphaNum Char where
  isAlphaNumToken :: Char -> Bool
  isAlphaNumToken = isAlphaNum

instance IsAlphaNum U8 where
  isAlphaNumToken :: U8 -> Bool
  isAlphaNumToken w =
    (w >= 48 && w <= 57) -- '0'..'9'
      || (w >= 65 && w <= 90) -- 'A'..'Z'
      || (w >= 97 && w <= 122) -- 'a'..'z'

class Digit t where
  digitToInt :: t -> Int

instance Digit Char where
  digitToInt c = ord c - ord '0'

instance Digit U8 where
  digitToInt w = fromIntegral w - 48

class (Textual s) => MinusSign s where
  minus :: Proxy s -> Elem s

instance MinusSign String where
  minus :: Proxy String -> Char
  minus _ = '-'

instance MinusSign BS.ByteString where
  minus :: Proxy BS.ByteString -> U8
  minus _ = 45

--- Parser functions

item :: (Textual s) => Parser s (Elem s)
item = Parser $ \input ->
  case uncons input of
    Nothing      -> Left "Unexpected end of input"
    Just (x, xs) -> Right (x, xs)

char :: (Textual s, Eq (Elem s), Show (Elem s)) => Elem s -> Parser s (Elem s)
char c = do
  x <- item
  if x == c
    then return x
    else fail $ "Expected: " ++ show c ++ " Received: " ++ show x

satisfy :: (Textual s, Show (Elem s)) => (Elem s -> Bool) -> Parser s (Elem s)
satisfy f = do
  x <- item
  if f x
    then return x
    else fail $ "Input: '" ++ show x ++ "' does not satisfy predicate"

digit :: (Textual s, Show (Elem s), IsDigit (Elem s)) => Parser s (Elem s)
digit = satisfy isDigitToken

digits0 :: (Textual s, Show (Elem s), IsDigit (Elem s)) => Parser s [Elem s]
digits0 = many digit

digits1 :: (Textual s, Show (Elem s), IsDigit (Elem s)) => Parser s [Elem s]
digits1 = some digit

space :: (Textual s, Show (Elem s), IsSpace (Elem s)) => Parser s ()
space = void $ satisfy isSpaceToken

spaces0 :: (Textual s, Show (Elem s), IsSpace (Elem s)) => Parser s ()
spaces0 = void $ many space

spaces1 :: (Textual s, Show (Elem s), IsSpace (Elem s)) => Parser s ()
spaces1 = void $ some space

newline :: (Textual s, Show (Elem s), IsNewline (Elem s)) => Parser s ()
newline = void $ satisfy isNewlineToken

newline0 :: (Textual s, Show (Elem s), IsNewline (Elem s)) => Parser s ()
newline0 = void $ many newline

newline1 :: (Textual s, Show (Elem s), IsNewline (Elem s)) => Parser s ()
newline1 = void $ some newline

alpha :: (Textual s, Show (Elem s), IsAlpha (Elem s)) => Parser s (Elem s)
alpha = satisfy isAlphaToken

alpha0 :: (Textual s, Show (Elem s), IsAlpha (Elem s)) => Parser s [Elem s]
alpha0 = many alpha

alpha1 :: (Textual s, Show (Elem s), IsAlpha (Elem s)) => Parser s [Elem s]
alpha1 = some alpha

alphaNum :: (Textual s, Show (Elem s), IsAlphaNum (Elem s)) => Parser s (Elem s)
alphaNum = satisfy isAlphaNumToken

alphaNum0 :: (Textual s, Show (Elem s), IsAlphaNum (Elem s)) => Parser s [Elem s]
alphaNum0 = many alphaNum

alphaNum1 :: (Textual s, Show (Elem s), IsAlphaNum (Elem s)) => Parser s [Elem s]
alphaNum1 = some alphaNum

string :: (Textual s, Show (Elem s), Eq (Elem s)) => [Elem s] -> Parser s [Elem s]
string = mapM char

nat :: (Textual s, Show (Elem s), IsDigit (Elem s), Digit (Elem s)) => Parser s Int
nat = foldl' step 0 <$> digits1
  where
    step acc d = acc * 10 + digitToInt d

option :: (Alternative f) => a -> f a -> f a
option def p = p <|> pure def

int ::
  forall s.
  (Textual s, Show (Elem s), IsDigit (Elem s), Digit (Elem s), MinusSign s, Eq (Elem s)) =>
  Parser s Int
int = do
  f <- option id (negate <$ char (minus (Proxy :: Proxy s)))
  f <$> nat

parseBounded ::
  forall s a.
  ( Textual s,
    Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s),
    Bounded a,
    Integral a
  ) =>
  Parser s a
parseBounded = do
  n <- int
  let n' = fromIntegral n
  if n' >= minBound && n' <= maxBound
    then return n'
    else fail $ "Value out of bounds: " ++ show n

u8 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s U8
u8 = parseBounded

u16 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s U16
u16 = parseBounded

u32 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s U32
u32 = parseBounded

u64 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s U64
u64 = parseBounded

i8 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s I8
i8 = parseBounded

i16 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s I16
i16 = parseBounded

i32 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s I32
i32 = parseBounded

i64 ::
  ( Show (Elem s),
    IsDigit (Elem s),
    Digit (Elem s),
    MinusSign s,
    Eq (Elem s)
  ) =>
  Parser s I64
i64 = parseBounded

sepBy1 :: (Textual s) => Parser s a -> Parser s b -> Parser s [a]
sepBy1 px psep =
  (:)
    <$> px
    <*> many (psep *> px)

sepBy0 :: (Textual s) => Parser s a -> Parser s b -> Parser s [a]
sepBy0 px psep = sepBy1 px psep <|> pure []

lines1 :: (Textual s, IsNewline (Elem s), Show (Elem s)) => Parser s a -> Parser s [a]
lines1 px = sepBy1 px newline

choice :: [Parser s a] -> Parser s a
choice = asum

alt :: (Textual s) => (Parser s a, Parser s a) -> Parser s a
alt (px, py) = px <|> py

splitOn :: (Textual s, Show (Elem s), Eq (Elem s)) => Elem s -> Parser s a -> Parser s (a, a)
splitOn c px = do
  xs <- sepBy0 px (char c)
  case xs of
    [a, b] -> return (a, b)
    _ -> fail $ "Expected exactly two elements seperated by the delimiter:" ++ show c

splitOn' :: (Textual s, Show (Elem s), Eq (Elem s)) => [Elem s] -> Parser s a -> Parser s b -> Parser s (a, b)
splitOn' str px py = do
  x <- px
  _ <- string str
  y <- py
  return (x, y)

parseLine :: (Textual s, IsNewline (Elem s), Show (Elem s)) => Parser s [Elem s]
parseLine = many (satisfy (not . isNewlineToken))

label :: (Textual s) => ParserError -> Parser s a -> Parser s a
label errMsg px = Parser $ \input ->
  case parse px input of
    Right t -> Right t
    Left _  -> Left errMsg

(<?>) :: (Textual s) => Parser s a -> String -> Parser s a
px <?> ctx = label ctx px

-- Utilities

unwrapParser :: Parser s a -> s -> a
unwrapParser p input =
  case parse p input of
    Right (x, _) -> x
    Left err     -> error $ "Parse failed: " ++ err
