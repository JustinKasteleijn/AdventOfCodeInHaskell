{-# LANGUAGE Safe #-}

module Types.IntegerTypes
  ( U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
  )
where

import Data.Int
import Data.Word

-- Unsigned integers
type U8 = Word8

type U16 = Word16

type U32 = Word32

type U64 = Word64

-- Signed integers
type I8 = Int8

type I16 = Int16

type I32 = Int32

type I64 = Int64
