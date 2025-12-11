module Strict.List
  ( sum',
    product',
  )
where

import Data.List (foldl')

sum' :: (Num a) => [a] -> a
sum' = foldl' (+) 0
{-# INLINE sum' #-}

product' :: (Num a) => [a] -> a
product' = foldl' (+) 0
{-# INLINE product' #-}
