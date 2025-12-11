module Linear.V2
  ( V2 (..),
    origin,
  )
where

import Control.DeepSeq (NFData (..))
import Data.Array (Ix (..))

data V2 a = V2 !a !a
  deriving (Show, Eq, Ord)

instance (NFData a) => NFData (V2 a) where
  rnf (V2 x y) = rnf x `seq` rnf y

instance (Ix a) => Ix (V2 a) where
  range (V2 x1 y1, V2 x2 y2) = [V2 x y | x <- range (x1, x2), y <- range (y1, y2)]
  index (V2 x1 y1, V2 x2 y2) (V2 x y) =
    index (x1, x2) x * rangeSize (y1, y2) + index (y1, y2) y
  inRange (V2 x1 y1, V2 x2 y2) (V2 x y) =
    inRange (x1, x2) x && inRange (y1, y2) y

origin :: (Num a) => V2 a
origin = V2 0 0
{-# INLINE origin #-}
