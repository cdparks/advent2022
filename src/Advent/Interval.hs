{-# LANGUAGE StrictData #-}

module Advent.Interval
  ( Interval
  , new
  , lo
  , hi
  , overlaps
  , contains
  ) where

import Advent.Prelude hiding (combine)

-- | Closed interval
--
-- Invariant - for all i :: Interval, lo i <= hi i
--
data Interval = Interval
  { _lo :: Int
  , _hi :: Int
  }
  deriving stock (Eq, Ord, Generic)
  deriving anyclass Hashable

instance Show Interval where
  showsPrec d Interval {..}
    = showParen (d > 9)
    $ showString "Interval.new "
    . shows _lo
    . showChar ' '
    . shows _hi

-- | Create a proper closed 'Interval'
new :: Int -> Int -> Interval
new x y
  | x <= y = Interval x y
  | otherwise = Interval y x
{-# INLINE new #-}

-- | Low end of the interval
lo :: Interval -> Int
lo = _lo
{-# INLINE lo #-}

-- | High end of the interval
hi :: Interval -> Int
hi = _hi
{-# INLINE hi #-}

-- | Do these intervals overlap?
overlaps :: Interval -> Interval -> Bool
overlaps lhs rhs = max (lo lhs) (lo rhs) <= min (hi lhs) (hi rhs)
{-# INLINE overlaps #-}

-- | Does the left interval contain the right interval?
contains :: Interval -> Interval -> Bool
contains lhs rhs = lo lhs <= lo rhs && hi rhs <= hi lhs
{-# INLINE contains #-}
