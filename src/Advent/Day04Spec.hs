module Advent.Day04Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Advent.Interval (Interval)
import qualified Advent.Interval as Interval

spec :: Spec
spec = parsing parsePairs 4 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 2
    part1 problem `shouldBe` 450

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 4
    part2 problem `shouldBe` 837

-- | In how many pairs does one interval fully contain the other?
part1 :: [(Interval, Interval)] -> Int
part1 = countWhere contained
 where
  contained x y = or
    [ x `Interval.contains` y
    , y `Interval.contains` x
    ]

-- | In how many pairs do the intervals overlap?
part2 :: [(Interval, Interval)] -> Int
part2 = countWhere Interval.overlaps

-- | Count pairs satisfying some predicate
countWhere :: (a -> a -> Bool) -> [(a, a)] -> Int
countWhere f = length . filter (uncurry f)
{-# INLINE countWhere #-}

-- | Parse newline-separated pairs of intervals
parsePairs :: Parser [(Interval, Interval)]
parsePairs = parsePair `sepBy1` endOfLine

-- | Parse two comma-separted intervals
parsePair :: Parser (Interval, Interval)
parsePair = (,)
  <$> parseInterval
  <*  char ','
  <*> parseInterval

-- | Parse an 'Interval' from two hyphen-separated integers
parseInterval :: Parser Interval
parseInterval = Interval.new
  <$> decimal
  <*  char '-'
  <*> decimal
