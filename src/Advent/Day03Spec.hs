module Advent.Day03Spec
  ( spec
  ) where

import Advent.Prelude

import qualified Advent.Counter as Counter
import Advent.Input
import Advent.List
import Advent.Parse
import Data.List.Split (chunksOf)

spec :: Spec
spec = parsing parseSacks 3 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 157
    part1 problem `shouldBe` 7824

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 70
    part2 problem `shouldBe` 2798

-- | Sum of priorities for values that show up twice in each line
part1 :: [[Int]] -> Int
part1 sacks = sum $ do
  ~(front, back) <- splitHalf <$> sacks
  Counter.mostCommon $ Counter.into front `Counter.intersection` Counter.into back

-- | Sum of priorities for values that show up twice in each 3-line group
part2 :: [[Int]] -> Int
part2 sacks = sum $ do
  chunk <- chunksOf 3 $ Counter.into <$> sacks
  Counter.mostCommon $ Counter.intersections chunk

-- | Value of each 'Char'
--
-- @
-- a - z => 01 - 26
-- A - Z => 27 - 57
-- @
--
priority :: Char -> Int
priority (ord -> code) =
  code - if code >= lower then lower else upper
 where
  lower = ord 'a' - 1
  upper = ord 'A' - 26 - 1
{-# INLINE priority #-}

-- | Parse newline-separated sequences of letters
parseSacks :: Parser [[Int]]
parseSacks = many (priority <$> letter) `sepBy1` endOfLine
