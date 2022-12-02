module Advent.Day01Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing parseGroups 1 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 24000
    part1 problem `shouldBe` 69795

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 45000
    part2 problem `shouldBe` 208437

-- | Sum of largest group
part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

-- | Sum of three largest groups
part2 :: [[Int]] -> Int
part2 = done . foldl' step (0, 0, 0) . fmap sum
 where
  done (x, y, z) = x + y + z
  step (x, y, z) i
    | i < x     = (x, y, z)
    | i < y     = (i, y, z)
    | i < z     = (y, i, z)
    | otherwise = (y, z, i)

-- | Parse groups separated by newlines
parseGroups :: Parser [[Int]]
parseGroups = parseGroup `sepBy1` twoNewlines

-- | Each group is one or more decimal number separated by newlines
parseGroup :: Parser [Int]
parseGroup = decimal `sepBy1` endOfLine
