module Advent.Day01Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Parse

spec :: Spec
spec = do
  describe "1" $ do
    it "test" $ do
      groups <- parseFile parseGroups "inputs/test01"
      part1 groups `shouldBe` 24000

    it "problem" $ do
      groups <- parseFile parseGroups "inputs/day01"
      part1 groups `shouldBe` 69795

  describe "2" $ do
    it "test" $ do
      groups <- parseFile parseGroups "inputs/test01"
      part2 groups `shouldBe` 45000

    it "problem" $ do
      groups <- parseFile parseGroups "inputs/day01"
      part2 groups `shouldBe` 208437

-- | Sum of largest group
part1 :: [[Int]] -> Int
part1 = maximum . fmap sum

-- | Sum of three largest groups
part2 :: [[Int]] -> Int
part2 = done . foldl' step (0, 0, 0) . fmap sum
 where
  done (t1, t2, t3) = sum [t1, t2, t3]

  step (t1, t2, t3) i
    | i < t1 = (t1, t2, t3)
    | i < t2 = (i, t2, t3)
    | i < t3 = (t2, i, t3)
    | otherwise = (t2, t3, i)

-- | Parse groups separated by newlines
parseGroups :: Parser [[Int]]
parseGroups = parseGroup `sepBy1` twoNewlines

-- | Each group is one or more decimal number separated by newlines
parseGroup :: Parser [Int]
parseGroup = decimal `sepBy1` endOfLine
