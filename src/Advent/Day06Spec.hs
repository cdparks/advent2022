module Advent.Day06Spec
  ( spec
  ) where

import Advent.Prelude

import qualified Advent.CountingQueue as CountingQueue
import Advent.Input

spec :: Spec
spec = reading 6 $ do
  it "1" $ \Input{..} -> do
    part1 <$> lines example `shouldBe` [7, 5, 6, 10, 11]
    part1 problem `shouldBe` 1531

  it "2" $ \Input{..} -> do
    part2 <$> lines example `shouldBe` [19, 23, 23, 29, 26]
    part2 problem `shouldBe` 2518

-- | Find the end of the first distinct sequence of length 4
part1 :: Text -> Int
part1 = distinctSequence 4

-- | Find the end of the first distinct sequence of length 14
part2 :: Text -> Int
part2 = distinctSequence 14

-- | Find the end of the first distinct sequence of the specified length
distinctSequence :: Int -> Text -> Int
distinctSequence i = uncurry (go i . CountingQueue.into) . splitAt i . unpack
 where
  go !acc q = \case
    [] -> acc
    c:cs
      | CountingQueue.distinct q == i -> acc
      | otherwise -> go (acc + 1) (CountingQueue.shift q c) cs
