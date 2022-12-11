{-# LANGUAGE StrictData #-}

module Advent.Day09Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Advent.Vec2 (Vec2, zero)
import qualified Advent.Vec2 as Vec2
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

spec :: Spec
spec = parsing parseCommands 9 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 13
    part1 problem `shouldBe` 6037

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 1
    part2 problem `shouldBe` 2485

-- | Solve with 2-element rope
part1 :: [Vec2 Int] -> Int
part1 = solve 2

-- | Solve with 10-element rope
part2 :: [Vec2 Int] -> Int
part2 = solve 10

-- | Solve with n-element rope
solve :: Int -> [Vec2 Int] -> Int
solve n = Set.size . snd . flip execState start . traverse_ step
 where
  -- State is each element's current position and the set of positions
  -- visited by the last element
  start :: (Map Int (Vec2 Int), Set (Vec2 Int))
  start =
    ( Map.fromList $ (,zero) <$> [1..n]
    , Set.singleton zero
    )

  -- Move head of tail, then move every other element in response to
  -- the previous element
  step v = do
    move 1 v
    traverse_ moveTail [2..n]

  -- Add vector to i's position, updating the visited set for the last
  -- element only
  move i v = modify $ \(area, seen) -> do
    let pos = v + (area ! i)
    let saw = bool zero pos $ i == n
    (Map.insert i pos area, Set.insert saw seen)

  -- Move this element if it's more than 1 space away
  moveTail i = do
    area <- gets fst
    let d = area ! (i - 1) - area ! i
    when (abs (d ^. Vec2.x) > 1 || abs (d ^. Vec2.y) > 1) $
      move i $ signum d

-- | One command per line
parseCommands :: Parser [Vec2 Int]
parseCommands = mconcat <$> parseCommand `sepBy1` endOfLine

-- | Direction + count
parseCommand :: Parser [Vec2 Int]
parseCommand = do
  dir <- asum
    [ Vec2.up    <$ sym "U"
    , Vec2.down  <$ sym "D"
    , Vec2.left  <$ sym "L"
    , Vec2.right <$ sym "R"
    ]
  flip replicate dir <$> decimal
