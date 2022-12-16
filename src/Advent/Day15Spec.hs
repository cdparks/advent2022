module Advent.Day15Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Advent.Vec2 (Vec2)
import qualified Advent.Vec2 as Vec2
import Advent.Interval (Interval)
import qualified Advent.Interval as Interval

spec :: Spec
spec = parsing parseSensorBeacons 15 $ do
  it "1" $ \Input{..} -> do
    part1 10 example `shouldBe` 26
    part1 2000000 problem `shouldBe` 5256611

  it "2" $ \Input{..} -> do
    part2 20 example `shouldBe` Just 56000011
    part2 4000000 problem `shouldBe` Just 13337919186981

-- | Sum of the uncovered points along the line crossing y
part1 :: Int -> [Sensor] -> Int
part1 y = sum . fmap diff . intervals y
 where
  diff = uncurry subtract . Interval.toPair

-- | For each sensor, find any point right outside its range not
-- covered by any other sensor
part2 :: Int -> [Sensor] -> Maybe Int
part2 maxY sensors = listToMaybe $ do
  p <- fringe =<< sensors
  let (px, py) = Vec2.toPair p
  guard $ inRange px && inRange py
  guard $ not $ any (`contains` p) sensors
  pure $ px * 4000000 + py
 where
  inRange v = 0 <= v && v <= maxY

-- | Find intervals covered by sensors along the line crossing y
intervals :: Int -> [Sensor] -> [Interval]
intervals y sensors = Interval.merge $ do
  Sensor{..} <- sensors
  let dx = radius - abs (sensor ^. Vec2.y - y)
  guard $ dx >= 0
  pure $ Interval.new (sensor ^. Vec2.x - dx) (sensor ^. Vec2.x + dx)

-- | Each point 1 unit beyond the sensor's range
fringe :: Sensor -> [Vec2 Int]
fringe Sensor{..} = do
  let (sx, sy) = Vec2.toPair sensor
  let d = succ radius
  y <- [sy - d .. sy + d]
  let dx = d - abs (sy - y)
  [Vec2.new (sx - dx) y] <> [Vec2.new (sx + dx) y | dx /= 0]

-- | Does sensor's range include this point?
contains :: Sensor -> Vec2 Int -> Bool
contains Sensor{..} p = Vec2.manhattan sensor p <= radius

-- | One sensor-beacon pair per line
parseSensorBeacons :: Parser [Sensor]
parseSensorBeacons = parseSensorBeacon `sepBy1` endOfLine

-- | Parse sensor, beacon, and calculate manhattan distance between them
parseSensorBeacon :: Parser Sensor
parseSensorBeacon = do
  void "Sensor at "
  sensor <- parsePoint
  void ": closest beacon is at "
  beacon <- parsePoint
  let radius = Vec2.manhattan sensor beacon
  pure Sensor{..}

-- | Parse 2D point
parsePoint :: Parser (Vec2 Int)
parsePoint =  Vec2.new
  <$  "x="
  <*> signed decimal
  <* ", y="
  <*> signed decimal

data Sensor = Sensor
  { sensor :: Vec2 Int
  , beacon :: Vec2 Int
  , radius :: Int
  }
  deriving (Eq, Show)
