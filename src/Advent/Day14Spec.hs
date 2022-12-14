module Advent.Day14Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse hiding (Done)
import Advent.Vec2 (Vec2, zero)
import qualified Advent.Vec2 as Vec2
import qualified Data.HashSet as HashSet

spec :: Spec
spec = parsing parseCave 14 $ do
  it "1" $ \Input{..} -> do
    uncurry part1 example `shouldBe` 24
    uncurry part1 problem `shouldBe` 683

  it "2" $ \Input{..} -> do
    uncurry part2 example `shouldBe` 93
    uncurry part2 problem `shouldBe` 28821

-- | Count number of grains that come to rest, stopping once any fall
-- into the abyss
part1 :: Vec2 Int -> Cave -> Int
part1 bounds = simulate react
 where
  react p q
    | p == q = Done p
    | inBounds q = More q
    | otherwise = OutOfBounds

  inBounds p = and
    [ 0 <= p ^. Vec2.x
    , p ^. Vec2.x <= bounds ^. Vec2.x
    , 0 <= p ^. Vec2.y
    , p ^. Vec2.y <= bounds ^. Vec2.y
    ]

-- | Count the number of grains that come to rest, assuming that there
-- is an infinitely wide floor two units below the natural bounds of
-- the cave.
part2 :: Vec2 Int -> Cave -> Int
part2 bounds = simulate react
 where
  maxY = 2 + bounds ^. Vec2.y
  react p q
    | p == q = Done p
    | q ^. Vec2.y >= maxY = Done p
    | otherwise = More q

-- | Find the number of grains that stop according to some criteria
simulate
  :: (Vec2 Int -> Vec2 Int -> Reaction)
  -- ^ Decide what to do based on current and next point
  -> Cave
  -- ^ Set of blocked points
  -> Int
  -- ^ Number of grains at rest
simulate react = step 0 entry
 where
  entry = Vec2.new 500 0

  step !acc p ps = case fall p ps of
    Nothing -> acc
    Just q -> step (succ acc) entry $ HashSet.insert q ps

  fall p ps = do
    q <- next p ps
    case react p q of
      Done r -> pure r
      More r -> fall r ps
      OutOfBounds -> Nothing

  next p ps = listToMaybe $ do
    q <- (p +) <$> [Vec2.up, Vec2.up + Vec2.left, Vec2.up + Vec2.right, zero]
    q <$ guard (not $ q `HashSet.member` ps)

-- | What to do given current and next point
data Reaction
  = Done (Vec2 Int)
  | More (Vec2 Int)
  | OutOfBounds

-- | Parse cave and its maximum @x@ and @y@ coordinates
parseCave :: Parser (Vec2 Int, Cave)
parseCave = toCave <$> parsePath `sepBy1` endOfLine

-- | Points separated by arrows
parsePath :: Parser [Vec2 Int]
parsePath =
  expand <$> parsePoint `sepBy1` " -> "
 where
  expand path = uncurry segment =<< zip path (drop 1 path)

-- | 2D point
parsePoint :: Parser (Vec2 Int)
parsePoint = Vec2.new <$> decimal <* "," <*> decimal

-- | Convert sequence of lines into a 'Cave'
toCave :: [[Vec2 Int]] -> (Vec2 Int, Cave)
toCave = (getBounds &&& id) . HashSet.fromList . concat

-- | Maximum @x@ and @y@ coordinates
getBounds :: Cave -> Vec2 Int
getBounds =
  Vec2.fromPair . maxXY . unzip . fmap Vec2.toPair . HashSet.toList
 where
  maxXY = bimap maximum maximum

-- | Convert a pair of collinear points into a sequence of points
segment :: Vec2 Int -> Vec2 Int -> [Vec2 Int]
segment p0 q0 =
  (p+) <$> (Vec2.y0 <$> [0..dx]) <> (Vec2.x0 <$> [0..dy])
 where
  (p, q) = minmax p0 q0
  (dx, dy) = Vec2.toPair $ q - p

-- | Set of blocked spaces
type Cave = HashSet (Vec2 Int)
