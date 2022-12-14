module Advent.Day08Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse (Parser, digit, sepBy1, endOfLine)
import qualified Data.HashMap.Strict as HashMap
import Advent.Vec2 (Vec2)
import qualified Advent.Vec2 as Vec2

spec :: Spec
spec = parsing parseGrid 8 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 21
    part1 problem `shouldBe` 1785

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 8
    part2 problem `shouldBe` 345168

-- | Count trees that are visible from at least one edge
part1 :: Grid -> Int
part1 grid = flip execState 0 $
  for_ (HashMap.toList grid) $ \(pos, x) -> do
    let vis = any (all (< x) . scan grid pos) [minBound..]
    when vis $ modify succ

-- | Find the highest scenic score
part2 :: Grid -> Int
part2 grid = maximum $ do
  (pos, x) <- HashMap.toList grid
  pure $ product $ score grid pos x <$> [minBound..]

-- | Cardinal directions
data Direction = N | E | S | W
  deriving (Eq, Show, Ord, Bounded, Enum)

-- | Stream of all predecessors or successors of initial position in
-- some direction
scan :: Grid -> Vec2 Int -> Direction -> [Int]
scan grid (Vec2.toPair -> (j, i)) = \case
  N -> pick (repeat j) (preds  i)
  E -> pick (succs  j) (repeat i)
  S -> pick (repeat j) (succs  i)
  W -> pick (preds  j) (repeat i)
 where
  succs x = [x + 1 .. ]
  preds x = [x - 1, x - 2 .. ]
  pick xs = takeWhileJust . fmap (`HashMap.lookup` grid) . zipWith Vec2.new xs

-- | Score is the number of sequential trees (<) x, plus one for the
-- first tree that is (>=) x, if any.
score :: Grid -> Vec2 Int -> Int -> Direction -> Int
score grid pos x d =
  length $ lower <> take 1 rest
 where
  (lower, rest) = span (< x) $ scan grid pos d

-- | Take until we hit the first 'Nothing'
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = foldr step []
 where
  step m as = maybe [] (:as) m

-- | Parse a 2D grid from a sequence of newline separate rows
parseGrid :: Parser Grid
parseGrid = toGrid <$> parseRow `sepBy1` endOfLine

-- | A row is a sequence of digits
parseRow :: Parser [Int]
parseRow = some $ subtract (ord '0') . ord <$> digit

-- | Convert 2D list to a mapping from position to value
toGrid :: [[Int]] -> Grid
toGrid xxs = HashMap.fromList $ do
  (i, xs) <- zip [0..] xxs
  (j, x) <- zip [0..] xs
  pure (Vec2.new j i, x)

type Grid = HashMap (Vec2 Int) Int
