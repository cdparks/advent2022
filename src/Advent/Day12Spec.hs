module Advent.Day12Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse
import Advent.Vec2 (Vec2)
import qualified Advent.Vec2 as Vec2
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Sequence as Seq

spec :: Spec
spec = parsing parseGrid 12 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` Just 31
    part1 problem `shouldBe` Just 350

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` Just 29
    part2 problem `shouldBe` Just 349

-- | Find path from 'S' to 'E' (backwards)
part1 :: Grid -> Maybe Int
part1 = bfs 'E' 'S'

-- | Find path from 'E' to the closest 'a'
part2 :: Grid -> Maybe Int
part2 = bfs 'E' 'a'

-- | Breadth-first-search from start to end
bfs :: Char -> Char -> Grid -> Maybe Int
bfs start end grid = do
  pos <- listToMaybe [pos | (pos, c) <- HashMap.toList grid, c == start]
  loop mempty $ Seq.singleton (0, pos)
 where
  loop seen q = case viewl q of
    EmptyL -> Nothing
    (!dist, !point) :< qs
      | grid ! point == end -> pure dist
      | point `HashSet.member` seen -> loop seen qs
      | otherwise -> do
        let ns = (succ dist,) <$> neighbors point
        loop (HashSet.insert point seen) $ foldl' (|>) qs ns

  neighbors point = do
    let here = height $ grid ! point
    neighbor <- Vec2.neighbors4 point
    there <- height <$> maybeToList (HashMap.lookup neighbor grid)
    neighbor <$ guard (here - there <= 1)

  height = \case
    'S' -> height 'a'
    'E' -> height 'z'
    c -> ord c - ord 'a'

-- | Parse a 2D grid from a sequence of newline separate rows
parseGrid :: Parser Grid
parseGrid = toGrid <$> some letter `sepBy1` endOfLine

-- | Convert 2D list to a mapping from position to value
toGrid :: [[Char]] -> Grid
toGrid xxs = HashMap.fromList $ do
  (i, xs) <- zip [0..] xxs
  (j, x) <- zip [0..] xs
  pure (Vec2.new j i, x)

type Grid = HashMap (Vec2 Int) Char
