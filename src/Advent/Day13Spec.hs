module Advent.Day13Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing parsePairs 13 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 13
    part1 problem `shouldBe` 4821

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 140
    part2 problem `shouldBe` 21890

-- | Sum of the indices of the correctly ordered pairs
part1 :: [(Tree, Tree)] -> Int
part1 = sum . indices (uncurry (<))

-- | Product of the indices of the dividers after sorting
part2 :: [(Tree, Tree)] -> Int
part2 = product . indices (`elem` dividers) . sort . (dividers <>) . uncurry (<>) . unzip
 where
  dividers =
    [ Node [Node [Leaf 2]]
    , Node [Node [Leaf 6]]
    ]

-- | One-based indices of elements satisfying predicate
indices :: (a -> Bool) -> [a] -> [Int]
indices p xs = do
  (i, x) <- zip [1..] xs
  i <$ guard (p x)

-- | Inputs are rose trees with integers at the leaves
data Tree
  = Leaf Int
  | Node [Tree]

-- | Derived 'Eq' won't match custom 'Ord'
instance Eq Tree where
  lhs == rhs = compare lhs rhs == EQ

-- | Almost a normal 'Ord' instance except that @Leaf `compare` Node@
-- converts the 'Leaf' to a 'Node'` and retries the comparison
instance Ord Tree where
  lhs `compare` rhs = case (lhs, rhs) of
    (Leaf x,  Leaf y)  -> x `compare` y
    (Node xs, Node ys) -> xs `compare` ys
    (Leaf {}, Node {}) -> Node [lhs] `compare` rhs
    (Node {}, Leaf {}) -> lhs `compare` Node [rhs]

-- | Sequence of pairs of trees
parsePairs :: Parser [(Tree, Tree)]
parsePairs = parsePair `sepBy1` endOfLine

-- | Pair of trees separated by newlines
parsePair :: Parser (Tree, Tree)
parsePair = (,)
  <$> parseTree
  <*  endOfLine
  <*> parseTree
  <*  endOfLine

-- | A tree is a bracketed sequence of integers or trees
parseTree :: Parser Tree
parseTree = "[" *> node <* "]"
 where
  node = Node <$> item `sepBy` ","
  item = Leaf <$> decimal <|> parseTree
