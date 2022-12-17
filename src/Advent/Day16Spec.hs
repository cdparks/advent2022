module Advent.Day16Spec
  ( spec
  ) where

import Advent.Prelude hiding (take)

import Advent.Input
import Advent.Parse
import Data.Bits (Bits(..))
import Data.HashMap.Strict ((!))
import qualified Data.HashMap.Strict as HashMap

spec :: Spec
spec = parsing parseNodes 16 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 1651
    part1 problem `shouldBe` 1737

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 1707
    part2 problem `shouldBe` 2216

-- | Find maximum pressure we can release in 30 minutes
part1 :: Network -> Int
part1 = maximum . HashMap.elems . getScores 30

-- | Find maximum pressure two parties can release in 26 minutes
-- by opening disjoint sets of valves
part2 :: Network -> Int
part2 network = maximum $ do
  (v1, s1) <- HashMap.toList scores
  (v2, s2) <- HashMap.toList scores
  s1 + s2 <$ guard (v1 .&. v2 == 0)
 where
  scores = getScores 26 network

-- | Mapping from valv bitsets to maximum pressure released in the
-- specified time
getScores :: Int -> Network -> HashMap Word64 Int
getScores maxTime network@Network{..} =
  execState (visit maxTime 0 0 $ _index ! "AA") mempty
 where
  visit :: Int -> Word64 -> Int -> Int -> State (HashMap Word64 Int) ()
  visit !time !seen !score !i = do
    modify $ HashMap.alter (maxScore score) seen
    for_ rates $ \(j, rate) -> do
      let t = pred $ time - distMap ! (i, j)
      when (t > 0 && not (seen `testBit` j)) $
        visit t (seen `setBit` j) (score + t * rate) j

  maxScore :: Int -> Maybe Int -> Maybe Int
  maxScore score = Just . maybe score (max score)

  rates :: [(Int, Int)]
  rates = do
    (i, Node{..}) <- HashMap.toList _nodes
    guard $ _rate > 0
    pure (i, _rate)

  distMap :: HashMap (Int, Int) Int
  distMap = shortestPaths 10000 network

-- | Breadth first search starting at every node
shortestPaths :: Int -> Network -> HashMap (Int, Int) Int
shortestPaths maxDist Network{..} = HashMap.fromList $ do
  i <- HashMap.keys _nodes
  let dist = execState (bfs [(0, i)]) mempty
  j <- HashMap.keys _nodes
  pure ((i, j), HashMap.findWithDefault maxDist j dist)
 where
  bfs = \case
    [] -> pure ()
    (d, j) : qs -> do
      seen <- gets $ HashMap.member j
      unless seen $ modify $ HashMap.insert j d
      bfs $ qs <> [(succ d, n) | not seen, n <- _neighbors (_nodes ! j)]

-- | One node per line
parseNodes :: Parser Network
parseNodes = toNetwork <$> parseNode `sepBy1` endOfLine

-- | Node, flow rate, list of neighbors
parseNode :: Parser (Text, Int, [Text])
parseNode = (,,)
  <$  "Valve "
  <*> take 2
  <*  " has flow rate="
  <*> decimal
  <*  traverse_ plural ["; tunnel", "lead", "to valve"]
  <*> take 2 `sepBy1` ", "

-- | Optionally parse a trailing "s"
plural :: Parser Text -> Parser ()
plural x = void x <* optional "s" <* " "

-- | Convert raw input to 'Network'
--
-- Map each node to an index in 1..N. The index is the position of the
-- bit to set in a 64-bit word when that valve is open.
toNetwork :: [(Text, Int, [Text])] -> Network
toNetwork input = Network nodes index
 where
  index :: HashMap Text Int
  index = HashMap.fromList $ do
    ((name, _, _), i) <- zip input [1..]
    pure (name, i)

  nodes :: HashMap Int Node
  nodes = HashMap.fromList $ do
    (name, rate, neighbors) <- input
    pure
      ( index ! name
      , Node
        { _name = name
        , _rate = rate
        , _neighbors = (index !) <$> neighbors
        }
      )

-- | Mapping from name to index, and index to node
data Network = Network
  { _nodes :: HashMap Int Node
  , _index :: HashMap Text Int
  }
  deriving Show

-- | Name, flow rate, neighbors' indices
data Node = Node
  { _name :: Text
  , _rate :: Int
  , _neighbors :: [Int]
  }
  deriving Show
