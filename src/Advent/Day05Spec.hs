module Advent.Day05Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse hiding (take)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IntMap

spec :: Spec
spec = parsing parseProblem 5 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` "CMZ"
    part1 problem `shouldBe` "HBTMTBSDC"

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` "MCD"
    part2 problem `shouldBe` "PQTJRSHWS"

-- | Move each crate within a 'Move' one at a time
part1 :: Problem -> String
part1 Problem{..} = do
  let step m@Move{..} = replicateM_ _count $ move 1 m
  tops $ execState (traverse_ step _moves) _mapping

-- | Move each crate within a 'Move' all at once
part2 :: Problem -> String
part2 Problem{..} = do
  let step m@Move{..} = move _count m
  tops $ execState (traverse_ step _moves) _mapping

-- | Transfer /n/ elements using the 'Move' instruction
move :: MonadState (IntMap String) m => Int -> Move -> m ()
move n Move{..} = do
  stack <- gets (! _from)
  modify
    $ IntMap.insertWith (<>) _to (take n stack)
    . IntMap.insert _from (drop n stack)

-- | Get the top crate from each column
tops :: IntMap String -> String
tops = concatMap (take 1) . IntMap.elems

-- | Parse start state and move instructions
parseProblem :: Parser Problem
parseProblem = do
  stacks <- transpose <$> parseRows
  endOfLine
  columns <- parseColumns
  twoNewlines
  let mapping = IntMap.fromList $ zip columns (catMaybes <$> stacks)
  Problem mapping <$> parseMoves

-- | Parse each row of crates
parseRows :: Parser [[Maybe Char]]
parseRows = parseRow `sepBy` endOfLine

-- | Parse a single row of crates
parseRow :: Parser [Maybe Char]
parseRow = parseCrate `sepBy1` char ' '

-- | Parse a single crate
--
-- Whitespace is significant, since an empty crate is represented by
-- three spaces.
--
parseCrate :: Parser (Maybe Char)
parseCrate = asum
  [ Nothing <$ count 3 space
  , Just <$> (char '[' *> anyChar <* char ']')
  ]

-- | Parse the column ids
parseColumns :: Parser [Int]
parseColumns = parseColumn `sepBy1` space

-- | Parse a single column id
parseColumn :: Parser Int
parseColumn = space *> decimal <* space

-- | Parse newline-separated moves
parseMoves :: Parser [Move]
parseMoves = parseMove `sepBy1` endOfLine

-- | Parse a single move
parseMove :: Parser Move
parseMove = Move
  <$  void "move "
  <*> decimal
  <*  void " from "
  <*> decimal
  <*  void " to "
  <*> decimal

data Problem = Problem
  { _mapping :: IntMap String
  , _moves :: [Move]
  }

data Move = Move
  { _count :: Int
  , _from :: Int
  , _to :: Int
  }
