{-# LANGUAGE StrictData #-}

module Advent.Day11Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse hiding (take)
import Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Sequence as Seq

spec :: Spec
spec = parsing parseMonkeys 11 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 10605
    part1 problem `shouldBe` 101436

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 2713310158
    part2 problem `shouldBe` 19754471646

-- | Run for 20 rounds, dividing worry by 3 each time
part1 :: IntMap Monkey -> Int
part1 = run 20 (`div` 3)

-- | Run for 10K rounds, w/ worry modulo the product of all divisors
part2 :: IntMap Monkey -> Int
part2 monkeys = run 10000 (`mod` d) monkeys
 where
  d = product $ divisor <$> IntMap.elems monkeys

-- | Run for /n/ rounds, modifying worry with the specified function
run :: Int -> (Int -> Int) -> IntMap Monkey -> Int
run n simplify = done . execState (replicateM_ n step)
 where
  -- Product of the two largest inspection values
  done :: IntMap Monkey -> Int
  done = product . take 2 . sortOn Down . fmap inspections . IntMap.elems

  -- Run a single round
  -- * Each monkey gets a chance to drain its queue
  -- * A monkey may place items onto the back of another monkey
  step :: State (IntMap Monkey) ()
  step = do
    keys <- gets IntMap.keys
    for_ keys $ \key -> do
      Monkey{..} <- gets (! key)
      let len = length queue
      for_ queue $ \item -> do
        let worry = simplify $ operation item
        push worry $ target worry
      update key $ \monkey -> monkey
        { queue = mempty
        , inspections = inspections + len
        }

  -- Add worry to the back of the specified monkey's queue
  push :: Int -> Int -> State (IntMap Monkey) ()
  push worry key = update key $ \monkey -> monkey
    { queue = queue monkey |> worry
    }

  -- Update the monkey at this key
  update :: Int -> (Monkey -> Monkey) -> State (IntMap Monkey) ()
  update key = modify . flip IntMap.adjust key

-- | Monkeys are separated by two newlines
parseMonkeys :: Parser (IntMap Monkey)
parseMonkeys = IntMap.fromList <$> parseMonkey `sepBy1` endOfLine

-- | Very fragile parsing
parseMonkey :: Parser (Int, Monkey)
parseMonkey = do
  i <- parseId
  queue <- parseQueue
  operation <- parseOperation
  divisor <- parseTrailingInt
  target <- parseTarget divisor
  pure (i, Monkey{ inspections = 0, ..})

-- | Ignore everything except the number, followed by a colon
parseId :: Parser Int
parseId = do
  skipWhile $ not . isDigit
  decimal <* char ':' <* endOfLine

-- | Ignore everything except commma-separated numbers
parseQueue :: Parser (Seq Int)
parseQueue = do
  skipWhile $ not . isDigit
  Seq.fromList <$> decimal `sepBy1` ", " <* endOfLine

-- | Build a function from the operation and its right-hand-argument
parseOperation :: Parser (Int -> Int)
parseOperation = do
  skipWhile (/= '=')
  void "= old "
  op <- (+) <$ "+ " <|> (*) <$ "* "
  op <$> decimal <|> twice op <$ "old" <* endOfLine
 where
  twice f a = f a a

-- | Build a function choosing the true or false target
parseTarget :: Int -> Parser (Int -> Int)
parseTarget d =
  target <$> parseTrailingInt <*> parseTrailingInt
 where
  target t f = bool f t . (== 0) . (`mod` d)

-- | Ignore everything except the number
parseTrailingInt :: Parser Int
parseTrailingInt = do
  skipWhile $ not . isDigit
  decimal <* endOfLine

-- | A "monkey" maintains a queue of items to process and can throw
-- items to other monkeys
data Monkey = Monkey
  { queue :: Seq Int
  , divisor :: Int
  , operation :: Int -> Int
  , target :: Int -> Int
  , inspections :: Int
  }
