module Advent.Day02Spec
  ( spec
  ) where

import Advent.Prelude

import Advent.Input
import Advent.Parse

spec :: Spec
spec = parsing parseRounds 2 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 15
    part1 problem `shouldBe` 15691

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 12
    part2 problem `shouldBe` 12989

-- | Interpret each round as "play this move"
part1 :: [(Move, XYZ)] -> Int
part1 = sum . fmap (uncurry play . fmap fromXYZ)
 where
  play S  me@P = score L me
  play S  me@R = score W me
  play R  me@S = score L me
  play R  me@P = score W me
  play P  me@R = score L me
  play P  me@S = score W me
  play _  me   = score D me

-- | Interpret each round as "solve for this outcome"
part2 :: [(Move, XYZ)] -> Int
part2 = sum . fmap (uncurry play . fmap fromXYZ)
 where
  play S  me@L = score me P
  play S  me@W = score me R
  play R  me@L = score me S
  play R  me@W = score me P
  play P  me@L = score me R
  play P  me@W = score me S
  play ye me@D = score me ye

-- | Compute raw score
--
-- The score for a given round is the sum of outcome:
--   Loss    => 0
--   Draw    => 3
--   Win     => 6
--
-- and the move's point value:
--  Rock     => 1
--  Paper    => 2
--  Scissors => 3
--
score :: Outcome -> Move -> Int
score outcome move = 3 * fromEnum outcome + fromEnum move + 1

-- | Interpret 'XYZ' as some other enumeration
fromXYZ :: Enum a => XYZ -> a
fromXYZ = toEnum . fromEnum

-- | Generic representation of my instructions
data XYZ = X | Y | Z
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Interpret my instructions as "rock, paper, scissors"
data Move = R | P | S
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Interpret my instructions as "lose, draw, win"
data Outcome = L | D | W
  deriving (Eq, Ord, Show, Bounded, Enum)

-- | Parse rounds separated by whitespace
parseRounds :: Parser [(Move, XYZ)]
parseRounds = many parseRound
 where
  parseRound = (,)
    <$> asum [R <$ sym "A", P <$ sym "B", S <$ sym "C"]
    <*> asum [X <$ sym "X", Y <$ sym "Y", Z <$ sym "Z"]
