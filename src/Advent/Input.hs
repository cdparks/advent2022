{-# LANGUAGE StrictData #-}

module Advent.Input
  ( Input(..)
  , parsing
  , reading
  , parseInputs
  , readInputs
  ) where

import Advent.Prelude

import Advent.Parse (Parser, parseIO)
import Text.Printf (printf)

-- | Read and parse inputs
parseInputs :: Parser a -> Int -> IO (Input a)
parseInputs parser = traverse (parseIO parser) <=< readInputs

-- | Read inputs
readInputs :: Int -> IO (Input Text)
readInputs day = Input
  <$> readFile ("inputs" </> "test" <> formatted)
  <*> readFile ("inputs" </> "day" <> formatted)
 where
  formatted = printf "%02d" day

-- | Parse inputs before all tests in a spec group run
parsing :: Parser a -> Int -> SpecWith (Input a) -> Spec
parsing parser = beforeAll . parseInputs parser

-- | Read inputs before all tests in a spec group run
reading :: Int -> SpecWith (Input Text) -> Spec
reading = beforeAll . readInputs

-- | Some test and problem input data
data Input a = Input
  { testInput :: a
  , problemInput :: a
  }
  deriving (Functor, Foldable, Traversable)
