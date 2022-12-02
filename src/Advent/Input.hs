{-# LANGUAGE StrictData #-}

module Advent.Input
  ( Input(..)
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

-- | Some test and problem input data
data Input a = Input
  { testInput :: a
  , problemInput :: a
  }
  deriving (Functor, Foldable, Traversable)
