module Advent.Counter
  ( Counter
  , empty
  , increment
  , decrement
  , union
  , intersection
  , unions
  , intersections
  , counts
  , size
  , mostCommon
  , leastCommon
  , into
  , fromList
  ) where

import Advent.Prelude hiding (empty)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Foldable as Foldable

-- | Count the number of times a given key is encountered
newtype Counter k = Counter
  { unCounter :: HashMap k Int
  }
  deriving newtype (Eq, Show, Ord)

instance Hashable k => Semigroup (Counter k) where
  (<>) = union
  {-# INLINE (<>) #-}

instance Hashable k => Monoid (Counter k) where
  mempty = empty
  {-# INLINE mempty #-}

-- | Empty 'Counter'
empty :: Hashable k => Counter k
empty = Counter mempty
{-# INLINE empty #-}

-- | Increment an element's count
increment :: Hashable k => k -> Counter k -> Counter k
increment k = Counter . HashMap.insertWith (+) k 1 . unCounter
{-# INLINE increment #-}

-- | Decrement an element's count
decrement :: Hashable k => k -> Counter k -> Counter k
decrement k = Counter . HashMap.alter dec k . unCounter
 where
  dec mv = do
    v <- mv
    pred v <$ guard (v > 1)

-- | Union of two 'Counter's
union :: Hashable k => Counter k -> Counter k -> Counter k
union (Counter lhs) (Counter rhs) = Counter $ HashMap.unionWith (+) lhs rhs
{-# INLINE union #-}

-- | Intersection of two 'Counter's
intersection :: Hashable k => Counter k -> Counter k -> Counter k
intersection (Counter lhs) (Counter rhs) = Counter $ HashMap.intersectionWith (+) lhs rhs
{-# INLINE intersection #-}

-- | Union of zero or more 'Counter's
unions :: Hashable k => [Counter k] -> Counter k
unions = foldr union empty
{-# INLINE unions #-}

-- | Intersection of zero or more 'Counter's
--
-- An empty input always produces the empty 'Counter'
--
intersections :: Hashable k => [Counter k] -> Counter k
intersections = \case
  [] -> empty
  c:cs -> foldr intersection c cs
{-# INLINE intersections #-}

-- | Convert 'Counter' to list of keys and their counts
counts :: Counter k -> [(k, Int)]
counts = HashMap.toList . unCounter
{-# INLINE counts #-}

-- | Number of elements being tracked
size :: Counter k -> Int
size = HashMap.size . unCounter
{-# INLINE size #-}

-- | Retrieve keys from most to least common
mostCommon :: Counter k -> [k]
mostCommon = sorted (Down . snd)
{-# INLINE mostCommon #-}

-- | Retrieve keys from least to most common
leastCommon :: Counter k -> [k]
leastCommon = sorted snd
{-# INLINE leastCommon #-}

-- | Sort keys by some projection on the counts
sorted :: Ord v => ((k, Int) -> v) -> Counter k -> [k]
sorted project = fmap fst . sortOn project . counts
{-# INLINE sorted #-}

-- | Build a 'Counter' from a sequence
into :: (Hashable k, Foldable f) => f k -> Counter k
into = fromList . Foldable.toList
{-# INLINE into #-}

-- | Build a 'Counter' from a list
fromList :: Hashable k => [k] -> Counter k
fromList = Counter . HashMap.fromListWith (+) . fmap (,1)
{-# INLINE fromList #-}
