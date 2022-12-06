{-# LANGUAGE StrictData #-}

module Advent.CountingQueue
  ( CountingQueue
  , empty
  , push
  , pop
  , drop
  , shift
  , distinct
  , into
  , fromList
  ) where

import Advent.Prelude hiding (empty, drop)

import qualified Data.Sequence as Seq
import Advent.Counter (Counter)
import qualified Advent.Counter as Counter
import qualified Data.Foldable as Foldable

-- | A queue that tracks how often each element is encountered
data CountingQueue a = CountingQueue
  { counter :: Counter a
  , queue :: Seq a
  }
  deriving stock (Eq, Show, Ord)

instance Hashable k => Semigroup (CountingQueue k) where
  CountingQueue c1 q1 <> CountingQueue c2 q2 = CountingQueue (c1 <> c2) (q1 <> q2)
  {-# INLINE (<>) #-}

instance Hashable k => Monoid (CountingQueue k) where
  mempty = empty
  {-# INLINE mempty #-}

-- | Empty 'CountingQueue'
empty :: Hashable k => CountingQueue k
empty = CountingQueue mempty mempty
{-# INLINE empty #-}

-- | Push an element on the back of the 'CountingQueue'
push :: Hashable k => CountingQueue k -> k -> CountingQueue k
push CountingQueue{..} k = CountingQueue
  { counter = Counter.increment k counter
  , queue = queue |> k
  }
{-# INLINE push #-}

-- | Attempt to pop an element from the front of the 'CountingQueue'
pop :: Hashable k => CountingQueue k -> Maybe (k, CountingQueue k)
pop CountingQueue{..} = case viewl queue of
  EmptyL -> Nothing
  k :< rest -> pure
    ( k
    , CountingQueue
      { counter = Counter.decrement k counter
      , queue = rest
      }
    )

-- | Drop the element at the front of the 'CountingQueue' or return the original
drop :: Hashable k => CountingQueue k -> CountingQueue k
drop q = maybe q snd $ pop q
{-# INLINE drop #-}

-- | 'drop' followed by a 'push'
shift :: Hashable k => CountingQueue k -> k -> CountingQueue k
shift q = push $ drop q
{-# INLINE shift #-}

-- | Count number of distinct elements
distinct :: CountingQueue k -> Int
distinct CountingQueue{..} = Counter.size counter
{-# INLINE distinct #-}

-- | Build a 'CountingQueue' from a sequence
into :: (Hashable k, Foldable f) => f k -> CountingQueue k
into = fromList . Foldable.toList
{-# INLINE into #-}

-- | Build a 'CountingQueue' from a list
fromList :: Hashable k => [k] -> CountingQueue k
fromList xs = CountingQueue
  { counter = Counter.fromList xs
  , queue = Seq.fromList xs
  }
{-# INLINE fromList #-}
