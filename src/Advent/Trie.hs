module Advent.Trie
  ( Trie
  , singleton
  , insert
  , lookup
  , fromList
  , toList
  , value
  , children
  , flowUp
  ) where

import Advent.Prelude hiding (lookup, toList)

import qualified Data.HashMap.Strict as HashMap

-- | Trie with optionally @v@-labeled nodes and @k@ valued paths
data Trie k v = Trie
  { value :: Maybe v
  , children :: HashMap k (Trie k v)
  }
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance (Show k, Show v) => Show (Trie k v) where
  show = ("Trie.fromList " <>) . show . toList

instance (Hashable k, Semigroup v) => Semigroup (Trie k v) where
  Trie v1 c1 <> Trie v2 c2 = Trie (v1 <> v2) $ HashMap.unionWith (<>) c1 c2

instance (Hashable k, Semigroup v) => Monoid (Trie k v) where
  mempty = Trie mempty mempty

-- | Insert a value at the end of the specified path
insert :: (Hashable k, Semigroup v) => [k] -> v -> Trie k v -> Trie k v
insert ps v = (singleton ps v <>)

-- | Monoidally combine each level with the levels below it
flowUp :: (Hashable k, Semigroup v) => Trie k v -> Trie k v
flowUp (Trie v cs) = do
  let children = HashMap.map flowUp cs
  Trie { children, value = v <> foldMap value (HashMap.elems children) }

-- | Attempt to get the value at this path
lookup :: Hashable k => [k] -> Trie k v -> Maybe v
lookup ps t = go t ps
 where
  go Trie{..} = \case
    [] -> value
    k:ks -> do
      child <- HashMap.lookup k children
      go child ks

-- | Build a 'Trie' with a single path
singleton :: Hashable k => [k] -> v -> Trie k v
singleton ps v = go ps
 where
  go = \case
    [] -> Trie (Just v) mempty
    k:ks -> Trie Nothing $ HashMap.singleton k $ go ks

-- | Build a 'Trie' from a sequence of paths and values
fromList :: (Hashable k, Semigroup v) => [([k], v)] -> Trie k v
fromList paths = foldMap (uncurry singleton) paths

-- | Convert a 'Trie' back into a sequence of paths and values
toList :: Trie k v -> [([k], v)]
toList = go []
 where
  go path Trie{..} = do
    let front = maybe id ((:) . (path,)) value
    front $ do
      (k, t) <- HashMap.toList children
      go (path <> [k]) t
