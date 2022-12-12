{-# LANGUAGE StrictData #-}

module Advent.Vec2
  ( Vec2
  , Point2
  , new
  , x
  , y
  , zero
  , fromPair
  , toPair
  , up
  , down
  , left
  , right
  , neighbors4
  ) where

import Advent.Prelude

-- | Opaque 2D vector
data Vec2 a = Vec2
  { _x :: a
  , _y :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

-- | Exposed constructor
new :: a -> a -> Vec2 a
new = Vec2
{-# INLINE new #-}

-- | Lens for @x@ component
x :: Lens' (Vec2 a) a
x = lens _x $ \s b -> s { _x = b }
{-# INLINE x #-}

-- | Lens for @y@ component
y :: Lens' (Vec2 a) a
y = lens _y $ \s b -> s { _y = b }
{-# INLINE y #-}

instance Show a => Show (Vec2 a) where
  showsPrec d Vec2 {..}
    = showParen (d > 10)
    $ showString "Vec2.new "
    . showsPrec (d + 1) _x
    . showChar ' '
    . showsPrec (d + 1) _y

-- | Use vector as a point
type Point2 = Vec2

instance Applicative Vec2 where
  pure a = Vec2 a a
  {-# INLINE pure #-}
  Vec2 f g <*> Vec2 a b = Vec2 (f a) (g b)
  {-# INLINE (<*>) #-}

-- | Lift numeric operations through 'Vec2'
instance Num a => Num (Vec2 a) where
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}

  (+) = liftA2 (+)
  {-# INLINE (+) #-}

  (*) = liftA2 (*)
  {-# INLINE (*) #-}

  (-) = liftA2 (-)
  {-# INLINE (-) #-}

  negate = fmap negate
  {-# INLINE negate #-}

  abs = fmap abs
  {-# INLINE abs #-}

  signum = fmap signum
  {-# INLINE signum #-}

-- | Convert pair to 'Vec2'
fromPair :: forall a. (a, a) -> Vec2 a
fromPair = uncurry Vec2
{-# INLINE fromPair #-}

-- | Convert 'Vec2' to pair
toPair :: forall a. Vec2 a -> (a, a)
toPair Vec2{..} = (_x, _y)
{-# INLINE toPair #-}

-- | Convenience zero value
zero :: forall a. Num a => Vec2 a
zero = pure 0
{-# INLINE zero #-}

-- | Unit vector up
up :: forall a. Num a => Vec2 a
up = Vec2 0 1
{-# INLINE up #-}

-- | Unit vector down
down :: forall a. Num a => Vec2 a
down = Vec2 0 (-1)
{-# INLINE down #-}

-- | Unit vector left
left :: forall a. Num a => Vec2 a
left = Vec2 (-1) 0
{-# INLINE left #-}

-- | Unit vector right
right :: forall a. Num a => Vec2 a
right = Vec2 1 0
{-# INLINE right #-}

-- | Cardinal neighbors of point
neighbors4 :: forall a. Num a => Vec2 a -> [Vec2 a]
neighbors4 p = (p +) <$> [up, down, left, right]
{-# INLINE neighbors4 #-}
