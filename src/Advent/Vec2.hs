{-# LANGUAGE StrictData #-}

module Advent.Vec2
  ( Vec2(..)
  , Point2
  , zero
  , fromPair
  , toPair
  , up
  , down
  , left
  , right
  ) where

import Advent.Prelude

-- | 2D vector
data Vec2 a = Vec2
  { x :: a
  , y :: a
  }
  deriving stock (Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (Hashable)

instance Show a => Show (Vec2 a) where
  showsPrec _ Vec2 {..}
    = showChar '('
    . shows x
    . showString ", "
    . shows y
    . showChar ')'

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
toPair Vec2{..} = (x, y)
{-# INLINE toPair #-}

zero :: forall a. Num a => Vec2 a
zero = pure 0
{-# INLINE zero #-}

up :: forall a. Num a => Vec2 a
up = Vec2 0 1
{-# INLINE up #-}

down :: forall a. Num a => Vec2 a
down = Vec2 0 (-1)
{-# INLINE down #-}

left :: forall a. Num a => Vec2 a
left = Vec2 (-1) 0
{-# INLINE left #-}

right :: forall a. Num a => Vec2 a
right = Vec2 1 0
{-# INLINE right #-}
