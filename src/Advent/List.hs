module Advent.List
  ( splitHalf
  ) where


import Advent.Prelude

-- | Break list in half without using 'length'
--
-- Simultaneously iterate over two copies of the list:
-- 1. One element at a time /and/
-- 2. Two elements at a time
--
-- When we reach the end of the second argument, the first argument is
-- the back half of the list, and we've already reconstructed the first
-- half of the list along the way.
--
-- See https://doisinkidney.com/posts/2019-05-08-list-manipulation-tricks.html
--
splitHalf :: [a] -> ([a], [a])
splitHalf xs = go xs xs
 where
  go (y:ys) (_:_:zs) = first (y:) (go ys zs)
  go ys _ = ([], ys)
