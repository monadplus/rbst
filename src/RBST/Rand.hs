--------------------------------------------------------------------
-- |
-- Module      :  RBST.Rand
-- Copyright   :  (c) Arnau Abella 2020
-- License     :  MIT
-- Maintainer  :  arnauabell@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------
module RBST.Rand where

import qualified System.Random.Mersenne.Pure64 as Random

--   uniformRange :: ( PrimMonad m
--                 , Integral a, Bounded a, Variate a
--                 , Integral (Unsigned a), Bounded (Unsigned a), Variate (Unsigned a))
--              => (a,a) -> Gen (PrimState m) -> m a
-- uniformRange (x1,x2) g
--   | n == 0    = uniform g   -- Abuse overflow in unsigned types
--   | otherwise = loop
--   where
--     -- Allow ranges where x2<x1
--     (# i, j #) | x1 < x2   = (# x1, x2 #)
--                | otherwise = (# x2, x1 #)
--     n       = 1 + sub j i
--     buckets = maxBound `div` n
--     maxN    = buckets * n
--     loop    = do x <- uniform g
--                  if x < maxN then return $! add i (x `div` buckets)
--                              else loop
--                                {-# INLINE uniformRange #-}

data RBST k a = RBST
  { rbstGen  :: !Random.PureMT
  , rbstTree :: !(Tree k a)
  } deriving stock (Show, Generic, Foldable)

instance (Eq k, Eq a) => Eq (RBST k a) where
  (RBST _ tree1) == (RBST _ tree2) = tree1 == tree2

----------------------------------------------
----------- Smart Constructors ---------------
----------------------------------------------

defaultRandomGenerator :: Random.PureMT
defaultRandomGenerator = Random.pureMT 0

-- https://hackage.haskell.org/package/mersenne-random-pure64-0.2.2.0/docs/System-Random-Mersenne-Pure64.html





----------------------------------------------------------------------------
-- Generic functions
----------------------------------------------------------------------------

-- TODO
-- TODO
-- TODO
-- TODO
--
--
--
--
--
--
--
--
--
--

-- | Lift a function that works with 'Treap' to 'RTreap'.
withTreap :: (Treap m a -> r) -> (RTreap m a -> r)
withTreap f = f . rTreapTree
{-# INLINE withTreap #-}

-- | Lift a function that works with 'Treap' to 'RTreap'.
overTreap :: (Treap m a -> Treap m a) -> (RTreap m a -> RTreap m a)
overTreap set t = t { rTreapTree = set $ rTreapTree t }
{-# INLINE overTreap #-}
