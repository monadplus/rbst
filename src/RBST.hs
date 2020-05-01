{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------
-- |
-- Module      :  RBST
-- Copyright   :  (c) Arnau Abella 2020
-- License     :  MIT
-- Maintainer  :  arnauabell@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------
module RBST where

import           Control.DeepSeq           (NFData)
import qualified Control.Monad.Primitive   as Primitive
import           Data.Coerce               (coerce)
import           Data.Function             ((&))
import           GHC.Generics              (Generic)
import qualified System.Random.Marsenne.Pure64 as Random


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


-- TODO
-- [ ] Generic keys
-- [ ] Change PNRG
-- [ ] Write newNodeL, newNodeR
-- [ ] Duplicate Keys

-- $setup
-- >>> import Data.Monoid

newtype Size = Size
  { unSize :: Int
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Ord, Num, NFData)

data Tree k a
  = Node !Size !k !(Tree k a) !a !(Tree k a)
  | Empty
  deriving stock (Show, Read, Eq, Generic, Foldable)
  deriving anyclass (NFData)

data RBST k a = RBST
  { rbstGen  :: !Random.PureMT
  , rbstTree :: !(Tree k a)
  } deriving stock (Show, Generic, Foldable)

instance (Eq k, Eq a) => Eq (RBST k a) where
  (RBST _ tree1) == (RBST _ tree2) = tree1 == tree2

----------------------------------------------
----------- Smart Constructors ---------------
----------------------------------------------


-- |
-- >>> 5 + 3
-- 8
mystery :: Int -> Int
mystery x = case x of
  5 -> 5
  _ -> 0
defaultRandomGenerator :: Random.PureMT
defaultRandomGenerator = Random.pureMT 0

empty :: RBST k a
empty = Empty

one :: (k, a) -> RBST k a
one (!k, !x) = Node 1 k Empty x Empty

size :: RBST k a -> Size
size Empty             = 0
size (Node !s _ _ _ _) = s

sizeInt :: RBST k a -> Int
sizeInt Empty             = 0
sizeInt (Node !s _ _ _ _) = coerce s

lookup :: Ord k => k -> RBST k a -> Maybe a
lookup _ Empty = Nothing
lookup k1 (Node _ k2 l a r)
  | k1 == k2             = Just a
  | k1 < k2              = lookup k1 l
  | otherwise            = lookup k1 r

fixSize :: RBST k a -> RBST k a
fixSize Empty            = Empty
fixSize (Node _ k l c r) = Node (size l + size r + 1) k l c r

rotateR, rotateL :: RBST k a -> RBST k a

rotateR Empty = Empty
rotateR node@(Node _ _ Empty _ _) = node
rotateR (Node s k (Node s2 k2 l2 c2 r2) c r) =
  Node s k2 l2 c2 newR
  where
    newR = fixSize $ Node s k r2 c r

rotateL Empty = Empty
rotateL node@(Node _ _ _ _ Empty) = node
rotateL (Node s k l c (Node s2 k2 l2 c2 r2)) =
  Node s k2 newL c2 r2
  where
    newL = fixSize $ Node s k l c l2


insertRoot :: Ord k => (k, a) -> RBST k a -> RBST k a
insertRoot t Empty = one t
insertRoot t@(k1, _) (Node s k2 l c r)
  | k1 < k2   = rotateR (Node s k2 (insertRoot t l) c r)
  | otherwise = rotateL (Node s k2 l c (insertRoot t r))

insert :: (Int, a) -> RBST k a -> RBST k a
insert t Empty = one t
insert t@(!k1, _) node@(Node s !k2 l c r)
  | guessR (0, s + 1) == 0 = insertRoot t node
  | otherwise = fixSize $
      if coerce k1 < k2
        then Node s k2 (insert t l) c r
        else Node s k2 l c (insert t r)

-- TODO don't expose
-- | Invariant: : All keys from p must be smaller than any key of q.
join :: RBST k a -> RBST k a -> RBST k a
join Empty q = q
join p Empty = p
join p@(Node s k l c r) q@(Node s2 k2 l2 c2 r2)
  | guess (0, s + s2) < s = fixSize $ Node s k l c (join r q)
  | otherwise          = fixSize $ Node s2 k2 (join p l2) c2 r2

--        key
delete :: Int -> RBST k a -> RBST k a
delete _ Empty = Empty
delete k1 (Node s k2 l c r)
  | coerce k1 == k2  = join l r
  | coerce k1 < k2   = delete k1 l
  | otherwise        = delete k1 r
