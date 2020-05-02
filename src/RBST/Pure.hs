--------------------------------------------------------------------
-- |
-- Module      :  RBST.Pure
-- Copyright   :  (c) Arnau Abella 2020
-- License     :  MIT
-- Maintainer  :  arnauabell@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------
module RBST.Pure (
  -- * Data Types
    Size(..)
  , Tree(..)

  -- * Construction
  , empty
  , one
  , fromList

  -- * Query
  , size
  , sizeInt
  , lookup

  -- ** Insertion
  , insert

  -- ** Deletion
  , delete

  ) where

import           Control.DeepSeq         (NFData)
import qualified Control.Monad.Primitive as Primitive
import           Data.Coerce             (coerce)
import           Data.Foldable           (foldl')
import           Data.Function           ((&))
import           Data.Word               (Word64)
import           GHC.Generics            (Generic)

-- TODO
-- [ ] Duplicate Keys
-- [ ] Join + Semigroup + Monoid

-- $setup

-----------------------------------------
-- Data Structure and Instances
-----------------------------------------

-- | Guess a size of the given range.
--
-- This types is used to make random choices.
type Guess = (Size, Size) -> Size

-- | Size of the 'Tree' data structure. Guaranteed to be always > 0.
newtype Size = Size
  { unSize :: Word64
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Ord, Num, NFData)

-- | 'Tree" data structure.
data Tree k a
  = Node !Size !k !(Tree k a) !a !(Tree k a)
  | Empty
  deriving stock (Show, Read, Eq, Generic, Foldable)
  deriving anyclass (NFData)

----------------------------------------
-- Construction
----------------------------------------

-- | The empty tree.
--
-- > empty         == fromList []
-- > sizeInt empty == 0
--
empty :: Tree k a
empty = Empty
{-# INLINEABLE empty #-}

one :: k -> a -> Tree k a
one !k !x = Node 1 k Empty x Empty
{-# INLINEABLE one #-}

----------------------------------------------
-- Query
----------------------------------------------

-- | Return the 'Size' of the tree.
size :: Tree k a -> Size
size Empty             = 0
size (Node !s _ _ _ _) = s
{-# INLINEABLE size #-}

-- | Return the size of the tree.
sizeInt :: Tree k a -> Int
sizeInt Empty             = 0
sizeInt (Node !s _ _ _ _) = coerce s
{-# INLINEABLE sizeInt #-}

-- | Lookup the value at the key in the tree.
--
-- >>> lookup 1 (empty :: Tree Int Int)
-- Nothing
--
lookup :: Ord k => k -> Tree k a -> Maybe a
lookup _ Empty = Nothing
lookup k1 (Node _ k2 l a r)
  | k1 == k2  = Just a
  | k1 < k2   = lookup k1 l
  | otherwise = lookup k1 r
{-# INLINEABLE lookup #-}

----------------------------------------------
-- Insertion
----------------------------------------------

-- | Insert a new key\/value pair in the tree.
--
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value.
--
-- > insert 5 'x' empty == one 5 'x'
--
insert :: Ord k => Guess -> k -> a -> Tree k a -> Tree k a
insert _ k x Empty = one k x
insert guess !k1 x node@(Node s !k2 l _ r)
  | guess (0, s + 1) == 0 = insertRoot t node
  | otherwise = recomputeSize $
      if k1 < k2
        then updateL node (insert guess k1 x l)
        else updateR node (insert guess k1 x r)
{-# INLINEABLE insert #-}

----------------------------------------------
-- Deletion
----------------------------------------------

-- | Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- > delete 1 (one (1, "A")) == empty
--
delete :: Ord k => Guess -> k -> Tree k a -> Tree k a
delete _ _ Empty = Empty
delete guess k1 (Node s k2 l _ r)
  | k1 == k2  = join guess l r
  | k1 < k2   = delete k1 l
  | otherwise = delete k1 r
{-# INLINEABLE delete #-}

------------------------------------------------------
----------- Core internal functions ------------------
------------------------------------------------------

-- | Recompute tree size after modification
recomputeSize :: Tree k a -> Tree k a
recomputeSize Empty            = Empty
recomputeSize (Node _ k l c r) = Node (size l + size r + 1) k l c r
{-# INLINEABLE recomputeSize #-}

-- | Rotate tree to the left.
--
-- Before
--
--        ╱╲
--       ╱  ╲
--      ╱    ╲
--     ╱      ╲
--    ╱╲       C
--   ╱  ╲
--  ╱    ╲
-- A      B
--
-- After
--
--       ╱╲
--      ╱  ╲
--     ╱    ╲
--    ╱      ╲
--   A       ╱╲
--          ╱  ╲
--         ╱    ╲
--        B      C
--
rotateR :: Tree k a -> Tree k a
rotateR Empty = Empty
rotateR node@(Node _ _ Empty _ _) = node
rotateR (Node s k (Node s2 k2 l2 c2 r2) c r) =
  Node s k2 l2 c2 newR
  where
    newR = recomputeSize $ Node s k r2 c r
{-# INLINEABLE rotateR #-}

-- | Rotate tree to the left.
--
--
-- Before
--
--       ╱╲
--      ╱  ╲
--     ╱    ╲
--    ╱      ╲
--   A       ╱╲
--          ╱  ╲
--         ╱    ╲
--        B      C
--
-- After
--
--        ╱╲
--       ╱  ╲
--      ╱    ╲
--     ╱      ╲
--    ╱╲       C
--   ╱  ╲
--  ╱    ╲
-- A      B
--
rotateL :: Tree k a -> Tree k a
rotateL Empty = Empty
rotateL node@(Node _ _ _ _ Empty) = node
rotateL (Node s k l c (Node s2 k2 l2 c2 r2)) =
  Node s k2 newL c2 r2
  where
    newL = recomputeSize $ Node s k l c l2
{-# INLINEABLE rotateL #-}

-- | Update the left node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateL :: Tree k a -> Tree k a -> Tree k a
updateL (Node s k _ c r) newL = Node s k newL c r
{-# INLINEABLE updateL #-}

-- | Update the right node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateR :: Tree k a -> Tree k a -> Tree k a
updateR (Node s k l c _) newR = Node s k l c newR
{-# INLINEABLE updateR #-}

-- | Insert node at root and rebalance the tree.
--
-- We call 'rotateR' and 'rotateL' to rebalance the tree after the new node is inserted.
insertRoot :: Ord k => k -> a -> Tree k a -> Tree k a
insertRoot k x Empty = one k x
insertRoot k x tree@(Node s k2 l c r)
  | k < k2    = rotateR $ updateL tree (insertRoot t l)
  | otherwise = rotateL $ updateR tree (insertRoot t r)
{-# INLINEABLE insertRoot #-}

-- | Invariant: : All keys from p must be strictly smaller than any key of q.
--
-- Theorem. The join of two independent random binary search tree is a random binary search tree.
join :: Guess -> Tree k a -> Tree k a -> Tree k a
join _ Empty q = q
join _ p Empty = p
join guess p@(Node s k l c r) q@(Node s2 k2 l2 c2 r2)
  | guess (0, s + s2) < s = recomputeSize $ updateR p (join r q)
  | otherwise             = recomputeSize $ updateL q (join p l2)
{-# INLINEABLE join #-}
