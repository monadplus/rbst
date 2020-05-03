{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}
--------------------------------------------------------------------
-- |
-- Module      :  RBST.Internal
-- Copyright   :  (c) Arnau Abella 2020
-- License     :  MIT
-- Maintainer  :  arnauabell@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
--------------------------------------------------------------------
module RBST.Internal (
  -- * Data Types
    Size(..)
  , Tree(..)
  , RBST(..)

  -- * Construction
  , empty
  , one
  , fromList

  -- * Query
  , size
  , lookup

  -- ** Insertion
  , insert

  -- ** Deletion
  , delete

  -- ** Reexports
  , module Random
  ) where

import           Control.DeepSeq               (NFData)
import           Control.Monad.ST              (ST, runST)
import           Data.Bifunctor                (first, second)
import           Data.Coerce                   (coerce)
import           Data.STRef                    (modifySTRef, newSTRef, readSTRef, writeSTRef)
import           Data.Word                     (Word64)
import           GHC.Generics                  (Generic)
import           Prelude                       hiding (lookup)
import qualified System.Random.Mersenne.Pure64 as Random


-- TODO
-- [ ] Duplicate Keys
-- [ ] Join + Semigroup + Monoid

-- $setup


-----------------------------------------
-- Data Structure and Instances
-----------------------------------------

-- | Size of the 'Tree' data structure. Guaranteed to be always > 0.
newtype Size = Size
  { unSize :: Word64
  } deriving stock (Show, Read, Generic)
    deriving newtype (Eq, Ord, Num, NFData)

-- | 'Tree' data structure. The node contains the rank of the tree.
data Tree k a
  = Node !Size !k !(Tree k a) !a !(Tree k a)
  | Empty
  deriving stock (Show, Read, Eq, Generic, Foldable)
  deriving anyclass (NFData)

-- | 'RBST' data structure.
data RBST k a = RBST
  { rbstGen  :: !Random.PureMT
  , rbstTree :: !(Tree k a)
  } deriving stock (Show, Generic, Foldable)

instance (Eq k, Eq a) => Eq (RBST k a) where
  (RBST _ tree1) == (RBST _ tree2) = tree1 == tree2

----------------------------------------
-- Construction
----------------------------------------

defaultRandomGenerator :: Random.PureMT
defaultRandomGenerator = Random.pureMT 0

-- | The empty 'Tree'.
--
-- > empty         == fromList []
-- > size empty == 0
empty :: RBST k a
empty = emptyWithGen defaultRandomGenerator

emptyWithGen :: Random.PureMT -> RBST k a
emptyWithGen gen = RBST gen Empty

-- | Single node 'Tree'.
--
-- >>> size (one 1 'a')
-- 1
one :: k -> a -> RBST k a
one = oneWithGen defaultRandomGenerator

oneWithGen :: Random.PureMT -> k -> a -> RBST k a
oneWithGen gen k x = RBST gen (Node 1 k Empty x Empty)

-- | Create a tree from a list of key\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a")] == one 5 "a"
fromList :: Ord k => [(k,v)] -> RBST k a
fromList = foldl' ins empty
  where
    ins tree (!k,!x) = insert k x tree
{-# INLINEABLE fromList #-}

----------------------------------------------
-- Query
----------------------------------------------

-- | Return the size of the tree.
size :: RBST k a -> Int
size = withTree Pure.sizeInt
{-# INLINEABLE size #-}

-- | Return the 'Size' of the tree.
size :: Tree k a -> Size
size Empty             = 0
size (Node !s _ _ _ _) = s
{-# INLINEABLE size #-}

-- | Return the size of the tree.
sizeInt :: Tree k a -> Int
sizeInt Empty             = 0
sizeInt (Node !s _ _ _ _) = fromIntegral (coerce s :: Word64)
{-# INLINEABLE sizeInt #-}

-- | Lookup the value at the key in the tree.
--
-- >>> lookup 1 (empty :: Tree Int Int)
-- Nothing
--
lookup :: Ord k => k -> RBST k a -> Maybe a
lookup k1 = withTree lookup'
  where
    lookup Empty = Nothing
    lookup (Node _ k2 l a r)
      | k1 == k2  = Just a
      | k1 < k2   = lookup l
      | otherwise = lookup r
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
insert :: Ord k => k -> a -> RBST k a -> RBST k a
insert k x RBST{..} =
  let (tree, gen) = insert' rbstGen rbstTree
  in RBST gen tree
  where
    insert' gen Empty = (one k x, gen)
    insert' gen node@(Node s !k2 l _ r)
      | guess == 0 = (insertRoot k1 x node, gen')
      | k1 < k2    = first (recomputeSize . updateL node) (insert' gen' l)
      | otherwise  = first (recomputeSize . updateR node) (insert' gen' r)
      where
        (guess, gen') = uniformR (0, s+1) gen
{-# INLINEABLE insert #-}

----------------------------------------------
-- Deletion
----------------------------------------------

-- | Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- > delete 1 (one (1, "A")) == empty
--
delete :: Ord k => k -> RBST k a -> RBST k a
delete k RBST{..} = delete' rbstTree
  where
    delete' Empty = (Empty, rbstGen)
    delete' (Node s k2 l _ r)
      | k1 == k2  = join rbstGen l r
      | k1 < k2   = delete' l
      | otherwise = delete' r
{-# INLINEABLE delete #-}

----------------------------------------------
-- Core internal functions
----------------------------------------------

-- | Lift a function from 'Tree' to 'RBST'.
withTree :: (Tree k a -> r) -> (RBST k a -> r)
withTree f = f . rbstTree
{-# INLINE withTree #-}

-- | Lift a function from 'Tree' to 'RBST'.
overTree :: (Tree k a -> Tree k a) -> (RBST k a -> RBST k a)
overTree set t = t { rbstTree = set $ rbstTree t }
{-# INLINE overTree #-}

withRBST :: (Guess -> Tree k v -> Tree k v) -> (RBST k a -> RBST k a)
withRBST action = \RBST{..} -> RBST rbstTree' rbstGen'
  where
   (rbstTree', rbstGen') = withGuess rbstGen (\g -> action g rbstTree)
{-# INLINE withRBST #-}


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
{-# INLINE rotateL #-}

-- | Update the left node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateL :: Tree k a -> Tree k a -> Tree k a
updateL (Node s k _ c r) newL = Node s k newL c r
{-# INLINE updateL #-}

-- | Update the right node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateR :: Tree k a -> Tree k a -> Tree k a
updateR (Node s k l c _) newR = Node s k l c newR
{-# INLINE updateR #-}

-- | Insert node at root and rebalance the tree.
--
-- We call 'rotateR' and 'rotateL' to rebalance the tree after the new node is inserted.
insertRoot :: Ord k => k -> a -> Tree k a -> Tree k a
insertRoot k x Empty = one k x
insertRoot k x tree@(Node s k2 l c r)
  | k < k2    = rotateR $ updateL tree (insertRoot k x l)
  | otherwise = rotateL $ updateR tree (insertRoot k x r)
{-# INLINE insertRoot #-}

-- | Invariant: : All keys from p must be strictly smaller than any key of q.
--
-- Theorem. The join of two independent random binary search tree is a random binary search tree.
join :: Random.PureMT -> Tree k a -> Tree k a -> (Tree k a, Random.PureMT)
join _ Empty q = q
join _ p Empty = p
join gen p@(Node s k l c r) q@(Node s2 k2 l2 c2 r2)
  | coerce r < s = first recomputeSize $ updateR p (join gen' r q)
  | otherwise = first recomputeSize $ updateL q (join gen' p l2)
  where
    (r, gen') = uniformR (0, s + s2) gen
{-# INLINE join #-}

-- | Return a uniformly random 'Word64' in the given range.
uniformR :: (Word64, Word64) -> Random.PureMT -> (Word64, Random.PureMT)
uniformR (x1, x2)
  | n == 0    = const (error "Check uniformR")
  | otherwise = loop
  where
    (# i,j #) | x1 < x2   = (# x1, x2 #)
              | o therwise = (# x2, x1 #)
    n = 1 + (j - i)
    buckets = maxBound `div` n
    loop g =
      let (!x, g') = randomWord64 g
       in if x < maxN
            then (i + (x `div` buckets), g')
            else loop g'
{-# INLINE uniformR #-}
