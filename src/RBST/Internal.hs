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
-- Efficient implementation of a /Randomized Binary Search Tree/.
--
-- __NOTE__: the computational complexity of each operation is annotated in the documentation and it is guaranteed, irrespectively of the input distribution (with a small constant factor overhead).
--
--------------------------------------------------------------------
module RBST.Internal (
  -- * Data structure & Instances
    Size(..)
  , Tree(..)
  , RBST(..)

  -- * Construction functions
  , defaultRandomGenerator
  , empty
  , emptyWithGen
  , one
  , oneWithGen

  -- * Query functions
  , size
  , sizeTree
  , lookup

  -- * Modification functions
  -- ** Insertion
  , insert
  -- ** Deletion
  , delete

  -- * Set operations
  , union
  --, intersection
  --, difference

  -- * Randomization functions
  , uniformR

  -- * Internals functions
  , withTree

  -- * Reexports
  --, module System.Random.Mersenne.Pure64
  ) where

import           Control.DeepSeq               (NFData (..), rnf)
import           Data.Bifunctor                (first)
import           Data.Coerce                   (coerce)
import           Data.Foldable                 (foldl')
import           Data.Word                     (Word64)
import           GHC.Exts                      (IsList (..))
import           GHC.Generics                  (Generic)
import           Prelude                       hiding (lookup)
import qualified System.Random.Mersenne.Pure64 as Random


-- TODO
-- [ ] Duplicate Keys

-- $setup
-- >>> import RBST.Pretty


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

-- | (<>) is implemented via 'merge'.
instance Semigroup (RBST k a) where
    (<>) = union

-- | mempty is implemented via 'empty'.
instance Monoid (RBST k a) where
    mempty = empty

-- | (==) is implemented via (==) of the underlying 'Tree'.
instance (Eq k, Eq a) => Eq (RBST k a) where
  (RBST _ tree1) == (RBST _ tree2) = tree1 == tree2

-- | Create a tree from a list of key\/value pairs, and viceversa.
--
-- __NOTE__: This requires @{-# LANGUAGE OverloadedLists #-}@ enabled.
--
-- Functions have the following time complexity:
--
-- 1. 'fromList': \( O(n \cdot \log \ n) \)
-- 2. 'toList': \( O(n) \).
--
-- @
-- > let tree = fromList @(RBST String Int) [("duck",5), ("lion",3), ("ape",1)]
--
-- > toList smallRBST == [('A',1),('B',2),('C',3),('D',4),('E',5)]
-- @
instance Ord k => IsList (RBST k a) where
  type Item (RBST k a) = (k,a)

  fromList :: [(k,a)] -> RBST k a
  fromList = foldl' ins empty where
      ins tree (!k,!x) = insert k x tree
  {-# INLINEABLE fromList #-}

  -- | Inorder traversal.
  toList :: RBST k a -> [(k,a)]
  toList RBST{..} = toListTree rbstTree where
    toListTree Empty            = []
    toListTree (Node _ k l x r) = toListTree l  ++ (k,x) : toListTree r
  {-# INLINEABLE toList #-}

instance (NFData k, NFData a) => NFData (RBST k a) where
    rnf RBST{..} = rnf rbstTree `seq` ()

----------------------------------------
-- Construction
----------------------------------------

-- | A pure mersenne twister pseudo-random number generator, 'Random.PureMT',
defaultRandomGenerator :: Random.PureMT
defaultRandomGenerator = Random.pureMT 0
{-# INLINE defaultRandomGenerator #-}

-- | The empty 'Tree'.
--
-- > empty         == fromList []
-- > size empty == 0
empty :: RBST k a
empty = emptyWithGen defaultRandomGenerator
{-# INLINE empty #-}

-- | Returns an empty 'RBST' from a 'Random.PureMT'.
emptyWithGen :: Random.PureMT -> RBST k a
emptyWithGen gen = RBST gen Empty
{-# INLINE emptyWithGen #-}

-- | Single node 'RBST'.
--
-- >>> size (one 1 'a')
-- 1
one :: k -> a -> RBST k a
one = oneWithGen defaultRandomGenerator
{-# INLINE one #-}

-- | Returns a single node 'RBST' from a 'Random.PureMT'.
oneWithGen :: Random.PureMT -> k -> a -> RBST k a
oneWithGen gen = (RBST gen .) . oneTree
{-# INLINE oneWithGen #-}

-- | Single node 'Tree'.
oneTree :: k -> a -> Tree k a
oneTree k x = Node 1 k Empty x Empty
{-# INLINE oneTree #-}

----------------------------------------------
-- Query
----------------------------------------------

-- | \( O(1) \). Return the size of the 'RBST'.
size :: RBST k a -> Int
size = withTree sizeTreeInt
{-# INLINE size #-}

-- | \( O(1) \). Return the 'Size' of the 'Tree'.
sizeTree :: Tree k a -> Size
sizeTree Empty             = 0
sizeTree (Node !s _ _ _ _) = s
{-# INLINE sizeTree #-}

-- | \( O(1) \). Return the size of the 'Tree'.
sizeTreeInt :: Tree k a -> Int
sizeTreeInt Empty             = 0
sizeTreeInt (Node !s _ _ _ _) = fromIntegral (coerce s :: Word64)
{-# INLINE sizeTreeInt #-}

-- | \( O(\log \ n) \). Lookup the value at the key in the tree.
--
-- >>> lookup 1 (empty :: Tree Int Int)
-- Nothing
lookup :: Ord k => k -> RBST k a -> Maybe a
lookup k1 = withTree lookup'
  where
    lookup' Empty = Nothing
    lookup' (Node _ k2 l a r)
      | k1 == k2  = Just a
      | k1 < k2   = lookup' l
      | otherwise = lookup' r
{-# INLINEABLE lookup #-}

----------------------------------------------
-- Insertion
----------------------------------------------

-- | \( O(\log \ n) \). Insert a new key\/value pair in the tree.
--
-- If the key is already present in the map, the associated value is
-- replaced with the supplied value.
--
-- > insert 5 'x' empty == one 5 'x'
insert :: Ord k => k -> a -> RBST k a -> RBST k a
insert k1 x RBST{..} =
  let (tree, gen) = insert' rbstGen rbstTree
  in RBST gen tree
  where
    insert' gen Empty = (oneTree k1 x, gen)
    insert' gen node@(Node s !k2 l _ r)
      | guess == 0 = (insertRoot k1 x node, gen')
      | k1 < k2    = first (recomputeSize . updateL node) (insert' gen' l)
      | otherwise  = first (recomputeSize . updateR node) (insert' gen' r)
      where
        (guess, gen') = uniformR (0, coerce s+1) gen
{-# INLINEABLE insert #-}

----------------------------------------------
-- Deletion
----------------------------------------------

-- | \( O(\log \ n) \). Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- > delete 1 (one (1, "A")) == empty
delete :: Ord k => k -> RBST k a -> RBST k a
delete k1 RBST{..} =
  let (tree, gen) = delete' rbstTree
  in RBST gen tree
  where
    delete' Empty = (Empty, rbstGen)
    delete' (Node _ k2 l _ r)
      | k1 == k2  = join rbstGen l r
      | k1 < k2   = delete' l
      | otherwise = delete' r
{-# INLINEABLE delete #-}

----------------------------------------------
-- Set operations
----------------------------------------------

-- | \( \theta(m + n) \). Union of two 'RBST' deleting duplicate keys.
--
-- The 'union' returns a random BST.
union :: RBST k a -> RBST k a -> RBST k a
union = undefined

-- | \( \theta(m + n) \). Intersection of two 'RBST'.
--
-- Notice, the intersection do not require randomness to produce a /Random Binary Search Tree/.
intersect :: RBST k a -> RBST k a -> RBST k a
intersect = error "Not implemented yet!"

-- | \( \theta(m + n) \). Difference of two 'RBST'.
--
-- Notice, the difference do not require randomness to produce a /Random Binary Search Tree/.
difference :: RBST k a -> RBST k a -> RBST k a
difference = error "Not implemented yet!"

----------------------------------------------
-- Random
----------------------------------------------

-- | Return a uniformly random 'Word64' in the given range.
uniformR :: (Word64, Word64) -> Random.PureMT -> (Word64, Random.PureMT)
uniformR (x1, x2)
  | n == 0    = const (error "Check uniformR")
  | otherwise = loop
  where
    (# i,j #) | x1 < x2   = (# x1, x2 #)
              | otherwise = (# x2, x1 #)
    n = 1 + (j - i)
    buckets = maxBound `div` n
    maxN = buckets * n -- rounding
    loop g =
      let (!x, g') = Random.randomWord64 g
       in if x < maxN
            then (i + (x `div` buckets), g')
            else loop g'
{-# INLINE uniformR #-}

----------------------------------------------
-- Core internal functions
----------------------------------------------

-- | Lift a function from 'Tree' to 'RBST'.
withTree :: (Tree k a -> r) -> (RBST k a -> r)
withTree f = f . rbstTree
{-# INLINE withTree #-}

-- | \( O(1) \). Recompute tree size after modification
recomputeSize :: Tree k a -> Tree k a
recomputeSize Empty            = Empty
recomputeSize (Node _ k l c r) =
  let !s = sizeTree l + sizeTree r + 1 in Node s k l c r
{-# INLINEABLE recomputeSize #-}

-- | \( O(1) \). Rotate tree to the left.
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
rotateR (Node s k (Node _ k2 l2 c2 r2) c r) =
  Node s k2 l2 c2 newR
  where
    newR = recomputeSize $ Node s k r2 c r
{-# INLINEABLE rotateR #-}

-- | \( O(1) \). Rotate tree to the left.
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
rotateL (Node s k l c (Node _ k2 l2 c2 r2)) =
  Node s k2 newL c2 r2
  where
    newL = recomputeSize $ Node s k l c l2
{-# INLINE rotateL #-}

-- | \( O(1) \). Update the left node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateL :: Tree k a -> Tree k a -> Tree k a
updateL Empty newL            = newL
updateL (Node s k _ c r) newL = Node s k newL c r
{-# INLINE updateL #-}

-- | \( O(1) \). Update the right node with the given subtree.
--
-- Notice, the size is not recomputed so you
-- will probably need to call 'recomputeSize'.
updateR :: Tree k a -> Tree k a -> Tree k a
updateR Empty newR            = newR
updateR (Node s k l c _) newR = Node s k l c newR
{-# INLINE updateR #-}

-- | \(O(\log \n )\). Insert node at root and rebalance the tree.
--
-- We call 'rotateR' and 'rotateL' to rebalance the tree after the new node is inserted.
insertRoot :: Ord k => k -> a -> Tree k a -> Tree k a
insertRoot k x Empty = oneTree k x
insertRoot k x tree@(Node _ k2 l _ r)
  | k < k2    = rotateR $ updateL tree (insertRoot k x l)
  | otherwise = rotateL $ updateR tree (insertRoot k x r)
{-# INLINE insertRoot #-}

-- | \(O(\log \n )\. Split the tree \( T \) into two trees \( T_< \) and \( T_> \), which contain the keys of \( T \) that are smaller than x and larger than x, respectively.
split :: Ord k => k -> RBST k a -> (RBST k a, RBST k a)
split k RBST{..} = fmap (RBST rbstGen) $ split' rbstTree
  where
    split' Empty = (Empty, Empty)
    split' node@(_ k2 l _ r)
      | k < k2    =
          let (t1, t2) = split' l
           in (t1, updateL node t2)
      | otherwise =
          let (t1, t2) = split' r
           in (updateR node t1, t2)
{-# INLINE split #-}

-- | \(O(\log \ n )\). Invariant: : All keys from p must be strictly smaller than any key of q.
--
-- Theorem. The join of two independent random binary search tree is a random binary search tree.
join :: Random.PureMT -> Tree k a -> Tree k a -> (Tree k a, Random.PureMT)
join gen Empty q = (q, gen)
join gen p Empty = (p, gen)
join gen p@(Node s _ _ _ r) q@(Node s2 _ l2 _ _)
  | coerce guess < s = first (recomputeSize . updateR p) (join gen' r q)
  | otherwise        = first (recomputeSize . updateL q) (join gen' p l2)
  where
    maxN = unSize (s + s2)
    (guess, gen') = uniformR (0, maxN) gen
{-# INLINE join #-}
