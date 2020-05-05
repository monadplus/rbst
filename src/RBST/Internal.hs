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
-- The implementation uses the /Mersenne twister/, a pure pseudo-random number generator (Matsumoto and Nishimura).
--
-- __NOTE__: the computational complexity of each operation is annotated in the documentation and it is guaranteed, irrespectively of the input distribution (with a small constant factor overhead).
--
--------------------------------------------------------------------
module RBST.Internal (
  -- * Types, TyCons & Instances
    Size(..)
  , Tree(..)
  , RBST(..)
  , MonadRandT
  , MonadRand

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
  , intersect
  , difference

  -- * Randomization functions
  , uniformR

  -- * Internals functions
  , withTree

  -- * Reexports
  --, module System.Random.Mersenne.Pure64
  ) where

import           Control.DeepSeq                  (NFData (..), rnf)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Coerce                      (coerce)
import           Data.Foldable                    (foldl')
import           Data.Functor.Identity            (Identity)
import           Data.Word                        (Word64)
import           GHC.Exts                         (IsList (..))
import           GHC.Generics                     (Generic)
import           Prelude                          hiding (lookup)
import qualified System.Random.Mersenne.Pure64    as Random

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
instance Ord k => Semigroup (RBST k a) where
    (<>) = union

-- | mempty is implemented via 'empty'.
instance Ord k => Monoid (RBST k a) where
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
  toList RBST{..} = toListTree rbstTree
    where
      toListTree Empty            = []
      toListTree (Node _ k l x r) = toListTree l  ++ (k,x) : toListTree r
  {-# INLINEABLE toList #-}

instance (NFData k, NFData a) => NFData (RBST k a) where
    rnf RBST{..} = rnf rbstTree `seq` ()

-- | A random state transformer for the pseudo-random bits.
type MonadRandT m a = StateT Random.PureMT m a

-- | A random state in the 'Data.Functor.Identity' monad.
type MonadRand a = StateT Random.PureMT Identity a

----------------------------------------
-- Construction
----------------------------------------

-- | A pure mersenne twister pseudo-random number generator, 'Random.PureMT',
--
-- __TODO__: may be a better idea to use Random.newPureMT.
defaultRandomGenerator :: Random.PureMT
defaultRandomGenerator = Random.pureMT 0
{-# INLINE defaultRandomGenerator #-}

-- | The empty 'Tree'.
--
-- @
-- > empty         == fromList []
-- > size empty == 0
-- @
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
-- >>> lookup 'A' (empty :: RBST Char Int)
-- Nothing
--
-- >>> lookup 'A' (one 'A' 7)
-- Just 7
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
-- @
-- > insert 5 'x' empty == one 5 'x'
-- @
insert :: Ord k => k -> a -> RBST k a -> RBST k a
insert k x RBST{..} = runRand (insert' rbstTree) rbstGen
  where
    insert' Empty = return (oneTree k x)
    insert' node@(Node s !k2 l _ r) = do
      guess <- uniformR (0, coerce s)
      if guess == 0
        then do (rep, tree) <- insertRoot k x node
                if rep then pushDown tree
                       else pure tree
      else if k < k2
        then recomputeSize . updateL node <$> insert' l
      else
        recomputeSize . updateR node <$> insert' r
{-# INLINEABLE insert #-}

----------------------------------------------
-- Deletion
----------------------------------------------

-- | \( O(\log \ n) \). Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- @
-- > delete 1 (one (1, "A")) == empty
-- @
delete :: Ord k => k -> RBST k a -> RBST k a
delete k RBST{..} = runRand (delete' rbstTree) rbstGen
  where
    delete' Empty = return Empty
    delete' (Node _ k2 l _ r)
      | k == k2   = join l r
      | k < k2    = delete' l
      | otherwise = delete' r
{-# INLINEABLE delete #-}

----------------------------------------------
-- Set operations
----------------------------------------------

-- | \( \theta(m + n) \). Union of two 'RBST'.
--
-- In case of duplication, only one key remains by a random choice.
union :: Ord k => RBST k a -> RBST k a -> RBST k a
union (RBST s tree1) (RBST _ tree2) = runRand (union' tree1 tree2) s
  where
  union' t1 t2 = do
    let m = fromIntegral $ sizeTreeInt t1
        n = fromIntegral $ sizeTreeInt t2
        total = m + n
    if total == 0
      then return Empty
    else do
      u <- uniformR (1, total)
      let (a,b) = if u <= m then (t1,t2) else (t2,t1)
          (Node _ aKey aL x aR) = a -- Ignore warning: checked at u <= m
      (rep, bL, bR) <- split aKey b
      l <- union' aL bL
      r <- union' aR bR
      let randomize = if rep then pushDown else pure
      randomize (recomputeSize (Node 0 aKey l x r))
{-# INLINEABLE union #-}


-- | \( \theta(m + n) \). Intersection of two 'RBST'.
--
-- Notice, the intersection do not require randomness to produce a /Random Binary Search Tree/.
intersect :: Ord k => RBST k a -> RBST k a -> RBST k a
intersect (RBST s t1) (RBST _ t2) = runRand (intersect' t1 t2) s
  where
  intersect' Empty _ = return Empty
  intersect' (Node _ k l x r) b = do
    (rep, bL, bR) <- split k b
    iL <- intersect' l bL
    iR <- intersect' r bR
    if rep then pure $ recomputeSize (Node 0 k iL x iR)
           else join iL iR
{-# INLINEABLE intersect #-}

-- | \( \theta(m + n) \). Difference of two 'RBST'.
--
-- Notice, the difference do not require randomness to produce a /Random Binary Search Tree/.
difference :: Ord k => RBST k a -> RBST k a -> RBST k a
difference (RBST s t1) (RBST _ t2) = runRand (difference' t1 t2) s
  where
  difference' Empty _ = return Empty
  difference' (Node _ k l x r) b = do
    (rep, bL, bR) <- split k b
    dL <- difference' l bL
    dR <- difference' r bR
    if rep then join dL dR
           else pure $ recomputeSize (Node 0 k dL x dR)
{-# INLINEABLE difference #-}

----------------------------------------------
-- Random
----------------------------------------------

-- | Return a uniformly random 'Word64' in the given range.
uniformR :: (Word64, Word64) -> MonadRand Word64
uniformR (x1, x2)
  | n == 0    = error "Check uniformR"
  | otherwise = loop
  where
    (# i,j #) | x1 < x2   = (# x1, x2 #)
              | otherwise = (# x2, x1 #)
    n = 1 + (j - i)
    buckets = maxBound `div` n
    maxN = buckets * n -- rounding
    loop = do
      gen <- State.get
      let (!x, nextGen) = Random.randomWord64 gen
      if x < maxN
        then State.put nextGen >> return (i + (x `div` buckets))
        else State.put nextGen >> loop
{-# INLINE uniformR #-}

----------------------------------------------
-- Core internal functions
----------------------------------------------

-- | Given a random computation 'Tree' and an initial state, returns a 'RBST'.
runRand :: MonadRand (Tree k a) -> Random.PureMT -> RBST k a
runRand r s = let (tree, s') = State.runState r s in RBST s' tree

-- | Returns the key of the 'Node' or 'Nothing'.
-- getKey :: Tree k a -> Maybe k
-- getKey Empty = Nothing
-- getKey (Node _ k _ _ _) = Just k
-- {-# INLINE getKey #-}

-- | Return the left subtree or empty.
getL :: Tree k a -> Tree k a
getL Empty            = Empty
getL (Node _ _ l _ _) = l
{-# INLINE getL #-}

-- | Return the right subtree or empty.
getR :: Tree k a -> Tree k a
getR Empty            = Empty
getR (Node _ _ _ _ r) = r
{-# INLINE getR #-}

-- | 'fmap' over 'rbstGen'.
-- overGen :: (Random.PureMT -> Random.PureMT) -> RBST k a -> RBST k a
-- overGen f RBST{..} = RBST (f rbstGen) rbstTree
-- {-# INLINE overGen #-}

-- | Set a new 'rbstGen'.
-- setGen :: Random.PureMT -> RBST k a -> RBST k a
-- setGen newGen = overGen (const newGen)
-- {-# INLINE setGen #-}

-- | Lift a function from 'Tree' to 'RBST'.
withTree :: (Tree k a -> r) -> (RBST k a -> r)
withTree f = f . rbstTree
{-# INLINE withTree #-}

-- overTree :: (Tree k a -> Tree k a) -> (RBST k a -> RBST k a)
-- overTree f RBST{..} = RBST rbstGen (f rbstTree)
-- {-# INLINE overTree #-}

-- | \( O(1) \). Recompute tree size after modification
recomputeSize :: Tree k a -> Tree k a
recomputeSize Empty            = Empty
recomputeSize (Node _ k l c r) =
  let !s = sizeTree l + sizeTree r + 1 in Node s k l c r
{-# INLINE recomputeSize #-}

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
-- rotateR :: Tree k a -> Tree k a
-- rotateR Empty = Empty
-- rotateR node@(Node _ _ Empty _ _) = node
-- rotateR (Node s k (Node _ k2 l2 c2 r2) c r) =
--   Node s k2 l2 c2 newR
--   where
--     newR = recomputeSize $ Node s k r2 c r
-- {-# INLINEABLE rotateR #-}

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
-- rotateL :: Tree k a -> Tree k a
-- rotateL Empty = Empty
-- rotateL node@(Node _ _ _ _ Empty) = node
-- rotateL (Node s k l c (Node _ k2 l2 c2 r2)) =
--   Node s k2 newL c2 r2
--   where
--     newL = recomputeSize $ Node s k l c l2
-- {-# INLINE rotateL #-}

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

-- | \(O(\log \n )\). Insert node at root using 'split' and recompute the size.
--
-- __NOTE__: duplicated keys are removed by randomly picking one of them.
insertRoot :: Ord k => k -> a -> Tree k a -> MonadRand (Bool, Tree k a)
insertRoot k x Empty = return (False, oneTree k x)
insertRoot k x tree = do
  (rep, l, r) <- split k tree
  return (rep, recomputeSize (Node 0 k l x r))
{-# INLINE insertRoot #-}

-- | \(O(\log \n )\. Split the tree \( T \) into two trees \( T_< \) and \( T_> \), which contain the keys of \( T \) that are smaller than x and larger than x, respectively.
--
-- This is a sligh variant which removes any duplicate of 'k' and returns a bool indicating so.
split :: Ord k => k -> Tree k a -> MonadRand (Bool, Tree k a, Tree k a)
split _ Empty = return (False, Empty, Empty)
split k node@(Node _ k2 l _ r)
  | k < k2 = do
    (b, t1, t2) <- split k l
    return (b, t1, recomputeSize (updateL node t2))
  | k == k2 = do
      (_, t1, t2) <- split k r
      newT1       <- join l t1
      return (True, newT1, t2)
  | otherwise = do
      (b, t1, t2) <- split k r
      return (b, recomputeSize (updateR node t1), t2)
{-# INLINE split #-}

-- | Given a BST where left and right subtrees are random BST, returns a completly random BST.
--
-- __NOTE__: the input can't be 'Empty'.
pushDown :: Tree k a -> MonadRand (Tree k a)
pushDown Empty = error "The input of pushDown can be an empty tree."
pushDown tree@(Node _ _ l _ r) = do
  let !m = fromIntegral $ sizeTreeInt l
      !n = fromIntegral $ sizeTreeInt r
      !total = m + n
  u <- uniformR (0, total)
  if u < m
    then updateR l <$> (pushDown $ recomputeSize $ updateL tree (getR l))
  else if u < total
    then updateL r <$> (pushDown $ recomputeSize $ updateR tree (getL r))
  else
    return tree

-- | \(O(\log \ n )\). Invariant: : All keys from p must be strictly smaller than any key of q.
--
-- Theorem. The join of two independent random binary search tree is a random binary search tree.
join :: Tree k a -> Tree k a -> MonadRand (Tree k a)
join Empty q = return q
join p Empty = return p
join p@(Node s _ _ _ pR) q@(Node s2 _ qL _ _) = do
  guess <- uniformR (0, unSize (s + s2))
  if guess < unSize s
    then recomputeSize . updateR p <$> join pR q
    else recomputeSize . updateL q <$> join p  qL
{-# INLINE join #-}
