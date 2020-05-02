{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
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

import           RBST.Pure                     (Size (..), Tree (..))
import qualified RBST.Pure                     as Pure
import qualified System.Random.Mersenne.Pure64 as Random
import           Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef)
import           Control.Monad.ST (ST, runST)

-----------------------------------------
-- Data Structure and Instances
-----------------------------------------
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

emptyWithGen :: Random.PureMT -> RBST k a
emptyWithGen gen = RBST gen Pure.empty

empty :: RBST k a
empty = emptyWithGen defaultRandomGenerator

oneWithGen :: Random.PureMT -> k -> a -> RBST k a
oneWithGen gen k x = RBST gen (Pure.one k x)

one :: k -> a -> RBST k a
one = oneWithGen defaultRandomGenerator

-- | Create a tree from a list of key\/value pairs.
--
-- > fromList [] == empty
-- > fromList [(5,"a")] == one 5 "a"
--
fromList :: Ord k => [(k,v)] -> RBST k a
fromList guess = foldl' ins empty
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

-- | Lookup the value at the key in the tree.
--
-- >>> lookup 1 (empty :: Tree Int Int)
-- Nothing
--
lookup :: Ord k => k -> RBST k a -> Maybe a
lookup k = withTree (Pure.lookup k)
{-# INLINEABLE lookup #-}

----------------------------------------------
-- Insertion
----------------------------------------------

insert :: Ord k => k -> a -> RBST k a -> RBST k a
insert k x RBST{..} = runST $ do
  ref <- newSTRef rbstGen
  let guess range = runST $ do
        g <- readSTRef ref
        let (w, g') = uniformR range g
        writeSTRef ref g'
        return w
  let rbstTree' = Pure.insert guess k x rbstTree
  rbstGen' <- readSTRef ref
  return (RBST rbstGen' rbstTree')
{-# INLINEABLE insert #-}

----------------------------------------------
-- Deletion
----------------------------------------------

-- | Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
--
-- > delete 1 (one (1, "A")) == empty
--
delete :: Ord k => k -> RBST k a -> RBST k a
delete
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
-- TODO
{-# INLINEABLE delete #-}

-------------------------------------------
-- Generic functions
--------------------------------------------


-- | Lift a function from 'Tree' to 'RBST'.
withTree :: (Tree k a -> r) -> (RBST k a -> r)
withTree f = f . rbstTree
{-# INLINE withTree #-}

-- | Lift a function from 'Tree' to 'RBST'.
overTree :: (Tree k a -> Tree k a) -> (RBST k a -> RBST k a)
overTree set t = t { rbstTree = set $ rbstTree t }
{-# INLINE overTree #-}

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
