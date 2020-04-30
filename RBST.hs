{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

import Test.QuickCheck.Modifiers (Positive(..))
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified System.Random.MWC as MWC
import qualified System.IO.Unsafe as Unsafe
import qualified Control.Monad.Primitive as Primitive

newtype Size = Size { unSize :: Int }
  deriving newtype (Num, Show)

data RBST a = Leaf | Node {-# UNPACK #-}!Size (Positive Int) (RBST a) !a (RBST a)
  deriving stock (Show)

empty :: RBST a
empty = Leaf

singleton :: (Int, a) -> RBST a
singleton (!k, !x) = Node 1 (coerce k) Leaf x Leaf

size :: forall a. RBST a -> Int
size Leaf = 0
size (Node !s _ _ _ _) = coerce s

-- | Classic find
find :: Int -> RBST a -> Maybe a
find _ Leaf = Nothing
find k1 (Node _ k2 l a r)
  | k1 == getPositive k2 = Just a
  | k1 < getPositive k2  = find k1 l
  | otherwise            = find k1 r

-- | Constant
fixSize :: RBST a -> RBST a
fixSize Leaf = Leaf
fixSize (Node _ k l c r) = Node (coerce $ size l + size r + 1) k l c r

rotateR :: RBST a -> RBST a
rotateL :: RBST a -> RBST a

rotateR Leaf = Leaf
rotateR node@(Node _ _ Leaf _ _) = node
rotateR (Node s k (Node s2 k2 l2 c2 r2) c r) =
  Node s k2 l2 c2 newR
  where
    newR = fixSize $ Node s k r2 c r

rotateL Leaf = Leaf
rotateL node@(Node _ _ _ _ Leaf) = node
rotateL (Node s k l c (Node s2 k2 l2 c2 r2)) =
  Node s k2 newL c2 r2
  where
    newL = fixSize $ Node s k l c l2

insert :: (Int, a) -> RBST a -> RBST a
insertRoot :: (Int, a) -> RBST a -> RBST a

insertRoot x Leaf = singleton x
insertRoot t@(k1, _) (Node s k2 l c r)
  | coerce k1 < k2 = rotateR (Node s k2 (insertRoot t l) c r)
  | otherwise      = rotateL (Node s k2 l c (insertRoot t r))

-- | Global PRNG
gen :: MWC.Gen (Primitive.PrimState IO)
gen = Unsafe.unsafePerformIO MWC.create

insert t Leaf = singleton t
insert t@(!k1, _) node@(Node s !k2 l c r)
  | guess (unSize s + 1) == 0 = insertRoot t node
  | otherwise = fixSize $
      if coerce k1 < k2
        then Node s k2 (insert t l) c r
        else Node s k2 l c (insert t r)
  where
    guess n = Unsafe.unsafePerformIO $ MWC.uniformR (0, n) gen



-- TODO: arnau Key Deletion Thu 30 Apr 2020 10:15:55 PM CEST















main :: IO ()
main = do
  let tree = empty
           & insert (5, "A")
           & insert (2, "B")
           & insert (8, "D")
           & insert (4, "C")
  print $ find 2 tree
  print $ find 7 tree
