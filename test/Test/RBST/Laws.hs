module Test.RBST.Laws (
    lawsSpec
  ) where

import           GHC.Exts        (IsList (..))
import           Test.Common     (TestRBST)
import           Test.Hspec      (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck
import           RBST.Pretty (compact)

lawsSpec :: Spec
lawsSpec = describe "Law abiding instances" $ do
    --semigroupSpec
    monoidSpec

-- | Semigroup is not lawful until unionWith is implemented.

-- semigroupSpec :: Spec
-- semigroupSpec = describe "Semigroup" $
--   prop "associativity" $
--     forAllShow arbitraryTree compact $ \a -> do
--       forAllShow arbitraryTree compact $ \b -> do
--         forAllShow arbitraryTree compact $ \c -> do
--           (a <> b) <> c `iso` a <> (b <> c)

monoidSpec :: Spec
monoidSpec = describe "Monoid" $
  prop "identity" $
    forAllShow arbitraryTree compact $ \a -> do
      (a <> mempty) `iso` a .&&. (mempty <> a) `iso` a


----------------------------------------
----------------------------------------

arbitraryTree :: Gen TestRBST
arbitraryTree = do
  n <- choose (0, 100)
  xs <- vectorOf n arbitrary
  return $ fromList xs

-- | '(===)' for trees.
iso :: TestRBST -> TestRBST -> Property
t1 `iso` t2 = toList t1 === toList t2
infix 4 `iso`
