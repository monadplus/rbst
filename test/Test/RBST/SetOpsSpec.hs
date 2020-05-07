module Test.RBST.SetOpsSpec (
    spec
  ) where

import Test.Hspec (Spec, describe, it)
import Test.Common (TestRBST, describedAs)
import GHC.Exts (IsList(..))

import           RBST (union)
import qualified RBST

spec :: Spec
spec = describe "Set operations tests" $ do
  unionSpec
  intersectionSpec
  subtractionSpec
  differenceSpec

unionSpec :: Spec
unionSpec = describe "union" $ do
  it "union of two trees works" $
    tree1 `union` tree2 `describedAs` [('A', 1), ('B', 0), ('C', 3), ('D', 4)]

intersectionSpec :: Spec
intersectionSpec = describe "intersection" $ do
  it "intersection of two trees works" $
    RBST.intersection tree1 tree2 `describedAs` [('B', 2)]

subtractionSpec :: Spec
subtractionSpec = describe "subtraction" $ do
  it "subtraction of two trees works" $
    RBST.subtraction tree1 tree2 `describedAs` [('A', 1)]

differenceSpec :: Spec
differenceSpec = describe "difference" $ do
  it "difference of two trees works" $
    RBST.difference tree1 tree2 `describedAs` [('A', 1), ('C', 3), ('D', 4)]

--------------------------------------------------------
--------------------------------------------------------

tree1 :: TestRBST
tree1 = fromList [('A', 1), ('B', 2)]

tree2 :: TestRBST
tree2 = fromList [('B', 0), ('C', 3), ('D', 4)]
