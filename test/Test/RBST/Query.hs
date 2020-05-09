{-# LANGUAGE TupleSections #-}
module Test.RBST.Query (
    querySpec
  ) where

import           GHC.Exts    (IsList (..))
import           Test.Common (TestRBST, smallRBST)
import           Test.Hspec  (Spec, describe, it, shouldBe)

import qualified RBST

querySpec :: Spec
querySpec = describe "Query tests" $ do
    sizeSpec
    heightSpec
    isListSpec
    lookupSpec
    atSpec

sizeSpec :: Spec
sizeSpec = describe "size" $ do
    it "size of empty RBST is 0" $
        RBST.size (RBST.empty :: TestRBST) `shouldBe` 0
    it "size of single node RBST is 1" $
        RBST.size (RBST.one 'A' 0 :: TestRBST) `shouldBe` 1
    it "size of smallRBST is 5" $
        RBST.size smallRBST `shouldBe` 5

heightSpec :: Spec
heightSpec = describe "height" $ do
    it "height of empty RBST is -1" $
        RBST.height (RBST.empty :: TestRBST) `shouldBe` -1
    it "height of single node RBST is 0" $
        RBST.height (RBST.one 'A' 0 :: TestRBST) `shouldBe` 0
    it "height of smallRBST is 2" $
        RBST.height smallRBST `shouldBe` 2

isListSpec :: Spec
isListSpec = describe "fromList/toList" $ do
    it "toList smallRBST is [1..5]" $
        toList smallRBST `shouldBe` zip ['A'..'E'] [1..5 :: Int]
    it "toList should sort by key" $
        let sort = fmap fst . toList @(RBST.RBST Int ()) . fromList . fmap (,())
         in sort [2,5,1,4,3] `shouldBe` [1..5 :: Int]

lookupSpec :: Spec
lookupSpec = describe "lookup" $ do
    it "lookup by keys are correct" $
        map (`RBST.lookup` smallRBST) ['A'..'F'] `shouldBe`
          [Just 1, Just 2, Just 3, Just 4, Just 5, Nothing]

atSpec :: Spec
atSpec = describe "at" $ do
    it "elements by indices are correct" $
        map (`RBST.at` smallRBST) [-1..5] `shouldBe`
          [Nothing, Just ('A',1), Just ('B',2), Just ('C',3), Just ('D',4), Just ('E',5), Nothing]
