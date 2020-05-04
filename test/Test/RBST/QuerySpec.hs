module Test.RBST.QuerySpec (
    spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Common (TestRBST, smallRBST)
import GHC.Exts (IsList(..))

import qualified RBST

spec :: Spec
spec = describe "Query tests" $ do
    basicSpec

basicSpec :: Spec
basicSpec = describe "Sanity checks" $ do
    it "size of empty RBST is 0" $
        RBST.size RBST.empty `shouldBe` 0
    it "size of singletone RBST is 1" $
        RBST.size (RBST.one 'A' 0 :: TestRBST) `shouldBe` 1
    it "size of smallRBST is 5" $
        RBST.size smallRBST `shouldBe` 5
    it "toList smallRBST is [1..5]" $
        toList smallRBST `shouldBe` (zip ['A'..'E'] [1..5])
    -- TODO lookup
