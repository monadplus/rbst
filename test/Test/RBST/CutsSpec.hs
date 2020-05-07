module Test.RBST.CutsSpec (
    spec
  ) where

import           Test.Common (describedAs, smallRBST)
import           Test.Hspec  (Spec, describe, it, shouldBe)

import qualified RBST

spec :: Spec
spec = describe "Cuts tests" $ do
    takeSpec
    dropSpec
    removeSpec

takeSpec :: Spec
takeSpec = describe "take" $ do
    it "take negative returns an empty tree" $
       RBST.take (-1) smallRBST `shouldBe` RBST.empty
    it "take 0 returns an empty tree" $
        RBST.take 0 smallRBST `shouldBe` RBST.empty
    it "take size returns tree itself" $
        RBST.take 5 smallRBST `shouldBe` smallRBST
    it "take 2 returns first two elements" $
        RBST.take 2 smallRBST `describedAs` [('A', 1), ('B', 2)]
    it "take 4 returns first four elements" $
        RBST.take 4 smallRBST `describedAs` [('A', 1), ('B', 2), ('C', 3), ('D', 4)]

dropSpec :: Spec
dropSpec = describe "drop" $ do
    it "drop negative returns tree itself" $
       RBST.drop (-1) smallRBST `shouldBe` smallRBST
    it "drop 0 returns tree itself" $
        RBST.drop 0 smallRBST `shouldBe` smallRBST
    it "drop size returns empty tree" $
        RBST.drop 5 smallRBST `shouldBe` RBST.empty
    it "drop 2 returns first two elements" $
        RBST.drop 2 smallRBST `describedAs` [('C', 3), ('D', 4), ('E', 5)]
    it "drop 4 returns first four elements" $
        RBST.drop 4 smallRBST `describedAs` [('E', 5)]

removeSpec :: Spec
removeSpec = describe "remove" $ do
    it "remove negative returns the tree itself" $
       RBST.remove (-1) smallRBST `shouldBe` smallRBST
    it "remove size returns the tree itself" $
        RBST.remove 5 smallRBST `shouldBe` smallRBST
    it "remove 0 removes the first element of the tree" $
        RBST.remove 0 smallRBST `describedAs` [('B', 2), ('C', 3), ('D', 4), ('E', 5)]
    it "remove 2 removes the 2th element of the tree" $
        RBST.remove 2 smallRBST `describedAs` [('A', 1), ('B', 2), ('D', 4), ('E', 5)]
    it "remove 4 removes the 4th element of the tree" $
        RBST.remove 4 smallRBST `describedAs` [('A', 1), ('B', 2), ('C', 3), ('D', 4)]
