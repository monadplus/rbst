module Test.RBST.UpdateSpec (
    spec
  ) where

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Common (smallRBST, describedAs)

import qualified RBST

spec :: Spec
spec = describe "Modification operations tests" $ do
  insertSpec
  deleteSpec

insertSpec :: Spec
insertSpec = describe "insert" $ do
  it "insert on empty tree works" $
    RBST.insert 'A' 1 RBST.empty `describedAs` [('A', 1)]
  it "insert on test tree works" $
    RBST.insert 'F' 6 smallRBST `describedAs` [('A', 1), ('B', 2), ('C', 3), ('D', 4), ('E', 5), ('F', 6)]
  it "insert a repeated key should update the item" $
    RBST.insert 'E' 0 smallRBST `describedAs` [('A', 1), ('B', 2), ('C', 3), ('D', 4), ('E', 0)]

deleteSpec :: Spec
deleteSpec = describe "delete" $ do
  it "delete removes a key from the tree" $
    RBST.delete 'E' smallRBST `describedAs` [('A', 1), ('B', 2), ('C', 3), ('D', 4)]
  it "delete a missing key has no effect" $
    RBST.delete 'F' smallRBST `shouldBe` smallRBST
  it "delete should work on empty trees" $
    ( RBST.delete 'A'
    $ RBST.delete 'B'
    $ RBST.delete 'C'
    $ RBST.delete 'D'
    $ RBST.delete 'E'
    $ RBST.delete 'E'
    smallRBST ) `shouldBe` RBST.empty
