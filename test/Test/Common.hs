module Test.Common (
    TestRBST
  , smallRBST
  , describedAs
  ) where

import           GHC.Exts                (IsList (..))
import           RBST                    (RBST)
import           Test.Hspec.Expectations (Expectation, shouldBe)

type TestRBST = RBST Char Int

smallRBST :: TestRBST
smallRBST = fromList $ zip ['A'..'E'] [1..5]

describedAs :: TestRBST -> [(Char, Int)] -> Expectation
describedAs tree expectedTreeNodes = toList tree `shouldBe` expectedTreeNodes
infixr 8 `describedAs`
