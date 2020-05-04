module Test.Common (
    TestRBST
  , smallRBST
  ) where

import           GHC.Exts (IsList (..))
import           RBST     (RBST)

type TestRBST = RBST Char Int

smallRBST :: TestRBST
smallRBST = fromList $ zip ['A'..'E'] [1..5]
