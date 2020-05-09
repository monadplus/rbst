module Test.RBST (
    rbstSpec
  ) where

import Test.Hspec (Spec, describe)

import Test.RBST.Cuts   (cutsSpec)
import Test.RBST.Laws   (lawsSpec)
import Test.RBST.Query  (querySpec)
import Test.RBST.SetOps (setOpsSpec)
import Test.RBST.Update (updateSpec)

rbstSpec :: Spec
rbstSpec = describe "RBST" $ do
  cutsSpec
  lawsSpec
  querySpec
  setOpsSpec
  updateSpec
