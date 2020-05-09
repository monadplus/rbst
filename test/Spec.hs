module Main (main) where

import           System.IO  (hSetEncoding, stderr, stdout, utf8)
import           Test.Hspec (Spec, hspec)
import           Test.RBST  (rbstSpec)

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8

    hspec allUnitTests

allUnitTests :: Spec
allUnitTests = rbstSpec
