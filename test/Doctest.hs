module Main (main) where

import System.FilePath.Glob (glob)
import Test.DocTest         (doctest)

main :: IO ()
main = do
    sourceFiles <- glob "src/**/*.hs"
    doctest
        $ "-XAllowAmbiguousTypes"
        : "-XBangPatterns"
        : "-XConstraintKinds"
        : "-XDataKinds"
        : "-XDefaultSignatures"
        : "-XDeriveAnyClass"
        : "-XDeriveDataTypeable"
        : "-XDeriveFoldable"
        : "-XDeriveFunctor"
        : "-XDeriveGeneric"
        : "-XDeriveTraversable"
        : "-XDerivingStrategies"
        : "-XDerivingVia"
        : "-XDuplicateRecordFields"
        : "-XEmptyCase"
        : "-XEmptyDataDecls"
        : "-XFlexibleContexts"
        : "-XFlexibleInstances"
        : "-XFunctionalDependencies"
        : "-XGADTs"
        : "-XGeneralizedNewtypeDeriving"
        : "-XInstanceSigs"
        : "-XKindSignatures"
        : "-XLambdaCase"
        : "-XNamedFieldPuns"
        : "-XOverloadedStrings"
        : "-XPolyKinds"
        : "-XQuasiQuotes"
        : "-XRankNTypes"
        : "-XRecordWildCards"
        : "-XScopedTypeVariables"
        : "-XStandaloneDeriving"
        : "-XTemplateHaskell"
        : "-XTupleSections"
        : "-XTypeApplications"
        : "-XTypeFamilies"
        : "-XTypeOperators"
        : "-XUndecidableInstances"
        : "-XRoleAnnotations"
        : sourceFiles

