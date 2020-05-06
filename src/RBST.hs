--------------------------------------------------------------------
-- |
-- Module      :  RBST
-- Copyright   :  (c) Arnau Abella 2020
-- License     :  MIT (see the file LICENSE)
-- Maintainer  :  Arnau Abella arnauabell@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- == General description
--
-- Package @rbst@ implements a self-balancing-tree-like data structure called /Randomized Binary Search Tree/. This data structure behave exactly like a <https://en.wikipedia.org/wiki/Random_binary_tree random_binary_search_tree>, irrespectively of the input data distribution, with fast (logarithmic time complexity) @'RBST.insert'@ \/ @'RBST.delete'@ \/ @'lookup'@ \/ @'union'@ \/ etc. operations.
--
-- == Package structure
--
-- This package contains the following modules:
--
-- * __"RBST.Pretty"__: pretty-printer for the tree.
--
-- Module __"RBST"__ reexports ony __"RBST.Pretty"__ and exports all 'RBST' functionalities.
--
-- == Usage example
--
-- A balanced 'Tree' can be created from a list of keys/values:
--
-- >>> import GHC.Exts (IsList (..))
-- >>> let empty = empty :: RBST Int String
-- >>> let single = one 1 'v'
-- >>> let tree = fromList [("x", 2),("y", 3), ("z", 1)] :: RBST String Int
-- >>> prettyPrint tree
--       ("y",3) [3]
--            ╱╲
--           ╱  ╲
--          ╱    ╲
--         ╱      ╲
--        ╱        ╲
--       ╱          ╲
-- ("x",2) [1] ("z",1) [1]
--
-- Each node shows:
--
-- 1. The key.
-- 2. The associated value.
-- 3. The size of the tree.
--
-- You can try it:
--
-- >>> insert "w" 5 tree
--
-- >>> delete "u" tree
--
-- >>> lookup "y" tree
-- Just 3
--
-- >>> lookup "w" tree
-- Nothing
--
-- >>> compactPrint tree
-- ("y",3) [3]
--      |
--      |-- ("z",1) [1]
--      |
--      \__ ("x",2) [1]
--
--
-- == Implementation
--
-- The implementation of /Randomized Binary Search Trees/ is based on:
--
-- * Conrado Martinez and Salvador Roura, \"/Randomized Binary Search Trees/\", January 1997, <http://akira.ruc.dk/~keld/teaching/algoritmedesign_f08/Artikler/03/Martinez97.pdf>.
--
--------------------------------------------------------------------
module RBST (

  -- * Data structure & Instances
    Size(..)
  , Tree(..)
  , RBST(..)

  -- * Construction functions
  , empty
  , one

  -- * Query functions
  , size
  , height
  , lookup
  , at

  -- * Modification functions
  , insert
  , delete
  , remove
  , take
  , drop

  -- * Set operations
  , union
  , intersection
  , subtraction
  , difference

  -- * Reexports
  , module RBST

  ) where

import           Prelude       hiding (drop, lookup, take)
import           RBST.Internal as Internal
import           RBST.Pretty   as RBST
