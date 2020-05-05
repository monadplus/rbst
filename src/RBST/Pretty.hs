--------------------------------------------------------------------
-- |
-- Module      : RBST.Pretty
-- Copyright   : (C) Arnau Abella 2020
-- License     : MIT (see the file LECENSE)
-- Maintainer  : Arnau Abella arnauabell@gmail.com
-- Stability   : experimental
-- Portability : non-portable
--
-- 'RBST' visualization.
--
--------------------------------------------------------------------
module RBST.Pretty (
  -- ** Tree printinting
    prettyTree
  , prettyPrintTree

  -- ** RBST printing
  , pretty
  , prettyPrint
  ) where

import           RBST.Internal (Tree(..), RBST(..), Size(..), withTree)
import           Data.Coerce (Coercible, coerce)

-- | 2-dimensional ASCII drawing of a 'Tree'.
--
-- > putStr $ drawTree $ fromList [("A", 3), ("B", 5), ("C", 2)]
--
-- @
-- TODO
-- @
prettyTree :: forall k i a b. (Coercible k i, Show i, Coercible a b, Show b)
       => Tree k a -> String
prettyTree = unlines . draw  @k @i @a @b

-- | Call 'pretty' function and output the result directly to @stdout@.
prettyPrintTree :: forall k i a b. (Coercible k i, Show i, Coercible a b, Show b)
            => Tree k a -> IO ()
prettyPrintTree = putStrLn . prettyTree @k @i @a @b

-- | 2-dimensional ASCII drawing of a 'RBST'.
--
-- > putStr $ drawTree $ fromList [("A", 3), ("B", 5), ("C", 2)]
--
-- @
-- TODO
-- @
pretty :: forall k i a b. (Coercible k i, Show i, Coercible a b, Show b)
       => RBST k a -> String
pretty = withTree (unlines . (draw  @k @i @a @b))

-- | Call 'pretty' function and output the result directly to @stdout@.
prettyPrint :: forall k i a b. (Coercible k i, Show i, Coercible a b, Show b)
            => RBST k a -> IO ()
prettyPrint = putStrLn . pretty @k @i @a @b

----------------------------------------------
-- Internal functions
----------------------------------------------

draw :: forall k i a b. (Coercible k i, Show i, Coercible a b, Show b)
       => Tree k a -> [String]
draw Empty = []
draw (Node s k l x r) = [drawNode] ++ drawSubTrees
  where
    drawNode = show (coerce @k @i k) ++ " → " ++ show (coerce @a @b x) ++ " (" ++ (show (unSize s)) ++ ")"

    drawSubTrees =
      let drawR = "|" : shift "+-- " "|  " (draw @k @i @a @b r)
          drawL = "|" : shift "`-- " "   " (draw @k @i @a @b l)
      in  drawR ++ drawL

    shift first other = zipWith (++) (first : repeat other)
