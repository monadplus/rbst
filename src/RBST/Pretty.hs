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
  -- * Printing
  -- ** Pretty printing
    pretty
  , prettyPrint

  -- ** Compact printing
  , compact
  , compactPrint
  ) where

import           Data.Char     (isSpace)
import           Data.List     (dropWhileEnd, intercalate)
import           RBST.Internal (RBST (..), Size (..), Tree (..), withTree)

-- | Pretty 2-dimensional ASCII drawing of a 'RBST'.
--
-- See 'RBST.Pretty.prettyPrint' for an example.
pretty :: (Show k, Show a) => RBST k a -> String
pretty = withTree drawPretty

-- | Call 'pretty' function and output the result directly to @stdout@.
--
-- >>> let tree = (fromList $ zip ['a'..'e'] [1..5]) :: RBST Char Int
-- >>> prettyPrint  tree
--                ('d',4) [5]
--                        ╱╲
--                       ╱  ╲
--                      ╱    ╲
--                     ╱      ╲
--                    ╱        ╲
--                   ╱          ╲
--                  ╱            ╲
--                 ╱              ╲
--                ╱                ╲
--       ('b',2) [3]       ('e',5) [1]
--            ╱╲
--           ╱  ╲
--          ╱    ╲
--         ╱      ╲
--        ╱        ╲
--       ╱          ╲
-- ('a',1) [1] ('c',3) [1]
prettyPrint :: (Show k, Show a) => RBST k a -> IO ()
prettyPrint = putStrLn . pretty

-- | Comptact 2-dimensional ASCII drawing of a 'RBST'.
--
-- See 'RBST.Pretty.comptactPrint' for an example.
compact :: (Show k, Show a) => RBST k a -> String
compact = withTree drawComptact

-- | Call 'comptact' function and output the result directly to @stdout@.
--
-- >>> let tree = (fromList $ zip ['a'..'e'] [1..5]) :: RBST Char Int
-- >>> compactPrint  tree
-- ('d',4) [5]
--      |
--      |-- ('e',5) [1]
--      |
--      \__ ('b',2) [3]
--              |
--              |-- ('c',3) [1]
--              |
--              \__ ('a',1) [1]
compactPrint :: (Show k, Show a) => RBST k a -> IO ()
compactPrint = putStrLn . compact

----------------------------------------------
-- Internal functions
----------------------------------------------

-- | Shows a 'RBST.Internal.Node' with the size attached to it.
showNode :: (Show k, Show a) => Size ->  k -> a -> String
showNode s k x = "(" ++ show k ++ "," ++ show x ++ ") [" ++ (show (unSize s)) ++ "]"

-- | Draw a 'Tree' using one of the visualization modes.
drawComptact, drawPretty :: (Show k, Show a) => Tree k a -> String
drawComptact = unlines . comptactWith showNode
drawPretty = prettyWith showNode

-- | Show 'Tree' in a comptact way using given function to display node.
comptactWith
    :: forall k a .
       (Size -> k -> a -> String)
    -> Tree k a
    -> [String]
comptactWith       _            Empty = []
comptactWith display (Node s k Empty x Empty) = [display s k x]
comptactWith display (Node s k l x r) = [nodeASCII] ++ drawSubTrees
  where
    nodeASCII = display s k x

    drawSubTrees =
      let drawR = ws "|" : shift (ws "|-- ")  (ws "|  ") (comptactWith display r)
          drawL = ws "|" : shift (ws "\\__ ") (ws "   ") (comptactWith display l)
      in  drawR ++ drawL

    ws = (++) (spaces (length nodeASCII `div` 2))

    shift first other = zipWith (++) (first : repeat other)

--------------------------------------------------------------------------------
-- The following code is copied from 'Treap.Pretty.
--
-- All credit to the authors.
--------------------------------------------------------------------------------

-- | Show 'Tree' in a pretty way using given function to display node.
prettyWith
    :: forall k a.
       (Size -> k -> a -> String)
    -> Tree k a
    -> String
prettyWith display = showTree . toBinTree
  where
    toBinTree :: Tree k a -> BinTree
    toBinTree Empty            = Leaf
    toBinTree (Node s k l x r) = Branch (display s k x) (toBinTree l) (toBinTree r)

-- | Intermidiate structure to help string conversion.
data BinTree
    = Leaf
    | Branch String BinTree BinTree

-- | Hardcore function responsible for pretty showing of the 'BinTree' data type.
showTree :: BinTree -> String
showTree Leaf                  = ""
showTree (Branch label left right) = case (left, right) of
    (Leaf, Leaf) -> label

    (_, Leaf) -> toLines $
        [ spaces rootShiftOnlyLeft   ++ label
        , spaces branchShiftOnlyLeft ++ "╱"
        ] ++ map (spaces leftShiftOnlyLeft ++) leftLines

    (Leaf, _) -> toLines $
        [ spaces rootShiftOnlyRight   ++ label
        , spaces branchShiftOnlyRight ++ "╲"
        ] ++ map (spaces rightShiftOnlyRight ++) rightLines

    (_, _) -> toLines $
        [ spaces rootOffset ++ label
        ]
        ++ map (spaces rootOffset ++ ) (branchLines branchHeight)
        ++ map (spaces childrenOffset ++) (zipChildren leftLines rightLines)
  where
    leftStr, rightStr :: String
    leftStr  = showTree left
    rightStr = showTree right

    leftLines :: [String]
    leftLines  = lines leftStr
    rightLines = lines rightStr

    rootLabelMiddle, leftLabelMiddle, rightLabelMiddle :: Int
    rootLabelMiddle  = middleLabelPos label
    leftLabelMiddle  = middleLabelPos $ head leftLines
    rightLabelMiddle = middleLabelPos $ head rightLines

    -- Case 1: all offsets when node has only left branch
    rootShiftOnlyLeft, leftShiftOnlyLeft, branchShiftOnlyLeft :: Int
    (rootShiftOnlyLeft, leftShiftOnlyLeft) = case compare rootLabelMiddle leftLabelMiddle of
        EQ -> (1, 0)
        GT -> (0, rootLabelMiddle - leftLabelMiddle - 1)
        LT -> (leftLabelMiddle - rootLabelMiddle + 1, 0)
    branchShiftOnlyLeft = rootLabelMiddle + rootShiftOnlyLeft - 1

    -- Case 2: all offsets when node has only right branch
    rootShiftOnlyRight, rightShiftOnlyRight, branchShiftOnlyRight :: Int
    (rootShiftOnlyRight, rightShiftOnlyRight) = case compare rootLabelMiddle rightLabelMiddle of
        EQ -> (0, 1)
        GT -> (0, rootLabelMiddle - rightLabelMiddle + 1)
        LT -> (rightLabelMiddle - rootLabelMiddle - 1, 0)
    branchShiftOnlyRight = rootLabelMiddle + rootShiftOnlyRight + 1

    -- Case 3: both
    leftWidth, rightOffMiddle, childDistance, branchHeight, rootMustMiddle :: Int
    leftWidth      = 1 + maximum (map length leftLines)
    rightOffMiddle = leftWidth + rightLabelMiddle
    childDistance  = rightOffMiddle - leftLabelMiddle
    branchHeight   = childDistance `div` 2
    rootMustMiddle = (leftLabelMiddle + rightOffMiddle) `div` 2

    rootOffset, childrenOffset :: Int
    (rootOffset, childrenOffset) = case compare rootLabelMiddle rootMustMiddle of
        EQ -> (0, 0)
        LT -> (rootMustMiddle - rootLabelMiddle, 0)
        GT -> (0, rootLabelMiddle - rootMustMiddle)

    zipChildren :: [String] -> [String] -> [String]
    zipChildren l []          = l
    zipChildren [] r          = map (spaces leftWidth ++ ) r
    zipChildren (x:xs) (y:ys) =
        let xLen = length x
            newX = x ++ spaces (leftWidth - xLen)
        in (newX ++ y) : zipChildren xs ys

-- | Generates strings containing of @n@ spaces.
spaces :: Int -> String
spaces n = replicate n ' '

{- | Calculates position of middle of non-space part of the string.

>>> s = "   abc "
>>> length s
7
>>> middleLabelPos s
4
-}
middleLabelPos :: String -> Int
middleLabelPos s =
    let (spacePrefix, rest) = span isSpace s
    in length spacePrefix + (length (dropWhileEnd isSpace rest) `div` 2)

-- | Like 'unlines' but doesn't add "\n" to the end.
toLines :: [String] -> String
toLines = intercalate "\n"

{- | Draws branches of the given height.

>>> putStrLn $ toLines $ branchLines 1
╱╲

>>> putStrLn $ toLines $ branchLines 2
 ╱╲
╱  ╲

>>> putStrLn $ toLines $ branchLines 3
  ╱╲
 ╱  ╲
╱    ╲
-}
branchLines :: Int -> [String]
branchLines n = go 0
  where
    go :: Int -> [String]
    go i
        | i == n    = []
        | otherwise = line : go (i + 1)
      where
        line :: String
        line = spaces (n - i - 1) ++ "╱" ++ spaces (2 * i) ++ "╲"
