module Debug where

import Text.Format
import Lul
import Tree
import StackSet

prettyPrintTree :: (Show a, Show b) => Tree a b -> Int -> String
prettyPrintTree Empty indent = replicate indent ' ' <> "🪹"
prettyPrintTree (Leaf a) indent = replicate indent ' ' <> "🌿 " <> show a
prettyPrintTree (Branch l b r) indent = replicate indent ' ' <> "🌳\n"
                                      <> prettyPrintTree l (indent+1) <> "\n"
                                      <> replicate (indent+1) ' ' <> ( show b ) <> "\n"
                                      <> prettyPrintTree r (indent+1)

prettyPrintStackSet :: (Show a, Show b) => StackSet a b -> String
prettyPrintStackSet (StackSet ws active _) = "🌲\n" <> prettyPrintTree (windows (ws !! active)) 1
