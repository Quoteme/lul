module Debug where
import Lul
import Tree
import Text.Format

prettyPrintTree :: Show a => Tree a b -> Int -> String
prettyPrintTree Empty indent = replicate indent ' ' <> "🪹"
prettyPrintTree (Leaf a) indent = replicate indent ' ' <> "🌿 " <> show a
prettyPrintTree (Branch l _ r) indent = replicate indent ' ' <> "🌳\n" <> prettyPrintTree l (indent+1) <> "\n" <> prettyPrintTree r (indent+1)

prettyPrintStackSet :: Show a => StackSet a b -> String
prettyPrintStackSet (StackSet ws active _) = "🌲\n" <> prettyPrintTree (windows (ws !! active)) 1
