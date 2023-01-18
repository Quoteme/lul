module Tree where

data Tree a b
  = Branch (Tree a b) b (Tree a b)
  | Leaf a
  | Empty
  deriving (Eq, Show)
