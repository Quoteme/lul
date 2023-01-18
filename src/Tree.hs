module Tree where

data Tree a b
  = Branch (Tree a b) b (Tree a b)
  | Leaf a
  | Empty
  deriving (Eq, Show)

-- | Create a simple tree from a list
fromList :: [a] -> b -> Tree a b
fromList []  _   = Empty
fromList [x] _   = Leaf x
fromList xs  arg = Branch
  (fromList firstHalf arg)
  arg
  (fromList secondHalf arg)
  where
    firstHalf  = take (length xs `div` 2) xs
    secondHalf = drop (length xs `div` 2) xs

-- | Create a list from a tree
toList :: Tree a b -> [a]
toList Empty = []
toList (Leaf a) = [a]
toList (Branch l _ r) = toList l <> toList r

-- | Append a new element to the tree
append :: Tree a b -> b -> a -> Tree a b
append Empty _ a = Leaf a
append (Leaf l) b r = Branch (Leaf l) b (Leaf r)
append (Branch l b1 r1) b2 r2 = Branch l b1 (Branch r1 b2 (Leaf r2))

-- | remove an element from the tree
remove :: (Eq a) => Tree a b -> a -> Tree a b
remove Empty _ = Empty
remove (Leaf a) a' | a==a' = Empty
remove (Leaf a) a' | a/=a' = Leaf a
remove (Branch l b r) a = Branch (remove l a) b (remove r a)

balance :: Tree a b -> Tree a b
balance Empty = Empty
balance (Leaf a) = Leaf a
balance (Branch Empty _ Empty) = Empty
balance (Branch l _ Empty) = l
balance (Branch Empty _ r) = r
balance (Branch l b r) = Branch (balance l) b (balance r)

-- | remove all the elements given in a list from the tree
removeAll :: (Eq a) => Tree a b -> [a] -> Tree a b
removeAll t [] = t
removeAll t (x:xs) = removeAll (remove t x) xs
