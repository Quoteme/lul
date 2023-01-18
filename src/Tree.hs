module Tree where

import Prelude hiding (Left, Right)
import TreeData
import Data.Bifunctor
import Data.Bifoldable
import Path
import Data.Maybe (isJust, fromJust)

data Tree a b
  = Branch (Tree a b) b (Tree a b)
  | Leaf a
  | Empty
  deriving (Eq, Show)

instance Bifunctor Tree where
  bimap f g (Branch l d r) = Branch (bimap f g l) (g d) (bimap f g r)
  bimap f _ (Leaf a) = Leaf (f a)
  bimap _ _ Empty = Empty

instance Bifoldable Tree where
  bifoldMap f g (Branch l d r) = bifoldMap f g l <> g d <> bifoldMap f g r
  bifoldMap f _ (Leaf a) = f a
  bifoldMap _ _ Empty = mempty

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

-- get the path to a node in a tree
pathTo :: (Eq a) => Tree a b -> a -> Maybe Path
pathTo Empty _ = Nothing
pathTo (Leaf a) a'
  | a==a' = Just $ Path []
  | otherwise = Nothing
pathTo (Branch l _ r) a
  | isJust leftPath = Just $ (Path [Left] <> fromJust leftPath)
  | isJust rightPath = Just $ (Path [Right] <> fromJust rightPath)
  | otherwise = Nothing
  where
    leftPath = pathTo l a
    rightPath = pathTo r a

-- | rotate the `n-th` parent of a leaf in a tree
rotateTree :: (Eq a) => Tree a SplitData -> Int -> a -> Tree a SplitData
rotateTree t n a = case pathTo t a of
  Nothing -> t
  Just p@(Path v) -> rotateTree' t (length v - n) p
  where
    rotateTree' :: Tree a SplitData -> Int -> Path -> Tree a SplitData
    rotateTree' Empty 0 _ = Empty
    rotateTree' (Leaf a) 0 _ = Leaf a
    rotateTree' (Branch l b r) 0 _ = Branch r (rotateSplit b) l
    rotateTree' t _ (Path []) = t
    rotateTree' (Branch l b r) n (Path (Left:ps)) = Branch (rotateTree' l (n-1) (Path ps)) b r
    rotateTree' (Branch l b r) n (Path (Right:ps)) = Branch l b (rotateTree' r (n-1) (Path ps))
