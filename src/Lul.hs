module Lul where

import Graphics.X11.Xlib

data Orientation = Horizontal | Vertical
  deriving (Eq, Show)

data SplitData = SplitData
  { ratio :: Float
  , orientation :: Orientation
  }
  deriving (Eq, Show)

data Workspace a b = Workspace
  { windows :: Tree a b
  , focused :: Path}
  deriving (Show, Eq)

data Tree a b
  = Branch (Tree a b) b (Tree a b)
  | Leaf a
  | Empty
  deriving (Eq, Show)

data Path
  = LeftPath  Path
  | RightPath Path
  | EndPath
  deriving (Eq, Show)

getFocused :: Tree a b -> Path -> Maybe a
getFocused (Leaf w)        EndPath      = Just w
getFocused (Leaf w)        _            = Nothing
getFocused (Branch l _ r) (LeftPath p)  = getFocused l p
getFocused (Branch l _ r) (RightPath p) = getFocused r p
getFocused _ _                          = Nothing

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

arrange :: Display -> Rectangle -> Tree Window SplitData -> IO ()
arrange dpy (Rectangle x y w h) (Leaf win) = moveResizeWindow dpy win x y w h
arrange dpy (Rectangle x y w h) (Branch l d r)
  | orientation d == Horizontal = do
    arrange dpy (Rectangle x y (w`div`2) h) l
    arrange dpy (Rectangle (x+fromIntegral (w`div`2)) y (w`div`2) h) r
  | orientation d == Vertical = do
    arrange dpy (Rectangle x y w (h`div`2)) l
    arrange dpy (Rectangle x (y+fromIntegral (h`div`2)) w (h`div`2)) r
arrange _ _ _ = return ()
