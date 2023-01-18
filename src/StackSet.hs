module StackSet where

import Data.List ((\\))
import Tree
import TreeData
import Graphics.X11.Xlib
import Text.Format (format)
import Graphics.X11.Xlib.Extras
import WindowDecoration
import Data.Bifoldable

data StackSet a b = StackSet
  { workspaces :: [Workspace a b]
  , active     :: Int
  , registeredWindows :: [a] }
  deriving (Show, Eq)

data Workspace a b = Workspace
  { windows :: Tree a b
  , focused :: Maybe Window}
  deriving (Show, Eq)

data Path
  = LeftPath  Path
  | RightPath Path
  | EndPath
  deriving (Eq, Show)


handleNewWindows :: Display -> Window -> StackSet Window SplitData -> SplitData -> IO (StackSet Window SplitData)
handleNewWindows dpy root ss sd = do
  (_,_,children) <- queryTree dpy root
  let newwin = children \\ registeredWindows ss
  mapM_ (decorateWin dpy "black") newwin
  let newss  = foldl (`addToCurrentSS` sd) ss newwin
  return (newss {registeredWindows=registeredWindows ss ++ newwin})

-- | Given the display, its size as a rectangle and a Binary tree of
-- windows and their data on how to split, show this on screen.
arrange :: Display -> Rectangle -> Tree Window SplitData -> IO ()
arrange dpy (Rectangle x y w h) (Leaf win) = do
  print $ format "Leaf: {0}" [show win]
  moveResizeWindow dpy win x y w h
arrange dpy (Rectangle x y w h) (Branch l d r)
  | orientation d == Horizontal = do
    print "horizontal split"
    print $ format "x: {0}, y: {1}, w: {2}, h: {3}" [(show x), (show y), (show w), (show h)]
    arrange dpy (Rectangle x y (w`div`2) h) l
    arrange dpy (Rectangle (x+fromIntegral (w`div`2)) y (w`div`2) h) r
  | orientation d == Vertical = do
    print "horizontal split"
    print $ format "x: {0}, y: {1}, w: {2}, h: {3}" [(show x), (show y), (show w), (show h)]
    arrange dpy (Rectangle x y w (h`div`2)) l
    arrange dpy (Rectangle x (y+fromIntegral (h`div`2)) w (h`div`2)) r
arrange _ _ _ = return ()

-- | Show the contents of the StackSet on screen
apply :: Display -> Rectangle -> StackSet Window SplitData -> IO ()
apply dpy rect ss = do
  -- first hide all workspaces which are not active / marked as hidden
  mapM_ hide (hidden ss)
  -- now show only the windows from the active workspace
  arrange dpy rect (windows (current ss))
    where
      hide :: Workspace Window SplitData -> IO ()
      hide ws = mapM_ (\w -> moveWindow dpy w (-10000) (-10000)) (toList (windows ws))

-- | Get the current workspace
current :: StackSet a b -> Workspace a b
current ss = workspaces ss!!active ss

-- | Get the hidden workspaces
hidden :: StackSet a b -> [Workspace a b]
hidden ss = l ++ r
  where
    l = take (active ss-1) $ workspaces ss
    r = drop (active ss+1)   $ workspaces ss

-- | add a Workspace that contains leafs of type `a` and nodes between them of type `b`
addToWorkspace :: Workspace a b -> b -> a -> Workspace a b
addToWorkspace ws b a = ws {windows=append (windows ws) b a}

-- | add a window to the active workspace of a stackset
addToCurrentSS :: StackSet a b -> b -> a -> StackSet a b
addToCurrentSS ss b a = ss {workspaces=l<>[c]<>r}
  where
    l = take (active ss-1) $ workspaces ss
    c = addToWorkspace (current ss) b a
    r = drop (active ss+1)   $ workspaces ss

-- | delete a window from a workspace
removeFromWorkspace :: (Eq a) => Workspace a b -> a -> Workspace a b
removeFromWorkspace ws win = ws {windows=remove (windows ws) win}

-- | delete a window from the active workspace of a stackset
removeFromCurrentSS :: (Eq a) => StackSet a b -> a -> StackSet a b
removeFromCurrentSS ss win = ss
  { workspaces=l<>[c]<>r,
  registeredWindows = filter (/=win) (registeredWindows ss)}
  where
    l = take (active ss-1) $ workspaces ss
    c = removeFromWorkspace (current ss) win
    r = drop (active ss+1)   $ workspaces ss

-- | balance the windowtree in a workspace
balanceWorkspace :: Workspace a b -> Workspace a b
balanceWorkspace ws = ws {windows=balance (windows ws)}

-- | balance the windowtree in the active workspace of a stackset
balanceCurrentSS :: StackSet a b -> StackSet a b
balanceCurrentSS ss = ss {workspaces=l<>[c]<>r}
  where
    l = take (active ss-1) $ workspaces ss
    c = balanceWorkspace (current ss)
    r = drop (active ss+1)   $ workspaces ss

-- | decorate the windows in a stackset
-- This primarily changes the border color of the focused window to red
-- and the border color of the unfocused windows to black
decorateStackSet :: Display -> StackSet Window SplitData -> IO (StackSet Window SplitData)
decorateStackSet dpy ss = do
  (focused,_) <- getInputFocus dpy
  putStrLn $ "focused: " ++ show focused
  bimapM_ (\win -> case win == focused of
                     True  -> decorateWin dpy "red" win
                     False -> decorateWin dpy "black" win
    ) (\_ -> return ()) (windows (current ss))
  return ss
