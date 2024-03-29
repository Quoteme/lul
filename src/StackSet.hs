module StackSet where

import Data.List ((\\))
import Tree
import TreeData
import Graphics.X11.Xlib
import Text.Format (format)
import Graphics.X11.Xlib.Extras
import WindowDecoration
import Data.Bifoldable
import Data.Maybe (fromJust)
import Data.Bits ((.|.))

data StackSet a b = StackSet
  { workspaces :: [Workspace a b]
  , active     :: Int
  , registeredWindows :: [a]
  , borderSize :: Int
  }
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


-- | This method is called, whenever a new window is created. It will
-- map the window, add it to the registered windows and then
-- automatically add it to the current workspace.
handleNewWindows :: Display -> Window -> StackSet Window SplitData -> SplitData -> IO (StackSet Window SplitData)
handleNewWindows dpy root ss sd = do
  (_,_,children) <- queryTree dpy root
  let newwin = children \\ registeredWindows ss
  mapM_ (\win -> do
    selectInput dpy win (enterWindowMask .|. keyPressMask)
    return ()
    ) newwin
  mapM_ (decorateWin dpy "black" (borderSize ss)) newwin
  let newss  = foldl (`addToCurrentSS` sd) ss newwin
  return (newss {registeredWindows=registeredWindows ss ++ newwin})

-- | set the focused window in the stackset
handleFocusChange :: StackSet Window SplitData -> Window -> StackSet Window SplitData
handleFocusChange ss win = ss { workspaces=l<>[c]<>r }
  where
    l = take (active ss-1) $ workspaces ss
    c = Workspace (windows (workspaces ss !! active ss)) (Just win)
    r = drop (active ss+1)   $ workspaces ss
    

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

-- | set the border colors of the windows in the active workspace.
-- This is primarily done, when the focus changes, so the active window
-- gets a different border color.
decorateStackSet :: Display -> StackSet Window SplitData -> IO ()
decorateStackSet dpy ss = do
  case focused (current ss) of
    Just focusedWin -> do
      putStrLn $ "focused: " ++ show focusedWin
      bimapM_ (\win -> case win == focusedWin of
                     True  -> decorateWin dpy "red" (borderSize ss) win
                     False -> decorateWin dpy "black" (borderSize ss) win
        ) (\_ -> return ()) (windows (current ss))
    Nothing  -> return ()

-- | Change the rotation of the window tree for the `n-th` parent of the 
-- focused window.
rotateSSaroundFocused :: Int -> StackSet Window SplitData -> StackSet Window SplitData
rotateSSaroundFocused n ss = ss {workspaces=l<>[c]<>r}
  where
    l = take (active ss-1) $ workspaces ss
    c = rotateWorkspace n (current ss)
    r = drop (active ss+1)   $ workspaces ss

-- | Change the rotation of the window tree for the `n-th` parent
-- of the focused window in a workspace.
rotateWorkspace :: Int -> Workspace Window SplitData -> Workspace Window SplitData
rotateWorkspace n ws = case focused ws of
  Just win -> ws {windows=rotateTree (windows ws) n win}
  Nothing  -> ws
