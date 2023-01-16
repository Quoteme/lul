module Main where

import Lul
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getEvent, eventName, queryTree, killClient, setEventType, ev_window)
import Data.Bits ((.|.))
import Control.Monad (when, void)
import Data.List ((\\))
import Control.Concurrent
import System.Process (spawnProcess, spawnCommand)
import System.Exit

main :: IO ()
main = do
  putStrLn "Starting Lul"
  initThreads
  dpy <- openDisplay ""
  root <- rootWindow dpy (defaultScreen dpy)
  selectInput dpy root (   buttonPressMask
                       .|. keyPressMask
                       .|. keyReleaseMask
                       .|. structureNotifyMask
                       .|. substructureNotifyMask)
  autostart >> (print =<< queryTree dpy root) >> print "*********"
  let ss = StackSet [Workspace Empty EndPath] 0 []
  let sd = SplitData 0.5 Horizontal
  loop dpy ss sd

loop :: Display -> StackSet Window SplitData -> SplitData -> IO ()
loop dpy ss sd = do
  root <- rootWindow dpy (defaultScreen dpy)
  allocaXEvent $ \e -> do
    -- sync dpy False
    nextEvent dpy e
    ev <- getEvent e
    print . eventName $ ev
    when (eventName ev=="ButtonPress") (handleButtonPress =<< get_ButtonEvent e)
    when (eventName ev=="KeyPress") (handleKeyPress =<< get_KeyEvent e)
    when (eventName ev=="CreateNotify") (updateStackSet root ss >>= apply dpy screenRect)
    when (eventName ev=="DestroyNotify") (removeObsoleteWin (ev_window ev))
    (focused,_) <- getInputFocus dpy
    -- print $ "FOCUSED"++show focused
    print "--- awaiting event ---"
  newss <- updateStackSet root ss
  loop dpy newss sd
  where
    screenRect = Rectangle 0 15
      (fromIntegral (displayWidth  dpy (defaultScreen dpy)))
      (fromIntegral (displayHeight dpy (defaultScreen dpy)))
    handleKeyPress :: XKeyEvent -> IO ()
    handleKeyPress k@(win,root,time,x,y,wx,wy,mod,keycode,_)
      | keycode==36 && mod==8 = void $ spawnProcess "st" []
      | keycode==24 && mod==8 = exit dpy
      | otherwise = print k
    handleButtonPress :: XButtonEvent -> IO ()
    handleButtonPress (win,root,time,x,y,wx,wy,mod,btn,_)
      | win/=root = raiseWindow dpy win
      | otherwise = print btn
    decorateWin :: Window -> IO ()
    decorateWin win = do
      borderColor <- initColor dpy "red"
      setWindowBorder dpy win borderColor
      setWindowBorderWidth dpy win 2
    updateStackSet :: Window -> StackSet Window SplitData -> IO (StackSet Window SplitData)
    updateStackSet root ss = do
      (v,w,children) <- queryTree dpy root
      let newwin = drop 1 $ children \\ registered ss
      mapM_ decorateWin newwin
      let newss  = foldl (`addToCurrentSS` sd) ss newwin
      return (newss {registered=registered ss ++ newwin})
    removeObsoleteWin :: Window -> IO ()
    removeObsoleteWin win = print win

exit :: Display -> IO ()
exit dpy = do
  root <- rootWindow dpy (defaultScreen dpy)
  (_,_,children) <- queryTree dpy root
  void $ forkFinally
    (mapM_ (killClient dpy) children)
    (const exitSuccess)

autostart :: IO ()
autostart = do
  -- spawnProcess "/home/luca/.cabal/bin/lal" []
  -- spawnCommand "setxkbmap de -variant nodeadkeys -option caps:swapescape"
  -- spawnCommand "sudo bluetooth off"
  -- spawnCommand "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  -- spawnCommand "redshift &"
  -- spawnCommand "nitrogen --restore &"
  -- spawnCommand "picom &"
  -- spawnProcess "st" []
  return ()

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
