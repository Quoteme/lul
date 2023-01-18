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
import Text.Format (format)
import Debug
import Tree

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
  newss <- allocaXEvent $ \e -> do
    -- sync dpy False
    nextEvent dpy e
    ev <- getEvent e
    (focused,_) <- getInputFocus dpy
    case (eventName ev) of{  
      "ButtonPress" -> do
        handleButtonPress =<< get_ButtonEvent e
        return ss
      ;
      "KeyPress" -> do
        handleKeyPress =<< get_KeyEvent e
        return ss
      ;
      "KeyRelease" -> do
        putStrLn $ prettyPrintStackSet ss
        return ss
      ;
      "CreateNotify" -> do
        putStrLn "CreateNotify"
        newss <- updateStackSet root ss
        apply dpy screenRect ss
        return newss
      ;
      "DestroyNotify" -> do
        print ev
        let closdeWindow = ev_window ev
        newss <- updateStackSet root $ balanceCurrentSS $ removeFromCurrentSS ss closdeWindow
        apply dpy screenRect newss
        return newss
        -- newss <- updateStackSet root ss
        -- apply dpy screenRect ss
        -- return ss
      ;
      _ -> do
        print . eventName $ ev
        return ss
     }
    -- when (eventName ev=="ButtonPress") (handleButtonPress =<< get_ButtonEvent e)
    -- when (eventName ev=="KeyPress") (handleKeyPress =<< get_KeyEvent e)
    -- when (eventName ev=="CreateNotify") (updateStackSet root ss >>= apply dpy screenRect)
    -- when (eventName ev=="DestroyNotify") (removeObsoleteWin (ev_window ev))
    -- print $ "FOCUSED"++show focused
    -- print "--- awaiting event ---"
    -- return ()
  -- newss <- updateStackSet root ss
  loop dpy newss sd
  where
    screenRect = Rectangle 0 15
      (fromIntegral (displayWidth  dpy (defaultScreen dpy)))
      (fromIntegral (displayHeight dpy (defaultScreen dpy)))
    handleKeyPress :: XKeyEvent -> IO ()
    handleKeyPress k@(win,root,time,x,y,wx,wy,mod,keycode,_)
      | keycode==36 && mod==8 = void $ spawnProcess "st" []
      | keycode==24 && mod==8 = exit dpy
      | otherwise = do
          -- print k
          return ()
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
      -- print $ format "updateStackSet| root: {0}, ss: {1}" [show root, show ss]
      putStrLn $ prettyPrintStackSet ss
      (_,_,children) <- queryTree dpy root
      let newwin = children \\ registeredWindows ss
      putStrLn $ format "updateStackSet| newwin: {0}" [show newwin]
      mapM_ decorateWin newwin
      let newss  = foldl (`addToCurrentSS` sd) ss newwin
      print $ format "updateStackSet| children: {0}" [show children]
      putStrLn $ prettyPrintStackSet newss
      return (newss {registeredWindows=registeredWindows ss ++ newwin})
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
