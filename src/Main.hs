module Main where

import Lul
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getEvent, eventName, queryTree, killClient, setEventType, ev_window, ev_root)
import Data.Bits ((.|.))
import Control.Monad (when, void)
import Data.List ((\\))
import Control.Concurrent
import System.Process (spawnProcess, spawnCommand)
import System.Exit
import Text.Format (format)
import Debug
import Tree
import TreeData
import StackSet
import WindowDecoration

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
                       .|. enterWindowMask
                       .|. substructureNotifyMask)
  autostart >> (print =<< queryTree dpy root) >> print "*********"
  let ss = StackSet [Workspace Empty Nothing] 0 []
  let sd = SplitData 0.5 Horizontal
  loop dpy ss sd

-- | Call this function indefinitely. It will wait for a new event to
-- occur and then it will rerun itself.
loop :: Display -> StackSet Window SplitData -> SplitData -> IO ()
loop dpy ss sd = do
  root <- rootWindow dpy (defaultScreen dpy)
  newss <- allocaXEvent $ \e -> do
    -- sync dpy False
    nextEvent dpy e
    ev <- getEvent e
    case (eventName ev) of{  
      "ButtonPress" -> do
        handleButtonPress =<< get_ButtonEvent e
        return ss
      ;
      "KeyPress" -> do
        handleKeyPress ss =<< get_KeyEvent e
      ;
      "KeyRelease" -> do
        putStrLn $ prettyPrintStackSet ss
        return ss
      ;
      "CreateNotify" -> do
        putStrLn $ format "CreateNotify| event: {0}" [show (ev)]
        newss <- handleNewWindows dpy root ss sd
        apply dpy screenRect newss
        return newss
      ;
      "DestroyNotify" -> do
        putStrLn . show $ ev
        putStrLn . show $ ss
        let closdeWindow = ev_window ev
        let newss = balanceCurrentSS $ removeFromCurrentSS ss closdeWindow
        apply dpy screenRect newss
        return newss
      ;
      "EnterNotify" -> do
        case (ev_window ev == ev_root ev) of
          True -> return ss
          False -> do
            let newss = handleFocusChange ss (ev_window ev)
            decorateStackSet dpy newss
            return newss
      ;
      _ -> do
        print . eventName $ ev
        return ss
     }
  loop dpy newss sd
  where
    screenRect = Rectangle 0 15
      (fromIntegral (displayWidth  dpy (defaultScreen dpy)))
      (fromIntegral (displayHeight dpy (defaultScreen dpy)))
    handleKeyPress :: StackSet Window SplitData -> XKeyEvent -> IO (StackSet Window SplitData)
    handleKeyPress ss k@(win,root,time,x,y,wx,wy,mod,keycode,_)
      | keycode==36 && mod==8 = spawnProcess "st" [] >> return ss
      | keycode==24 && mod==8 = exit dpy >> return ss
      | keycode==27 && mod==8 = do
        let newss = rotateSSaroundFocused 1 ss
        apply dpy screenRect newss
        return newss
      | otherwise = do
          return ss
    handleButtonPress :: XButtonEvent -> IO ()
    handleButtonPress (win,root,time,x,y,wx,wy,mod,btn,_)
      | win/=root = raiseWindow dpy win
      | otherwise = print btn

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
