module Main where

import Lul
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getEvent, eventName, queryTree, killClient, setEventType)
import Data.Bits ((.|.))
import Control.Monad (when, void)
import Control.Concurrent
import System.Process (spawnProcess, spawnCommand)
import System.Exit

main :: IO ()
main = do
  initThreads
  dpy <- openDisplay ""
  root <- rootWindow dpy (defaultScreen dpy)
  selectInput dpy root (   buttonPressMask
                       .|. keyPressMask
                       .|. keyReleaseMask
                       .|. structureNotifyMask
                       .|. substructureNotifyMask)
  autostart
  loop dpy root

loop :: Display -> Window -> IO ()
loop dpy root = do
  allocaXEvent $ \e -> do
    -- sync dpy False
    nextEvent dpy e
    ev <- getEvent e
    -- print . eventName $ ev
    when (eventName ev=="ButtonPress") (handleButtonPress =<< get_ButtonEvent e)
    when (eventName ev=="KeyPress") (handleKeyPress =<< get_KeyEvent e)
    when (eventName ev=="CreateNotify") (updateWindows root)
    (focused,_) <- getInputFocus dpy
    -- print $ "FOCUSED"++show focused
    print "--- awaiting event ---"
  loop dpy root
  where
    handleKeyPress :: XKeyEvent -> IO ()
    handleKeyPress k@(win,root,time,x,y,wx,wy,mod,keycode,_)
      | keycode==36 && mod==8 = void $ spawnProcess "st" []
      | keycode==24 && mod==8 = exit dpy
      | otherwise = print k
    handleButtonPress :: XButtonEvent -> IO ()
    handleButtonPress (win,root,time,x,y,wx,wy,mod,btn,_)
      | win/=root = raiseWindow dpy win
      | otherwise = print btn
    handleWindows :: [Window] -> IO ()
    handleWindows win = do
      borderColor <- initColor dpy "red"
      mapM_ (\(w,i) -> moveWindow dpy w (i*30) 30) $ zip win [0..]
      mapM_ (\w -> setWindowBorder dpy w borderColor) win
      mapM_ (\w -> setWindowBorderWidth dpy w 2) win
      arrange dpy screenRect winTree
      return ()
      where
        screenRect = Rectangle 0 15
          (fromIntegral (displayWidth  dpy (defaultScreen dpy)))
          (fromIntegral (displayHeight dpy (defaultScreen dpy)))
        winTree = fromList win (SplitData 0.5 Horizontal)
    updateWindows :: Window -> IO ()
    updateWindows root = do
      (v,w,children) <- queryTree dpy root
      print (v,w,children)
      handleWindows (drop 4 children)

exit :: Display -> IO ()
exit dpy = do
  root <- rootWindow dpy (defaultScreen dpy)
  (_,_,children) <- queryTree dpy root
  void $ forkFinally
    (mapM_ (killClient dpy) children)
    (const exitSuccess)

autostart :: IO ()
autostart = do
  spawnProcess "/home/luca/.cabal/bin/lal" []
  spawnCommand "setxkbmap de -variant nodeadkeys -option caps:swapescape"
  spawnCommand "sudo bluetooth off"
  spawnCommand "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnCommand "redshift &"
  spawnCommand "nitrogen --restore &"
  spawnCommand "picom &"
  return ()

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
