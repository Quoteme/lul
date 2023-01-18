module WindowDecoration where
import Graphics.X11

decorateWin :: Display -> String -> Window -> IO ()
decorateWin dpy color win  = do
  borderColor <- initColor dpy color
  setWindowBorder dpy win borderColor
  setWindowBorderWidth dpy win 2

initColor :: Display -> String -> IO Pixel
initColor dpy color = do
  let colormap = defaultColormap dpy (defaultScreen dpy)
  (apros,real) <- allocNamedColor dpy colormap color
  return $ color_pixel apros
