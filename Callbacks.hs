module Callbacks where

import Graphics.UI.GLUT
import Data.IORef

import Grid
import Snake

-- Takes ng as number of total grids to render.
display :: GLfloat -> GLfloat -> IO ()
display ws s = do
  clear [ColorBuffer]
  renderGrids ws s
  flush

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _ _ _ _ = return ()

idle :: Snake -> IO ()
idle _ = return ()
