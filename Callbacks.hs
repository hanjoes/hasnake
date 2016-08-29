module Callbacks where

import Graphics.UI.GLUT
import Data.IORef

import Grid

-- Takes ng as number of total grids to render.
display :: Int -> GLsizei -> IO ()
display ng s = do
  clear [ColorBuffer]
  renderGrids ng s
  flush

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _ _ _ _ = return ()

idle :: IO ()
idle = return ()
