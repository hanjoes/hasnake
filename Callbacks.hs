module Callbacks where

import Graphics.UI.GLUT
import Data.IORef

import Grid
import qualified Snake as S

-- Takes ng as number of total grids to render.
display :: GLfloat -> GLfloat -> IORef S.Snake -> IO ()
display ws s snake = do
  clear [ColorBuffer]
  renderGrids ws s snake
  flush

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _ _ _ _ = return ()

idle :: IORef S.Snake -> IO ()
idle s = do
  snake <- readIORef s
  writeIORef s $ S.update snake
