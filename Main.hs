-- HOpenGL
import Graphics.UI.GLUT
import Data.IORef

-- User defined
import Callbacks

-- Constants

-- Window size.
ws = 1000
-- Grid size should always divide the window size.
gs = 5


main :: IO()
main = do
  (progName, args) <- getArgsAndInitialize
  -- initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size ws ws 
  win <- createWindow "Hello World"
  -- reshapeCallback $= Just reshape
  -- len <- newIORef 4.0
  -- pos <- newIORef (0.0, 0.0)
  displayCallback $= display 1 gs
  keyboardMouseCallback $= Just keyboardMouse
  idleCallback $= Just idle
  mainLoop
