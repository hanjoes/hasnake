-- HOpenGL
import Graphics.UI.GLUT
import Data.IORef

-- User defined
import Callbacks
import qualified Snake as S

-- Constants

-- Window size.
ws = 1000
-- Grid size should always divide the window size.
gs = 10


main :: IO()
main = do
  (progName, args) <- getArgsAndInitialize
  -- initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size ws ws
  win <- createWindow "Hasnake"

  -- initialize snake
  snake <- newIORef $ S.Snake {
        S.body = [(0, 0), ((fromIntegral gs) / (fromIntegral ws), 0)],
        S.dir = S.Right,
        S.clr = Color3 1 0 (0 :: GLfloat)
        }

  -- reshapeCallback $= Just reshape
  displayCallback $= display (fromIntegral ws) (fromIntegral gs) snake
  keyboardMouseCallback $= Just keyboardMouse

  idleCallback $= (Just $ idle snake)
  mainLoop
