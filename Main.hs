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
  -- reshapeCallback $= Just reshape
  displayCallback $= display (fromIntegral ws) (fromIntegral gs)
  keyboardMouseCallback $= Just keyboardMouse

  -- initialize snake
  let snake = S.Snake {
        S.body = [(0, 0), ((fromIntegral gs) / (fromIntegral ws), 0)],
        S.dir = S.Right,
        S.clr = Color3 1 1 (0 :: GLfloat)
        }

  idleCallback $= (Just $ idle snake)
  mainLoop
