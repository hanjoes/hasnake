-- HOpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

-- User defined
import Callbacks
import Game
import Snake
import Utils

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
  snake <- newIORef $ Snake {
        body = [(ng / 2, ng / 2), (ng / 2, ng / 2 + 1)],
        dir = HSRight,
        bodyColor = Color3 1 0 (0 :: GLfloat)}

  -- initialize game
  currentTime <- getCurrentTime
  game <- newIORef $ Game {
    defaultSpeed = 1,
    lastUpdateTime = currentTime}

  -- register callbacks
  -- reshapeCallback $= Just reshape
  displayCallback $= display (fromIntegral ws) (fromIntegral gs) snake
  keyboardMouseCallback $= (Just $ keyboardMouse snake)
  idleCallback $= (Just $ idle snake game)
  mainLoop

  where ng = fromIntegral $ ws `div` gs
