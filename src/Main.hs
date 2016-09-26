-- HOpenGL
import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

-- User defined
import Callbacks
import Game
import Snake
import Bean
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
  let snake = Snake {
    body = [(ng / 2, ng / 2), (ng / 2, ng / 2 + 1)],
    dir = HSRight,
    bodyColor = Color3 1 0 (0 :: GLfloat)
  }

  -- initialize the first bean
  initialBeanLocation <- getBeanLocation (ng, ng) $ body snake

  let bean = Bean {
    beanLocation = initialBeanLocation,
    beanColor = Color3 0 1 (0 :: GLfloat)
  }

  -- initialize game (IORef)
  currentTime <- getCurrentTime
  game <- newIORef $ Game {
    defaultSpeed = 0.2,
    lastUpdateTime = currentTime,
    hasnake = snake,
    currentBean = bean
  }

  -- register callbacks
  -- reshapeCallback $= Just reshape
  displayCallback $= display (fromIntegral ws) (fromIntegral gs) game
  keyboardMouseCallback $= (Just $ keyboardMouse game)
  idleCallback $= (Just $ idle game)
  mainLoop

  where ng = fromIntegral $ ws `div` gs
