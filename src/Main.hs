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
gs = 20


main :: IO()
main = do
  (progName, args) <- getArgsAndInitialize
  -- initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size ws ws
  win <- createWindow "Hasnake"

  -- initialize snake
  let snake = initializeSnake ng

  -- initialize the first bean
  initialBeanLocation <- getBeanLocation (ng) $ body snake

  let bean = Bean {
    beanLocation = initialBeanLocation,
    beanColor = Color3 0 1 (0 :: GLfloat),
    eaten = False
  }

  -- initialize game (IORef)
  currentTime <- getCurrentTime
  game <- newIORef $ Game {
    defaultSpeed = 0.1,
    lastUpdateTime = currentTime,
    hasnake = snake,
    currentBean = bean,
    gameWindowSize = fromIntegral ws,
    gridSize = fromIntegral gs
  }

  -- register callbacks
  -- reshapeCallback $= Just reshape
  displayCallback $= display game
  keyboardMouseCallback $= (Just $ keyboardMouse game)
  idleCallback $= (Just $ idle game)
  mainLoop

  where ng = fromIntegral $ ws `div` gs
