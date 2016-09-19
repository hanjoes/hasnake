module Callbacks where

import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

import Grid
import Game

import qualified Snake as S

-- Takes ng as number of total grids to render.
display :: GLfloat -> GLfloat -> IORef S.Snake -> IO ()
display ws s snake = do
  clear [ColorBuffer]
  renderGrids ws s snake
  flush

-- keyboard and mouse callback
keyboardMouse :: KeyboardMouseCallback
keyboardMouse _ _ _ _ = return ()

-- idle callback
idle :: IORef S.Snake -> IORef Game -> IO ()
idle s g = do
  snake <- readIORef s
  game <- readIORef g
  update <- shouldUpdate game
  currentTime <- getCurrentTime
  case update of
    True -> do
      writeIORef s $ S.update snake
      writeIORef g $ Game {
        defaultSpeed = 1,
        lastUpdateTime = currentTime}
    False -> return ()

-- Helper function decide whether we update.
shouldUpdate :: Game -> IO Bool
shouldUpdate g = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime $ lastUpdateTime g
  return $ timeDiff > defaultSpeed g
