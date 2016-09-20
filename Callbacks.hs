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
keyboardMouse :: IORef S.Snake -> KeyboardMouseCallback
keyboardMouse s key Down _ _ = do
  snake <- get s
  s $= case key of
    (SpecialKey KeyUp) -> S.turnSnake S.Up snake
    (SpecialKey KeyDown) -> S.turnSnake S.Down snake
    (SpecialKey KeyLeft) -> S.turnSnake S.Left snake
    (SpecialKey KeyRight) -> S.turnSnake S.Right snake
keyboardMouse _ _ _ _ _ = return ()

-- idle callback
idle :: IORef S.Snake -> IORef Game -> IO ()
idle s g = do
  snake <- get s
  game <- get g
  update <- shouldUpdate game
  currentTime <- getCurrentTime
  case update of
    True -> do
      s $= S.update snake
      g $= Game {
        defaultSpeed = 1,
        lastUpdateTime = currentTime}
    False -> return ()
  postRedisplay Nothing

-- Helper function decide whether we update.
shouldUpdate :: Game -> IO Bool
shouldUpdate g = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime $ lastUpdateTime g
  return $ timeDiff > defaultSpeed g
