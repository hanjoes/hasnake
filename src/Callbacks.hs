module Callbacks where

import Graphics.UI.GLUT
import Data.IORef
import Data.Time.Clock

import Grid
import Game
import Utils
import Snake

-- Takes ng as number of total grids to render.
display :: IORef Game -> IO ()
display game = do
  clear [ColorBuffer]
  renderGame game
  flush

-- keyboard and mouse callback
keyboardMouse :: IORef Game -> KeyboardMouseCallback
keyboardMouse g key Down _ _ = do
  game <- get g
  let snake = hasnake game
  g $= game {
    hasnake = case key of
      (SpecialKey KeyUp) -> turnSnake HSUp snake
      (SpecialKey KeyDown) -> turnSnake HSDown snake
      (SpecialKey KeyLeft) -> turnSnake HSLeft snake
      (SpecialKey KeyRight) -> turnSnake HSRight snake
  }
keyboardMouse _ _ _ _ _ = return ()

-- idle callback
idle :: IORef Game -> IO ()
idle g = do
  game <- get g

  -- check game status

  flag <- shouldUpdate game
  case flag of
    True -> do
      currentTime <- getCurrentTime
      let snake' = update $ hasnake game

      -- update game
      g $= (checkGame game) {
        defaultSpeed = 0.2,
        lastUpdateTime = currentTime,
        hasnake = snake'
      }
    False -> return ()
  postRedisplay Nothing

-- Helper function decide whether we update.
shouldUpdate :: Game -> IO Bool
shouldUpdate g = do
  currentTime <- getCurrentTime
  let timeDiff = diffUTCTime currentTime $ lastUpdateTime g
  return $ timeDiff > defaultSpeed g
