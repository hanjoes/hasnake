module Game where

import Graphics.UI.GLUT
import Data.Time.Clock
import Data.IORef

import Bean
import Snake

-- This should be the only global state in the game.

data Game = Game {
  defaultSpeed :: NominalDiffTime,
  lastUpdateTime :: UTCTime,
  hasnake :: Snake,
  currentBean :: Bean, -- there is always a bean
  gameWindowSize :: GLsizei,
  gridSize :: GLsizei
}

checkGame :: Game -> Game
checkGame game
  | shouldBeDead game snake = game { hasnake = snake { isAlive = False } }
  | headOnBean game snake = game { hasnake = hasnakeGrow snake, currentBean = bean { eaten = True } }
  | otherwise = game { hasnake = snake }
  where snake = hasnake game
        bean = currentBean game

-- Minimum row is always 0
minRow :: Game -> GLfloat
minRow _ = 0

-- Minimum col is always 0
minCol :: Game -> GLfloat
minCol _ = 0

-- Maximum row
maxRow :: Game -> GLfloat
maxRow g = (numGrids g) - 1

-- Maximum col
maxCol :: Game -> GLfloat
maxCol g = (numGrids g) - 1

-- Helper function to get the number of grids of the square
numGrids :: Game -> GLfloat
numGrids game = ws / gs
  where ws :: GLfloat
        ws = fromIntegral $ gameWindowSize game
        gs :: GLfloat
        gs = fromIntegral $ gridSize game

-- Helper function to get the scale of the small grids
gridScale :: Game -> GLfloat
gridScale game = 1 / numGrids game

-- helper function to check whether snake's head is on the bean
headOnBean :: Game -> Snake -> Bool
headOnBean game snake = (snakeHead snake) == (beanLocation $ currentBean game)

-- helper function to check whether snake is still in border or is not on itself
shouldBeDead:: Game -> Snake -> Bool
shouldBeDead game snake = case snakeHead snake of
  (row, col) -> or [row > maxRow game,
                     row < minRow game,
                     col > maxCol game,
                     col < minCol game, (row, col) `elem` (bodyWithoutHead snake)]
