-- module Snake (Snake (body, dir, clr), Direction, SCoord) where
module Snake (Snake (..), SCoord, Direction (..), update, turnSnake) where

import Graphics.UI.GLUT

type SCoord = (GLfloat, GLfloat)

data Direction = Up | Down | Left | Right

data Snake = Snake { body :: [SCoord], dir :: Direction, bodyColor :: Color3 GLfloat }

-- After eaten a bean, update the snake
update :: Snake -> Snake
update s = s { body = updateBody (body s) (dir s) }

-- functions used to turn the snake
turnSnake :: Direction -> Snake -> Snake
turnSnake dir s = s { dir = dir }

-- Helper functions to update coordinations.
shiftLeft :: SCoord -> SCoord
shiftLeft (r, c) = (r, c - 1)

shiftRight :: SCoord -> SCoord
shiftRight (r, c) = (r, c + 1)

shiftUp :: SCoord -> SCoord
shiftUp (r, c) = (r - 1, c)

shiftDown :: SCoord -> SCoord
shiftDown (r, c) = (r + 1, c)

-- Helper function to update the whole body.
updateBody :: [SCoord] -> Direction -> [SCoord]
updateBody [] _ = []
updateBody (c:cs) dir = updatedHead:(updateBody cs $ lookforDir c cs)
  where updatedHead = case dir of
          Snake.Up -> shiftUp c
          Snake.Down -> shiftDown c
          Snake.Left -> shiftLeft c
          Snake.Right -> shiftRight c
        lookforDir c' cs'
          | shiftLeft c' `elem` cs'  = Snake.Right
          | shiftRight c' `elem` cs'  = Snake.Left
          | shiftUp c' `elem` cs'  = Snake.Down
          | otherwise = Snake.Up
