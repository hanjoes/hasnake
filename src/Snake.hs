-- module Snake (Snake (body, dir, clr), Direction, HasnakePos) where
module Snake (Snake (..), update, turnSnake, hasnakeGrow, hasnakeDie, snakeHead, initializeSnake) where

import Graphics.UI.GLUT

import Utils

data Snake = Snake { body :: [HasnakePos],
                     dir :: HasnakeDir,
                     bodyColor :: Color3 GLfloat,
                     isAlive :: Bool
                   }

initializeSnake :: GLfloat -> Snake
initializeSnake ng = Snake {
    body = [(ng / 2, ng / 2), (ng / 2, ng / 2 + 1)],
    dir = HSRight,
    bodyColor = Color3 1 0 (0 :: GLfloat),
    isAlive = True
  }

-- update the snake by direction
update :: Snake -> Snake
update s = s { body = updateBody (body s) (dir s) }

hasnakeDie :: GLfloat -> Snake
hasnakeDie ng = initializeSnake ng

hasnakeGrow :: Snake -> Snake
hasnakeGrow s = undefined

-- functions used to turn the snake
turnSnake :: HasnakeDir -> Snake -> Snake
turnSnake dir s = s { dir = dir }

-- Helper functions to update coordinations.
shiftLeft :: HasnakePos -> HasnakePos
shiftLeft (r, c) = (r, c - 1)

shiftRight :: HasnakePos -> HasnakePos
shiftRight (r, c) = (r, c + 1)

shiftUp :: HasnakePos -> HasnakePos
shiftUp (r, c) = (r - 1, c)

shiftDown :: HasnakePos -> HasnakePos
shiftDown (r, c) = (r + 1, c)

-- helper function to update the whole body.
updateBody :: [HasnakePos] -> HasnakeDir -> [HasnakePos]
updateBody [] _ = []
updateBody (c:cs) dir = updatedHead:(updateBody cs $ lookforDir c cs)
  where updatedHead = case dir of
          HSUp -> shiftUp c
          HSDown -> shiftDown c
          HSLeft -> shiftLeft c
          HSRight -> shiftRight c
        lookforDir c' cs'
          | shiftLeft c' `elem` cs'  = HSRight
          | shiftRight c' `elem` cs'  = HSLeft
          | shiftUp c' `elem` cs'  = HSDown
          | otherwise = HSUp

-- Helper function to get snake head
snakeHead :: Snake -> HasnakePos
snakeHead snake = case body snake of p:ps -> p
