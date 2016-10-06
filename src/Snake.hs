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
hasnakeGrow s = s { body = growBody $ body s }

-- Hellper function to append to tail
growBody:: [HasnakePos] -> [HasnakePos]
growBody (p:p':[]) = p:p':[(shiftPos growDir p')]
  where growDir = reverseDir $ lookforDir p [p']
growBody (p:ps) = p:(growBody ps)
growBody p = p

-- function to shift a hasnake pos
shiftPos :: HasnakeDir -> HasnakePos -> HasnakePos
shiftPos HSUp p = shiftUp p
shiftPos HSDown p = shiftDown p
shiftPos HSLeft p = shiftLeft p
shiftPos HSRight p = shiftRight p

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
  where updatedHead = shiftPos dir c

-- Helper function to look for direction given a head and the rest of the body
lookforDir :: HasnakePos -> [HasnakePos] -> HasnakeDir
lookforDir p ps
  | shiftLeft p `elem` ps  = HSRight
  | shiftRight p `elem` ps  = HSLeft
  | shiftUp p `elem` ps  = HSDown
  | otherwise = HSUp


-- Helper function to get snake head
snakeHead :: Snake -> HasnakePos
snakeHead snake = case body snake of p:ps -> p
