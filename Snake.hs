module Snake (Snake (Snake), body, dir, clr,
              SCoord,
              Direction (Up, Down, Left, Right),
              update) where
-- module Snake (Snake (body, dir, clr), Direction, SCoord) where

import Graphics.UI.GLUT

type SCoord = (GLfloat, GLfloat)

data Direction = Up | Down | Left | Right

data Snake = Snake { body :: [SCoord], dir :: Direction, clr :: Color3 GLfloat }

-- Given a snake and a direction, returns the updated snake.
update :: Snake -> Direction -> Snake
update = undefined

