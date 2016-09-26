module Grid where

import Graphics.UI.GLUT
import Data.IORef

import Snake
import Utils
import Bean
import Game

-- We only need row, column, size to define a grid.
-- Note: the row and column are all starting from 0.
data Grid = Grid {
  r :: GLfloat,
  c :: GLfloat,
  s :: GLfloat,
  gridColor :: Color3 GLfloat
}

-- Padding scale.
ps :: GLfloat
ps = 8.0

-- Grid color
presetColor :: Color3 GLfloat
presetColor = Color3 0.97 0.89 0.72

-- Border of the grid.
left :: Grid -> GLfloat
left g = -1 + 2 * ((c g) * (s g) + (s g) / ps)

right :: Grid -> GLfloat
right g = -1 + 2 * ((c g + 1) * (s g) - (s g) / ps)

top :: Grid -> GLfloat
top g = 1 - 2 * ((r g) * (s g) + (s g) / ps)

bottom :: Grid -> GLfloat
bottom g = 1 - 2 * ((r g + 1) * (s g) - (s g) / ps)

-- GLUT uses some kind of unit coordinate system. Weird!
renderGrid :: Grid -> IO ()
renderGrid g  = do
  renderPrimitive Quads $ do
  color $ gridColor g
  vertex $ Vertex2 (left g) (top g)
  vertex $ Vertex2 (right g) (top g)
  vertex $ Vertex2 (right g) (bottom g)
  vertex $ Vertex2 (left g) (bottom g)

-- Render all grids given window size and grid scale.
renderGame :: GLfloat -> GLfloat -> IORef Game -> IO ()
renderGame ws s game
  | ws <= 0 = return ()
  | otherwise = do
      game' <- get game
      mapM_ renderGrid $ genGrids ws s ng game'
  where ng = ws / s
        scale = s / ws

-- Helper function to get all grids to render
genGrids :: GLfloat -> GLfloat -> GLfloat -> Game -> [Grid]
genGrids ws s numGrids game = do
  let snake = hasnake game
  let bean = currentBean game
  row <- [0..numGrids - 1]
  col <- [0..numGrids - 1]
  return Grid {
    r = row,
    c = col,
    s = scale,
    gridColor = pickColor (row, col) snake bean
  }
  where ng = ws / s
        scale = s / ws

-- Pick color for the snake body.
pickColor :: HasnakePos -> Snake -> Bean -> Color3 GLfloat
pickColor p s b
  | p `elem` (body s) = bodyColor s
  | p == (beanLocation b) = beanColor b
  | otherwise = presetColor
