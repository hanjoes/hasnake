module Grid where

import Graphics.UI.GLUT
import Data.IORef

import qualified Snake as S

-- We only need row, column, size to define a grid.
-- Note: the row and column are all starting from 0.
data Grid = Grid { r :: GLfloat, c :: GLfloat, s :: GLfloat }

-- Padding scale.
ps :: GLfloat
ps = 8.0

-- Grid color
gridColor :: Color3 GLfloat
gridColor = Color3 0 0 1

-- Border of the grid.
left :: Grid -> GLfloat
left g = -1 + 2 * ((c g) * (s g) + (s g) / ps)

right :: Grid -> GLfloat
right g = -1 + 2 * ((c g + 1) * (s g) - (s g) / ps)

top :: Grid -> GLfloat
top g = 1 - 2 * ((r g) * (s g) + (s g) / ps)

bottom :: Grid -> GLfloat
bottom g = 1 - 2 * ((r g + 1) * (s g) - (s g) / ps)

-- Pick color for the snake body.
pickColor :: Grid -> S.Snake -> Color3 GLfloat
pickColor g s
  | gridCoord g `elem` (S.body s) = S.bodyColor s
  | otherwise = gridColor
  where gridCoord :: Grid -> S.SCoord
        gridCoord g = (r g, c g)

-- GLUT uses some kind of unit coordinate system. Weird!
renderGrid :: Grid -> S.Snake -> IO ()
renderGrid g s = do
  renderPrimitive Quads $ do
  color $ pickColor g s
  vertex $ Vertex2 (left g) (top g)
  vertex $ Vertex2 (right g) (top g)
  vertex $ Vertex2 (right g) (bottom g)
  vertex $ Vertex2 (left g) (bottom g)

-- Render all grids given window size and grid scale.
renderGrids :: GLfloat -> GLfloat -> IORef S.Snake -> IO ()
renderGrids ws s snake
  | ws <= 0 = return ()
  | otherwise = do
      snake' <- readIORef snake
      let body = S.body snake'
      sequence_ [renderGrid (Grid { r = r, c = c, s = scale }) snake' | r <- [0..ng-1], c <- [0..ng-1]]
  where ng = ws / s
        scale = s / ws

