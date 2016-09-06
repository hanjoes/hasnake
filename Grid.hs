module Grid where

import Graphics.UI.GLUT

-- We only need row, column, size to define a grid.
-- Note: the row and column are all starting from 0.
data Grid = Grid { r :: GLfloat, c :: GLfloat, s :: GLfloat }

-- Padding scale.
ps :: GLfloat
ps = 8.0

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
renderGrid g = do
  renderPrimitive Quads $ do
  color $ Color3 0 0 (1 :: GLfloat)
  vertex $ Vertex2 (left g) (top g)
  vertex $ Vertex2 (right g) (top g)
  vertex $ Vertex2 (right g) (bottom g)
  vertex $ Vertex2 (left g) (bottom g)

renderGrids :: GLfloat -> GLfloat -> IO ()
renderGrids ws s
  | ws <= 0 = return ()
  | otherwise = do
      sequence_ [renderGrid $ Grid { r = r, c = c, s = scale } | r <- [0..ng-1], c <- [0..ng-1]]
  where ng = ws / s
        scale = s / ws

