module Grid where

import Graphics.UI.GLUT

-- We only need row, column, size to define a grid.
-- Note: the row and column are all starting from 0.
data Grid = Grid { r :: GLfloat, c :: GLfloat, s :: GLfloat }

left :: Grid -> GLfloat
left g = (c g) * (s g)

right :: Grid -> GLfloat
right g = (c g + 1) * (s g)

top :: Grid -> GLfloat
top g = (r g) * (s g)

bottom :: Grid -> GLfloat
bottom g = (r g + 1) * (s g)

-- GLUT uses some kind of unit coordinate system. Weird!
renderGrid :: Grid -> IO ()
renderGrid g = renderPrimitive Quads $ do
  color $ Color3 0 0 (1 :: GLfloat)
  vertex $ Vertex2 (-1 + 2 * (left g)) (1 - 2 * (top g))
  vertex $ Vertex2 (-1 + 2 * (right g)) (1 - 2 * (top g))
  vertex $ Vertex2 (-1 + 2 * (right g)) (1 - 2 * (bottom g))
  vertex $ Vertex2 (-1 + 2 * (left g)) (1 - 2 * (bottom g))

renderGrids :: GLfloat -> GLfloat -> IO ()
renderGrids ws s
  | ws <= 0 = return ()
  | otherwise = do
      sequence_ [renderGrid $ Grid { r = r, c = c, s = scale } | r <- [0..ng-1], c <- [0..ng-1]]
  where ng = ws / s
        scale = s / ws

