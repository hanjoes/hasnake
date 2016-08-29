module Grid where

import Graphics.UI.GLUT

-- We only need row, column, size to define a grid.
-- Note: the row and column are all starting from 0.
data Grid = Grid { r :: Int, c :: Int, s :: GLsizei }

left :: Grid -> GLfloat
left g = (fromIntegral $ c g) * (fromIntegral $ s g)

right :: Grid -> GLfloat
right g = (fromIntegral $ c g + 1) * (fromIntegral $ s g)

top :: Grid -> GLfloat
top g = (fromIntegral $ r g) * (fromIntegral $ s g)

bottom :: Grid -> GLfloat
bottom g = (fromIntegral $ r g + 1) * (fromIntegral $ s g)

renderGrid :: Grid -> IO ()
renderGrid g = renderPrimitive Quads $ do
  color $ Color3 0 1 (0 :: GLfloat)
  vertex $ Vertex2 (-1) (1 :: GLfloat)
  vertex $ Vertex2 (-0.95) (1 :: GLfloat)
  vertex $ Vertex2 (-0.95) (0.95 :: GLfloat)
  vertex $ Vertex2 (-1) (0.95 :: GLfloat)

renderGrids :: Int -> GLsizei -> IO ()
renderGrids 0 _ = return ()
renderGrids ng s = do
  sequence_ [renderGrid $ Grid { r = r, c = c, s = s } | r <- [0..ng-1], c <- [0..ng-1]]

