module Utils where

import Graphics.UI.GLUT
import System.Random

type HasnakePos = (GLfloat, GLfloat)

data HasnakeDir = HSUp | HSDown | HSLeft | HSRight

getRandomHasnakePos :: GLfloat -> IO HasnakePos
getRandomHasnakePos limit = do
  randomRow <- getStdRandom $ randomR (0, limit - 1)
  randomCol <- getStdRandom $ randomR (0, limit - 1)
  return (fromIntegral $ truncate randomRow, fromIntegral $ truncate randomCol)

-- Get the bean location, it should not be on the snake's body.
getBeanLocation :: GLfloat -> [HasnakePos] -> IO HasnakePos
getBeanLocation limit excluded = do
  pos <- getRandomHasnakePos limit
  case pos `elem` excluded of
    False -> return pos
    True -> getBeanLocation limit excluded

-- Get the reversed direction
reverseDir :: HasnakeDir -> HasnakeDir
reverseDir HSUp = HSDown
reverseDir HSDown = HSUp
reverseDir HSLeft = HSRight
reverseDir HSRight = HSLeft
