module Utils where

import Graphics.UI.GLUT
import System.Random

type HasnakePos = (GLfloat, GLfloat)

data HasnakeDir = HSUp | HSDown | HSLeft | HSRight

getRandomHasnakePos :: HasnakePos -> IO HasnakePos
getRandomHasnakePos (rowNum, colNum) = do
  randomRow <- getStdRandom $ randomR (0, rowNum - 1)
  randomCol <- getStdRandom $ randomR (0, colNum - 1)
  return (fromIntegral $ truncate randomRow, fromIntegral $ truncate randomCol)

-- Get the bean location, it should not be on the snake's body.
getBeanLocation :: HasnakePos -> [HasnakePos] -> IO HasnakePos
getBeanLocation limits excluded = do
  pos <- getRandomHasnakePos limits
  case pos `elem` excluded of
    False -> return pos
    True -> getBeanLocation limits excluded

-- Get the reversed direction
reverseDir :: HasnakeDir -> HasnakeDir
reverseDir HSUp = HSDown
reverseDir HSDown = HSUp
reverseDir HSLeft = HSRight
reverseDir HSRight = HSLeft
