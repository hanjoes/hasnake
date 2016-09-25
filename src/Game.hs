module Game (Game (..)) where

import Graphics.UI.GLUT
import Data.Time.Clock

data Game = Game {
  defaultSpeed :: NominalDiffTime,
  lastUpdateTime :: UTCTime
  }
