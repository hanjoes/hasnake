module Game where

import Graphics.UI.GLUT
import Data.Time.Clock
import Data.IORef

import Bean
import Snake

-- This should be the only global state in the game.

data Game = Game {
  defaultSpeed :: NominalDiffTime,
  lastUpdateTime :: UTCTime,
  hasnake :: Snake,
  currentBean :: Bean -- there is always a bean
}
