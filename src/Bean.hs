module Bean where

import Graphics.UI.GLUT
import Utils

data Bean = Bean { beanLocation :: HasnakePos, beanColor :: Color3 GLfloat, eaten :: Bool }