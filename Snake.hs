module Snake where

type Position = (GLfloat, GLfloat)

data Direction = Up | Down | Left | Right

data Snake = { len :: Int, pos :: Position }
