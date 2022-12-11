module Display (renderEpisode) where

import qualified System.Random as Rand

import Graphics.Gloss

degreesRadians radians = radians * (180/pi);


window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

-- drawing :: Picture
-- drawing = circle 80

drawing :: Picture
drawing = pictures
  [ translate (-20) (-100) $ color ballColor $ circleSolid 30 
  , translate 30 50 $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)


renderEpisode = display window background drawing
