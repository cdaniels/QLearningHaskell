module Display (renderStep) where

import qualified System.Random as Rand

-- import Graphics.Gloss

-- import Control.Concurrent

import Environments
import GHC.Conc.IO (threadDelay)
import Control.Concurrent (forkIO)
-- import qualified Data.Text.Lazy.Encoding as T
-- import qualified Data.Text.Lazy.IO as T
import qualified Data.Text as T


import SDL
import SDL.Cairo
import Linear.V2 (V2(..))
import Graphics.Rendering.Cairo.Canvas


xPadding = 100
yPadding :: Double
yPadding = 100

drawCell x y color= do
  let cellSize = 50
  fill color
  noStroke
  let x' = fromIntegral x
  let y' = fromIntegral y
  rect $ D (xPadding + x'*cellSize) (yPadding + y'*cellSize) cellSize cellSize -- corner1 x,y , corner2 x,y

drawAgentCell x y = do
  let color = red 255 !@ 128
  drawCell x y color

drawStartCell = do
  let (x, y) = convertPosTo2D startState
  let color = green 255 !@ 128
  drawCell x y color

drawGoalCell = do
  let (x, y) = convertPosTo2D goalState
  let color = green 255 !@ 128
  drawCell x y color

-- drawCliffCells = do
--   let color = blue 255 !@ 128
--   -- cells = [convertcliffStates
--   drawCell (fromIntegral x) (fromIntegral y) color

-- drawDefaultCells = do
--   let color = blue 255 !@ 128
--   drawCell (fromIntegral x) (fromIntegral y) color

drawGrid = do
  -- drawDefaultCells
  drawStartCell
  drawGoalCell

renderStep :: Int -> Int -> IO ()
renderStep step state = do
  -- r <- newIORef ""
  -- forkIO $ animate window background $ getFrame state
  initializeAll
  window <- createWindow (T.pack "cairo-canvas using SDL2") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createCairoTexture' renderer window

  -- x, y position of agent
  let (x, y) = convertPosTo2D state

  withCairoTexture' texture $ runCanvas $ do
    background $ gray 102
    drawGrid
    drawAgentCell x y
    -- stroke $ green 255 !@ 128
    -- fill $ blue 255 !@ 128
    -- rect $ D 250 250 100 100
    -- triangle (V2 400 300) (V2 350 400) (V2 400 400)

  

  copy renderer texture Nothing Nothing
  present renderer
  putStrLn $ "Attempting to render Step for state : " ++ show state
  threadDelay 100000 -- microsecends
