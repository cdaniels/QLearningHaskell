module Display (renderStep, createDisplayRenderer, DisplayCanvas) where

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

type DisplayCanvas = (Renderer, Texture)
xPadding = 100
yPadding :: Double
yPadding = 100

-- render a grid cell at a certain location with a certain color
drawCell x y color= do
  let cellSize = 50
  fill color
  stroke $ gray 255 !@ 128
  let x' = fromIntegral x
  let y' = fromIntegral y
  rect $ D (xPadding + x'*cellSize) (yPadding + y'*cellSize) cellSize cellSize -- corner1 x,y , corner2 x,y

drawAgentCell x y = do
  let color = red 255 !@ 128
  drawCell x y color

drawStartCell = do
  let (x, y) = convertPosTo2D startState
  let color = rgb 255 255 0 !@ 128
  drawCell x y color

drawGoalCell = do
  let (x, y) = convertPosTo2D goalState
  let color = green 255 !@ 128
  drawCell x y color

drawCells = do
  sequence [drawDefaultCell coord | coord <- gridCells]

drawCliffCells = do
  sequence [drawCliffCell coord | coord <- cliffStates]

drawCliffCell (x,y) = do
  let color = gray 0 !@ 128
  drawCell x y color

drawDefaultCell (x,y) = do
  let color = gray 200 !@ 128
  drawCell x y color

-- render the cells in the grid
drawGrid = do
  drawCells
  drawCliffCells
  drawStartCell
  drawGoalCell

-- creates a display object that holds the context which allows a canvas to be displayed
createDisplayRenderer :: IO (Renderer, Texture)
createDisplayRenderer = do
  initializeAll
  window <- createWindow (T.pack "CliffWalking Environment") defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  texture <- createCairoTexture' renderer window
  return (renderer, texture)

-- render the grid for a specific step
renderStep :: DisplayCanvas -> Int -> Int -> IO ()
renderStep (renderer, texture) step state = do
  putStrLn $ "Attempting to render Step for state : " ++ show state
  -- x, y position of agent
  let (x, y) = convertPosTo2D state
  -- render the grid and the agent at the given location
  withCairoTexture' texture $ runCanvas $ do
    background $ gray 102
    drawGrid
    drawAgentCell x y
  -- update the displayed texture
  copy renderer texture Nothing Nothing
  present renderer
  -- delay so that the screen is visible
  threadDelay 100000 -- microsecends
