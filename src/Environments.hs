
-- module Environments (CliffWalkingEnv(..), makeCliffWalkingEnv, Observation, Action) where
-- module Environments (Observation, Action, resetEnv, gridH, gridW, Action) where
module Environments where


import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Array as A
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified System.Random as Rand

data Cell = Start | Goal | Free | Cliff
  deriving (Show, Eq)

isSafe :: Cell -> Bool
isSafe Cliff = False
isSafe _ = True

cellToChar :: Cell -> Char
cellToChar Start = 'S'
cellToChar Goal = 'G'
cellToChar Free = 'F'
cellToChar Cliff = 'H'

charToCell :: Char -> Cell
charToCell 'S' = Start
charToCell 'G' = Goal
charToCell 'F' = Free
charToCell 'C' = Cliff

type Observation = Int

data Action =
  MoveLeft |
  MoveDown |
  MoveRight |
  MoveUp
    deriving (Show, Eq, Enum)

gridH = 4
gridW = 12
startState = convertPosTo1D (0, 3)
goalState = convertPosTo1D (11, 3)
cliffStates = [(x,3) | x <- [1..10]]


-- step in th
stepXY :: Action -> Int -> Int -> (Int, Int)
stepXY MoveLeft x y 
  | 0 < x     = (x-1, y)
  | otherwise = (x, y)
stepXY MoveDown x y
  | y < gridH = (x, y+1)
  | otherwise = (x, y)
stepXY MoveRight x y
  | x < gridW = (x+1, y)
  | otherwise = (x, y)
stepXY MoveUp x y
  | 0 < y     =  (x ,y-1)
  | otherwise = (x, y)

step1D :: Action -> Int -> Int
step1D a pos1D =
  convertPosTo1D $ stepXY a x y
  where 
    x = getXFromPos pos1D
    y = getYFromPos pos1D

getXFromPos :: Int -> Int
getXFromPos l = l `rem` gridH

getYFromPos :: Int -> Int
getYFromPos l = l `quot` gridW

convertPosTo2D :: Int -> (Int, Int)
convertPosTo2D l = reverseTuple $ quotRem l gridW
  -- (l `mod` gridW) (l )

reverseTuple :: (a,b) -> (b,a)
reverseTuple (a,b) = (b,a)

convertPosTo1D :: (Int, Int) -> Int
convertPosTo1D (x, y) =  
  y * gridW + x

-- perform an action on the current observed state and renurn the next state, reward, and termination status
stepEnv :: Action -> Observation -> (Observation, Double, Bool)
stepEnv act pos
  | isGoal nextPos    = (nextPos, 0, True)
  | isCliff nextPos   = (nextPos, -100, False)
  | otherwise         = (nextPos, -1, False)
  where nextPos = step1D act pos

resetEnv :: (Observation, Int, Bool)
resetEnv = (startState, 0, False)

isGoal :: Observation -> Bool
isGoal pos1d = pos1d == goalState

isCliff :: Observation -> Bool
isCliff pos1d = convertPosTo2D pos1d `elem` cliffStates
