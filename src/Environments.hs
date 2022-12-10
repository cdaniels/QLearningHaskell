module Environments (CliffWalkingEnv(..), makeCliffWalkingEnv, Observation, Action, resetEnv, stepAgent) where

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

tileToChar :: Cell -> Char
tileToChar Start = 'S'
tileToChar Goal = 'G'
tileToChar Free = 'F'
tileToChar Cliff = 'H'

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



data CliffWalkingEnv = CliffWalkingEnv { 
  agentPos :: Observation,
           grid :: A.Array Int Cell, 
           dims :: (Int, Int), 
           previousAction :: Maybe Action
}



-- take the rows, cols, start, goal, cliff_range
-- returns the array for the cliff walking env
-- buildGridCells :: Int -> Int -> Int -> Int -> [Int] -> [Cell]
-- buildGridCells rows cols start goal cliffCells =
--   (replicate [Free] ++ 
--   where 
  


makeCliffWalkingEnv :: IO CliffWalkingEnv
makeCliffWalkingEnv = do
  gen <- Rand.getStdGen
  return $ CliffWalkingEnv { 
    agentPos = 0, 
             grid = A.listArray (0, 4*12) (charToCell <$> "FFFFFFFFFFFF" ++ "FFFFFFFFFFFF" ++ "SCCCCCCCCCCG"),
             dims = (4, 12),
             previousAction = Nothing
  }


resetEnv :: (Monad m) => StateT CliffWalkingEnv m Observation
resetEnv = do
  let initialPos = 0
  env <- get
  put $ env { agentPos = initialPos, previousAction = Nothing }
  return initialPos


  -- returns Observation(int), Reward(int), Is_Terminated?(bool)
stepAgent :: (Monad m) => Action -> StateT CliffWalkingEnv m (Observation, Int, Bool)
stepAgent act = do
  env <- get
  let pos = agentPos env
  let allLegalMoves = inBounds pos (dims env)
  let newPos = if act `elem` allLegalMoves 
        then applyMove act pos (snd . dims $ env) 
        else pos
  let (done, reward) = case cell of
        Goal -> (True, 0) -- no reward for reaching the goal
        Cliff -> (True, -100) -- stepping off the cliff encurs -100
        _ -> (False, -1) -- each step incurs a negative reward
        where cell = grid env A.! newPos
  put $ env {
    agentPos = newPos,
    previousAction = Just act
  }
  return (newPos, reward, done)

-- stepAgent Action.MoveLeft pos1d
--   | 0 < pos = pos - 1
-- stepAgent Action.MoveDown
-- stepAgent Action.MoveRight = 
-- stepAgent Action.MoveUp = 
--   where x y = convertPosToXY pos1d

-- stepEnv :: (Monad m) => Action -> StateT FrozenLakeEnvironment m (Observation, Double, Bool)
-- stepEnv act = do
--   env <- get
--   let currentObs = currentObservation env
--   let (slipRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
--   let allLegalMoves = legalMoves currentObs (dimens fle)
--   let (randomMoveIndex, finalGen) = Rand.randomR (0, length allLegalMoves - 1) gen'
--   let newObservation = if slipRoll >= slipChance fle
--         then if act `elem` allLegalMoves
--           then applyMoveUnbounded act currentObs (snd . dimens $ fle)
--           else currentObs
--         else applyMoveUnbounded (allLegalMoves !! randomMoveIndex) currentObs (snd . dimens $ fle)
--   let (done, reward) = case (grid fle) A.! newObservation of
--         Goal -> (True, 1.0)
--         Hole -> (True, 0.0)
--         _ -> (False, 0.0)
--   put $ fle {currentObservation = newObservation, randomGenerator = finalGen, previousAction = Just act}
--   return (newObservation, reward, done)


applyMove :: Action -> Observation -> Int -> Observation
applyMove action pos numCols = case action of
    MoveLeft -> pos - 1 
    MoveDown -> pos + numCols
    MoveRight -> pos + 1
    MoveUp -> pos - numCols

inBounds :: Observation -> (Int, Int) -> [Action]
inBounds pos (numRows, numCols) = catMaybes [left, down, right, up]
  where
    (row, col) = quotRem pos numRows
    left = if col > 0 then Just MoveLeft else Nothing
    down = if row < numRows - 1 then Just MoveDown else Nothing
    right = if col < numCols - 1 then Just MoveRight else Nothing
    up = if row > 0 then Just MoveUp else Nothing
