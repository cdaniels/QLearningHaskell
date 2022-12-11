
-- module Environments (CliffWalkingEnv(..), makeCliffWalkingEnv, Observation, Action) where
-- module Environments (Observation, Action, resetEnv, gridH, gridW, Action) where
module Environments where
data Cell = Start | Goal | Free | Cliff
  deriving (Show, Eq)

-- isSafe :: Cell -> Bool
-- isSafe Cliff = False
-- isSafe _ = True

-- cellToChar :: Cell -> Char
-- cellToChar Start = 'S'
-- cellToChar Goal = 'G'
-- cellToChar Free = 'F'
-- cellToChar Cliff = 'H'

-- charToCell :: Char -> Cell
-- charToCell 'S' = Start
-- charToCell 'G' = Goal
-- charToCell 'F' = Free
-- charToCell 'C' = Cliff

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


-- perform a step in the enviorenment
-- takes an action, an x and y position
-- returns a new x, y position
stepXY :: Action -> Int -> Int -> (Int, Int)
stepXY MoveLeft x y 
  | 0 < x     = (x-1, y)
  | otherwise = (x, y)
stepXY MoveDown x y
  | y < gridH-1 = (x, y+1)
  | otherwise = (x, y)
stepXY MoveRight x y
  | x < gridW-1 = (x+1, y)
  | otherwise = (x, y)
stepXY MoveUp x y
  | 0 < y     =  (x , y-1)
  | otherwise = (x, y)

-- perform a step in the enviorenment 
-- using 1d locations
-- takes an action, a 1d position
-- returns a new 1d position
step1D :: Action -> Int -> Int
step1D a pos1D =
  convertPosTo1D $ stepXY a x y
  where (x, y) = convertPosTo2D pos1D

-- returns the x coord of a 1d position
getXFromPos :: Int -> Int
getXFromPos l = l `rem` gridH

-- returns the y coord of a 1d position
getYFromPos :: Int -> Int
getYFromPos l = l `quot` gridW

-- convert a 1d position to 2d x, y coords
convertPosTo2D :: Int -> (Int, Int)
convertPosTo2D l = reverseTuple $ quotRem l gridW

-- reverse a tuple's values
reverseTuple :: (a,b) -> (b,a)
reverseTuple (a,b) = (b,a)

-- convert a 2d position to a 1d position
convertPosTo1D :: (Int, Int) -> Int
convertPosTo1D (x, y) =  
  y * gridW + x

-- perform an action on the current observed state and renurn the next state, reward, and termination status
stepEnv :: Action -> Observation -> (Observation, Double, Bool)
stepEnv act pos
  -- | isGoal nextPos    = (nextPos, 100, True)
  | isGoal nextPos    = (nextPos, 0, True)
  | isCliff nextPos   = (nextPos, -100, False)
  | otherwise         = (nextPos, -1, False)
  where nextPos = step1D act pos

-- reset the environment
-- returns a tuple contaning the observation for the stard of an episode
resetEnv :: (Observation, Int, Bool)
resetEnv = (startState, 0, False)

-- check if a certain observed state is the goal state
isGoal :: Observation -> Bool
isGoal pos1d = pos1d == goalState

-- check if a certain observed state is a cliff
isCliff :: Observation -> Bool
isCliff pos1d = convertPosTo2D pos1d `elem` cliffStates
