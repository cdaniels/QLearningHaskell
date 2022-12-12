module Agents where

import Data.Ord ( comparing )
import Data.List ( maximumBy, transpose )
import qualified System.Random as Rand

import Environments
    ( Action(..), Observation, gridH, gridW, stepEnv, resetEnv )
import Display (renderStep, createDisplayRenderer, DisplayCanvas)

-- tunable learning parameters
alpha :: Double
alpha = 0.1
epsilon :: Double
epsilon = 0.1
gamma :: Double
gamma = 1.0

-- constants for number of states and actions 
numStates :: Int
numStates = gridH*gridW
numActions :: Int
numActions = 4


-- table replesenting values for each action given a certain state
-- should have size numStates x numActions
type QTable = [[Double]]

-- initialize an array of size S*A
initQTable :: [[Double]]
initQTable = [[0.0 | i <- [1..numActions]] | j <- [1..numStates]]

-- Policy function
-- takes a state and Qtable 
-- return an action
epsilonGreedyPolicy :: Observation -> QTable -> IO Action
epsilonGreedyPolicy state qTable = do
  -- create a nuw pseudorandom generator
  -- gen <- Rand.newStdGen
  -- roll a random number between 0 and 1
  let minBound = 0.0::Double
  let maxBound = 1.0::Double
  -- let (roll, gen') = Rand.randomR (minBound, maxBound) gen
  roll <- Rand.randomRIO (minBound, maxBound)
  -- putStrLn $ show roll
  case compare roll epsilon of 
    LT -> do
      -- putStrLn "Less than Epsilon"
      getRandomAction
    GT -> do
      -- putStrLn "Greater than or Equal to Epsilon"
      getMaxActionFromQTable state qTable
    EQ -> do
      -- putStrLn "Greater than or Equal to Epsilon"
      getMaxActionFromQTable state qTable


-- get the maixmum valued action for a given state from the QTable
-- takes a state index and a QTable
-- returns the action with the highest value for that state
getMaxActionFromQTable :: Int -> QTable -> IO Action
getMaxActionFromQTable state qTable = do
  putStrLn $ "Getting max action from QTable for state: " ++ show state
  index <- getMaxActIndexFromQTable state qTable
  return $ getActionFromIndex index


-- get the index for the maixmum valued action for a given state from the QTable
-- takes a state index and a QTable
-- returns the index of the action with the highest value for that state
getMaxActIndexFromQTable :: Int -> QTable -> IO Int
getMaxActIndexFromQTable state qTable = randomArgMax $ getActValuesForState state qTable


-- get the maixmum value corresponding with an action for a given state from the QTable
-- takes a state index and a QTable
-- returns the highest value of an action for that state from the QTable
getMaxActValFromQTable :: Int -> QTable -> Double
getMaxActValFromQTable state qTable = maximum $ getActValuesForState state qTable

-- takes an array and returns the index of the element in that array that has the highest value
argMax :: Ord a => [a] -> Int
argMax = fst . maximumBy (comparing snd) . zip [0..]

-- takes an array and returns a random index from across all the maximum values in the array
randomArgMax :: (Ord a, Num a, Enum a) => [a] -> IO Int
randomArgMax xs = do
  let maxIndices = getMaxIndices xs
  let l = length maxIndices
  randIndex <- Rand.randomRIO (0, l-1)
  return $ maxIndices !! randIndex

-- takes an array and returns a sub array containing the maximum values
getMaxVals :: Ord a => [a] -> [a]
getMaxVals xs = filter (\x -> x == maximum xs) xs

-- takes an array and returns a sub array containing the itdices corresponding with the maximum values 
getMaxIndices :: (Ord a, Num b, Enum b) => [a] -> [b]
getMaxIndices xs = map fst $ filter (\t -> snd t == maximum xs) $ zip [0..] xs


-- returns the list of action values for a given state from the qTable
getActValuesForState :: Int -> QTable -> [Double]
getActValuesForState state qTable = qTable !! state


-- take an index and return the action which corresponds with that index
getActionFromIndex :: Int -> Action
getActionFromIndex index 
  | index == 0 = MoveLeft 
  | index == 1 = MoveRight
  | index == 2 = MoveUp
  | index == 3 = MoveDown
  | otherwise = MoveUp -- should not happen


-- takes an action and returns the index which corresponds with that action
getIndexFromAction :: Action -> Int
getIndexFromAction act 
  | act == MoveLeft = 0
  | act == MoveRight= 1
  | act == MoveUp   = 2
  | act == MoveDown = 3
  | otherwise = 0 -- should not happen

-- return a random action from all the possible actions
getRandomAction :: IO Action
getRandomAction = do
  randIndex <- Rand.randomRIO (0, 3)
  let actIndex = getActionFromIndex randIndex
  return actIndex


-- takes a number of runs and returns an array containing reward sum data
performRuns :: (Ord t, Num t) => t -> t -> IO [Double]
performRuns numRuns numEpisodes = do
  let runData = []
  runData' <- iterateRuns 0 numRuns numEpisodes runData
  putStrLn $ "finished performing runs with data: " ++ show (transpose runData')
  let avgData = averageOverRuns runData'
  return avgData


-- calculate the avereage rewards sum  of multiple runs
-- takes an array of reward data series for each run
-- returns an array of the average reward data over all the runs
averageOverRuns :: [[Double]] -> [Double]
averageOverRuns runData = [ calculateMean xs | xs <- transpose runData]


-- calculate the mean value of a given list
calculateMean :: (Fractional a, Foldable t) => t a -> a
calculateMean xs = sum xs / fromIntegral (length xs)

-- recursive function for iterating over runs while gathering data
-- takes the q table together with an run count which it increments and an array of runData which it appends to as it recurs
iterateRuns :: (Ord t, Num t) => t -> t -> t -> [[Double]] -> IO [[Double]]
iterateRuns runCount maxRuns numEpisodes runData = do
  -- get the resulting qTable and reward data from performing the specified number of episodes
  (q, rewards) <- performEpisodes numEpisodes
  -- increment run
  let runCount' = runCount + 1 
  let runData' = runData ++ [rewards]
  -- if done then return, else recur
  if runCount' == maxRuns
    then playLastEpisode q runData'
    else iterateRuns runCount' maxRuns numEpisodes runData'

playLastEpisode :: QTable -> [[Double]] -> IO [[Double]]
playLastEpisode qTable runData = do
  -- render an episode with the agents learned policy
  (q', reward') <- displayEpisode qTable
  -- return the resulting data for plotting purposes
  return runData

-- takes a number of episodes and returns an array containing reward sum data
performEpisodes :: (Ord t, Num t) => t -> IO (QTable, [Double])
performEpisodes numEpisodes = do
  -- initialize a qTable and an empty list for the data
  let q = initQTable
  let rewardData = []
  -- iterate over all the episodes, learning witht the qTable and filling the data list with results
  iterateEpisodes q 0 numEpisodes rewardData


-- recursive function for iterating over episodes while learning is performed
-- takes the q table together with an episode count which it increments and an array of rewardData which it appends to as it recurs
iterateEpisodes :: (Ord t, Num t) => QTable -> t -> t -> [Double] -> IO (QTable, [Double])
iterateEpisodes qTable episodeCount maxEpisodes rewardData = do
  (q', rewards) <- performEpisode qTable
  -- increment episode
  let episodeCount' = episodeCount + 1 
  let rewardData' = rewardData ++ [rewards]
  -- if done then return, else recur
  if episodeCount' == maxEpisodes
    then return (q', rewardData')
    else iterateEpisodes q' episodeCount' maxEpisodes rewardData'

-- perform an episode
-- takes the q table
-- returns the sum of rewards for that epsode
performEpisode :: QTable -> IO (QTable, Double)
performEpisode qTable = do
  let (state, reward, done) = resetEnv
  putStrLn $ "Performing episode with init state:" ++ show state
  let rewardSum = 0.0
  let stepCount = 0
  performStep qTable state rewardSum stepCount done

-- perform an episode while rendering its steps
-- takes the q table
-- returns the sum of rewards for that epsode
displayEpisode :: QTable -> IO (QTable, Double)
displayEpisode qTable = do
  let (state, reward, done) = resetEnv
  putStrLn $ "Performing episode with init state:" ++ show state
  let rewardSum = 0.0
  let stepCount = 0
  display <- createDisplayRenderer
  displayStep qTable state rewardSum stepCount done display

-- recursive function for performing steps of an episode
--   takes the q table together with 
--   an observation, 
--   cumulative reward sum, 
--   cumulative step count, 
--   and a flag signifying termination
performStep :: QTable -> Observation -> Double -> Int -> Bool -> IO (QTable, Double)
performStep qTable state rewardSum stepCount done = do
  action <- epsilonGreedyPolicy state qTable
  putStrLn $ "Choose action: " ++ show action
  let (state', reward, done) = stepEnv action state
  -- update data
  let rewardSum' = rewardSum + reward
  let stepCount' = stepCount + 1
  putStrLn $ "After action new state is: " ++ show state'
  q' <- updateQTable qTable state action reward state'
  if done 
    then return (qTable, rewardSum)
    else performStep q' state' rewardSum' stepCount' done


-- recursive function for performing steps of an episode  (while displaying)
--   takes the q table together with 
--   an observation, 
--   cumulative reward sum, 
--   cumulative step count, 
--   a flag signifying termination
--   and a DisplayCanvas type containing the canvas texture on which to render
displayStep :: QTable -> Observation -> Double -> Int -> Bool -> DisplayCanvas -> IO (QTable, Double)
displayStep qTable state rewardSum stepCount done display = do
  action <- epsilonGreedyPolicy state qTable
  putStrLn $ "Choose action: " ++ show action
  let (state', reward, done) = stepEnv action state
  -- update data
  let rewardSum' = rewardSum + reward
  let stepCount' = stepCount + 1
  putStrLn $ "After action new state is: " ++ show state'
  q' <- updateQTable qTable state action reward state'
  -- render the step if rendering is enabled
  Display.renderStep display stepCount state'
  -- if done then return, else recur 
  if done 
    then return (qTable, rewardSum)
    else displayStep q' state' rewardSum' stepCount' done display


-- takes the old q table and updates it with new state info
-- takes old_q_table, old_state, action, reward, new_state
-- return new_q_table
-- (in Python)   
-- self.Q[run, s, a] += alpha* (r + self.gamma * np.max(self.Q[run, next_state, :]) - self.Q[run, s, a])
updateQTable :: QTable -> Observation -> Action -> Double -> Observation -> IO QTable
updateQTable q s a r s' = do
  let oldQ = q
  let aIndex = getIndexFromAction a
  putStrLn $ "updatingQTable!" ++ show q

  let pred = (q !! s) !! aIndex
  -- let pred = 0.0
  let maxQActVal = getMaxActValFromQTable s q
  let target = r + gamma * maxQActVal

  -- updat the q table
  let q' = updateTable q (pred + alpha * (target - pred)) (s, aIndex)
  return q'


-- takes table, value, row, col, returns a table with the valu updated at the specified row, col location
-- goes through the table and replaces a value at a specific location
updateTable :: [[a]] -> a -> (Int, Int) -> [[a]]
updateTable m x (r,c) =
  take r m ++ -- earlier rows
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++ -- desired row with value replaced
  drop (r + 1) m -- remaining rows
