-- module Agents (QLearningAgent(..), makeQLearningAgent) where
module Agents (performEpisodes) where

import Data.Ord
import Data.List
import Data.IntCast
import qualified System.Random as Rand

import Environments


alpha = 0.1
epsilon = 0.1
gamma = 1.0

type QTable = [[Double]]

num_states = gridH*gridW
num_actions = 4

-- initialize an array of size S*A
initQTable :: [[Double]]
initQTable = [[0.0 | i <- [1..num_actions]] | j <- [1..num_states]]


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


getMaxActionFromQTable :: Int -> QTable -> IO Action
getMaxActionFromQTable state qTable = do
  putStrLn "getting max action from QTable for state:"
  -- TODO STATE TOO HIGH HERE
  putStrLn $ (show state)
  index <- getMaxActIndexFromQTable state qTable
  return $ getActionFromIndex index


getMaxActIndexFromQTable :: Int -> QTable -> IO Int
getMaxActIndexFromQTable state qTable = randomArgMax $ getActValuesForState state qTable

getMaxActValFromQTable :: Int -> QTable -> Double
getMaxActValFromQTable state qTable = maximum $ getActValuesForState state qTable

argMax :: Ord a => [a] -> Int
argMax = fst . maximumBy (comparing snd) . zip [0..]

randomArgMax :: (Ord a, Num a, Enum a) => [a] -> IO Int
randomArgMax xs = do
  let maxIndices = getMaxIndices xs
  let l = length maxIndices
  randIndex <- Rand.randomRIO (0, l-1)
  putStrLn "getingRandomArgMax!"
  putStrLn $ show maxIndices
  putStrLn "accessing index 1:!"
  putStrLn $ show randIndex
  return $ maxIndices !! randIndex

getMaxVals :: Ord a => [a] -> [a]
getMaxVals xs = filter (\x -> x == maximum xs) xs

getMaxIndices :: (Ord a, Num b, Enum b) => [a] -> [b]
getMaxIndices xs = map fst $ filter (\t -> snd t == maximum xs) $ zip [0..] xs


-- returns the list of action values for a given state from the qTable
getActValuesForState :: Int -> QTable -> [Double]
getActValuesForState state qTable = qTable !! state


getActionFromIndex :: Int -> Action
getActionFromIndex index 
  | index == 0 = MoveLeft 
  | index == 1 = MoveRight
  | index == 2 = MoveUp
  | index == 3 = MoveDown
  | otherwise = MoveUp -- should not happen


getIndexFromAction :: Action -> Int
getIndexFromAction act 
  | act == MoveLeft = 0
  | act == MoveRight= 1
  | act == MoveUp   = 2
  | act == MoveDown = 3
  | otherwise = 0 -- should not happen


getRandomAction :: IO Action
getRandomAction = do
  randIndex <- Rand.randomRIO (0, 3)
  putStrLn "Random Index is"
  putStrLn $ show randIndex
  let actIndex = getActionFromIndex randIndex
  return actIndex


-- takes a number of episodes returns
-- performEpisodes :: Int -> IO [Int]
performEpisodes numEpisodes = do
  let q = initQTable
  let rewardData = []
  -- (q', rewards) <- performEpisode q
  rewardData' <- iterateEpisodes q 0 numEpisodes rewardData
  -- let rewards = [1..100]
  return rewardData'


iterateEpisodes :: (Ord t, Num t) => QTable -> t -> t -> [Double] -> IO [Double]
iterateEpisodes qTable episodeCount maxEpisodes rewardData = do
  (q', rewards) <- performEpisode qTable
  -- increment episode
  let episodeCount' = episodeCount + 1 
  let rewardData' = rewardData ++ [rewards]
  -- if done then return, else recur
  if episodeCount' == maxEpisodes
    then return rewardData'
    else iterateEpisodes q' episodeCount' maxEpisodes rewardData'

-- returns the sum of rewards for that epiisode
-- performEpisode :: QTable -> IO (QTable, Double)
performEpisode qTable = do
  let (state, reward, done) = resetEnv
  putStrLn "performingEpisode with init state:"
  putStrLn $ show state
  let rewardSum = 0.0
  let stepCount = 0
  (q', rewardSum) <- performStep qTable state rewardSum stepCount done
  return (q', rewardSum)


performStep :: QTable -> Observation -> Double -> Int -> Bool -> IO (QTable, Double)
performStep qTable state rewardSum stepCount done = do
  putStrLn "about to choose action!"
  action <- epsilonGreedyPolicy state qTable
  -- action <- getRandomAction
  putStrLn "perfomingStep!"
  putStrLn $ show state
  putStrLn $ show action
  let (state', reward, done) = stepEnv action state
  -- let (state', reward, done) = (state, 0.0, False)
  -- update data
  let rewardSum' = rewardSum + reward
  let stepCount' = stepCount + 1
  putStrLn "new state is:!"
  putStrLn $ show state'
  q' <- updateQTable qTable state action reward state'
  -- if done then return, else recur
  if done 
    then return (qTable, rewardSum)
    else performStep q' state' rewardSum' stepCount' done


-- takes the old q table and updates it with new state info
-- takes old_q_table, old_state, action, reward, new_state
-- return new_q_table
-- (in Python)   
-- self.Q[run, s, a] += alpha* (r + self.gamma * np.max(self.Q[run, next_state, :]) - self.Q[run, s, a])
updateQTable :: QTable -> Observation -> Action -> Double -> Observation -> IO QTable
updateQTable q s a r s' = do
  let oldQ = q
  let aIndex = getIndexFromAction a

  putStrLn "updatingQTable!"
  putStrLn $ show q
  putStrLn "accessing index 0:!"
  putStrLn $ show s
  putStrLn "accessing index 1:!"
  putStrLn $ show aIndex
  let pred = (q !! s) !! aIndex
  -- let pred = 0.0
  let maxQActVal = getMaxActValFromQTable s q
  let target = r + gamma * maxQActVal

  -- updat the q table
  let q' = updateTable q (pred + alpha * (target - pred)) (s, aIndex)
  return q'

-- takes tabl, row, col, returns updated table
updateTable :: [[a]] -> a -> (Int, Int) -> [[a]]
updateTable m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m






































