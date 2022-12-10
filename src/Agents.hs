module Agents (QLearningAgent(..), makeQLearningAgent) where

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Array as A
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified System.Random as Rand

import Environments

data QLearningAgent = QLearningAgent
  { randomGenerator :: Rand.StdGen,
    qTable :: A.Array (Word, Word) Double,
    explorationRate :: Double
  }


-- makeQLearningAgent env params = "TODO"
makeQLearningAgent :: IO CliffWalkingEnv -> a -> IO QLearningAgent
makeQLearningAgent env params = do
  gen <- Rand.getStdGen
  return $ QLearningAgent {
    randomGenerator = gen,
    qTable = A.listArray ((0, 0), (15, 3)) (repeat 0.0),
    explorationRate = 0.9
  }





learnQTable :: (MonadState QLearningAgent m) =>
  Observation -> Observation -> Int -> Environments.Action -> m ()
learnQTable obs1 obs2 reward action = do
  agent <- get
  let q = qTable agent
      actionIndex = fromIntegral . fromEnum $ action
      prediction = q A.! (obs1, actionIndex)
      target = reward + gamma * (fst $ maxScore obs2 q)
      newValue = prediction + learningRate * (target - prediction)
      newQ = q A.// [((obs1, actionIndex), newValue)]
  put $ agent { qTable = newQ }
  where
    gamma = 0.96
    learningRate = 0.81


-- chooseActionUser :: (MonadIO m) => m Action
-- chooseActionUser = (toEnum . read) <$> (liftIO getLine)

-- chooseActionRandom :: (MonadIO m) => m Action
-- chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 3))

-- chooseActionQTable :: (MonadState QLearningAgent m) => m Action
-- chooseActionQTable = do
--   agent <- get
--   let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator agent)
--   if exploreRoll < explorationRate agent
--     then do
--       let (actionRoll, gen'') = Rand.randomR (0, 3) gen'
--       put $ agent { randomGenerator = gen'' }
--       return (toEnum actionRoll)
--     else do
--       let maxIndex = snd $ snd $ maxScore (agentPos agent) (qTable agent)
--       put $ agent { randomGenerator = gen' }
--       return (toEnum (fromIntegral maxIndex))



 -- maxScore :: Observation -> A.Array (Int, Int) Int -> (Int, (Int, Int))
 -- maxScore obs table = maximum valuesAndIndices
 --  where 
 --     indices = (obs, ) <$> [0..3]
 --     valuesAndIndices = (\i -> (table A.! i, i)) <$> indices



-- performEpisodes numEpisodes env = do
--   let rewards = [x**5 | x <- [1..numEpisodes]] 
--   return rewards
-- performEpisodes :: IO -> IO QLearningAgent -> IO ()
performEpisodes :: Int -> IO QLearningAgent -> IO ()
performEpisodes env agent = do
  void $ execStateT finalAction env
  where
    numEpisodes = 10000
    decayRate = 0.9
    minEpsilon = 0.01

    finalAction :: StateT CliffWalkingEnv IO ()
    finalAction = do
      rewards <- forM [1..numEpisodes] $ \i -> do
        Environments.resetEnv
        when (i `mod` 100 == 99) $ do
          env <- get
          let e = explorationRate agent
          let newE = max minEpsilon (e * decayRate)
          put $ env { explorationRate = newE }
        (_, reward) <- gameLoop chooseActionQTable
        return reward
      lift $ print (sum rewards)



gameLoop :: (MonadIO m) =>
  StateT CliffWalkingEnv m Action ->
  StateT CliffWalkingEnv m (Observation, Double)

gameLoop chooseAction = do
  oldObs <- agentPos <$> get
  newAction <- chooseAction
  (newObs, reward, done) <- Environments.stepAgent newAction
  learnQTable oldObs newObs reward newAction
  if done
    then do
      if reward > 0.0 
        then liftIO $ putStrLn "Win"
        else liftIO $ putStrLn "Lose"
      return (newObs, reward)
    else gameLoop chooseAction







-- QTable ::  2D array


-- 
-- learnQTable :: QTable 






































