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


makeQLearningAgent env params = "TODO"
makeQLearningAgent :: IO CliffWalkingEnv -> a -> IO QLearningAgent
makeQLearningAgent env params = do
  gen <- Rand.getStdGen
  return $ QLearningAgent {
    randomGenerator = gen,
    qTable = A.listArray ((0, 0), (15, 3)) (repeat 0.0),
    explorationRate = 0.9
  }


-- learnQTable :: (MonadState QLearningAgent m) =>
--   Environments.Observation -> Environments.Observation -> Double -> Environments.Action -> m ()
-- learnQTable obs1 obs2 reward action = do
--   fle <- get
--   let q = qTable fle
--       actionIndex = fromIntegral . fromEnum $ action
--       prediction = q A.! (obs1, actionIndex)
--       target = reward + gamma * (fst $ maxScore obs2 q)
--       newValue = prediction + learningRate * (target - prediction)
--       newQ = q A.// [((obs1, actionIndex), newValue)]
--   put $ fle { qTable = newQ }
--   where
--     gamma = 0.96
--     learningRate = 0.81


-- chooseActionUser :: (MonadIO m) => m Action
-- chooseActionUser = (toEnum . read) <$> (liftIO getLine)

-- chooseActionRandom :: (MonadIO m) => m Action
-- chooseActionRandom = toEnum <$> liftIO (Rand.randomRIO (0, 3))

-- chooseActionQTable :: (MonadState FrozenLakeEnvironment m) => m Action
-- chooseActionQTable = do
--   fle <- get
--   let (exploreRoll, gen') = Rand.randomR (0.0, 1.0) (randomGenerator fle)
--   if exploreRoll < explorationRate fle
--     then do
--       let (actionRoll, gen'') = Rand.randomR (0, 3) gen'
--       put $ fle { randomGenerator = gen'' }
--       return (toEnum actionRoll)
--     else do
--       let maxIndex = snd $ snd $ maxScore (currentObservation fle) (qTable fle)
--       put $ fle { randomGenerator = gen' }
--       return (toEnum (fromIntegral maxIndex))




-- maxScore :: Observation -> A.Array (Word, Word) Double -> (Double, (Word, Word))
-- maxScore obs table = maximum valuesAndIndices
--   where
--     indices = (obs, ) <$> [0..3]
--     valuesAndIndices = (\i -> (table A.! i, i)) <$> indices
