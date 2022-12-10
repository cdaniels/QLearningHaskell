module Simulation (performEpisodes) where

import Agents
import Environments

import Control.Monad (forM_)
import Control.Monad.State
import qualified Data.Array as A
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes)
import qualified System.Random as Rand


-- someFunc :: IO ()
-- performEpisodes env agent = "TODO" --

-- do
  -- agent.step
  -- gen <- Rand.getStdGen
  -- return $ CliffWalkingEnv { 
  --     currentObservation = 0, 
  --     grid = A.listArray (0, 15) (charToCell <$> "SFFFFCFCFFFCCFFG"),
  --     previousAction = Nothing,
  --     dimens = (4, 4)
  --   }



-- playGame :: IO ()
-- playGame = do
performEpisodes env agent = ""
-- performEpisodes :: IO CliffWalkingEnv -> IO QLearningAgent -> IO ()
-- performEpisodes env agent = do
--   void $ execStateT finalAction env
--   where
--     numEpisodes = 10000
--     decayRate = 0.9
--     minEpsilon = 0.01

--     finalAction :: StateT CliffWalkingEnv IO ()
--     finalAction = do
--       rewards <- forM [1..numEpisodes] $ \i -> do
--         Environments.resetEnv
--         when (i `mod` 100 == 99) $ do
--           env <- get
--           let e = explorationRate agent
--           let newE = max minEpsilon (e * decayRate)
--           put $ env { explorationRate = newE }
--         (_, reward) <- gameLoop chooseActionQTable
--         return reward
--       lift $ print (sum rewards)



-- gameLoop :: (MonadIO m) =>
--   StateT CliffWalkingEnv m Action ->
--   StateT CliffWalkingEnv m (Observation, Double)

-- gameLoop chooseAction = do
--   oldObs <- agentPos <$> get
--   newAction <- chooseAction
--   (newObs, reward, done) <- Environments.stepAgent newAction
--   Agents.learnQTable oldObs newObs reward newAction
--   if done
--     then do
--       if reward > 0.0 
--         then liftIO $ putStrLn "Win"
--         else liftIO $ putStrLn "Lose"
--       return (newObs, reward)
--     else gameLoop chooseAction
