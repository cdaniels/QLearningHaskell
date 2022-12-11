
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Display (renderEpisode)
import qualified Plotting (plotData)
import Environments
import Agents
-- import qualified Simulation (performEpisodes)

numEpisodes = 500

main :: IO ()
main = do
  -- specify environment paramaters
  let agentParams = ("", 50)
  let env = ("", 50)
  -- create the enviornment with specified parameters
  -- let env = makeCliffWalkingEnv
  -- let agent = makeQLearningAgent env agentParams
  -- rewards <- performEpisodes numEpisodes env
  rewards <- performEpisodes numEpisodes
  putStrLn "finished performing Episodes!!"
  putStrLn "rewardData is:"
  putStrLn $ show rewards
  -- train the agent by through episodes of running the environment
  -- let results = Simulation.performEpisodes env agent
  -- plot the learning progress
  Plotting.plotData numEpisodes rewards
  -- render an episode with the agents learned policy
  Display.renderEpisode env ()
