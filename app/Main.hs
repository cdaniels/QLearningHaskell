
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Display (renderEpisode)
import qualified Plotting (plotData)
import Environments
import Agents
-- import qualified Simulation (performEpisodes)

numEpisodes = 100

main :: IO ()
main = do
  -- specify environment paramaters
  let agentParams = ("", 50)
  -- create the enviornment with specified parameters
  let env = makeCliffWalkingEnv
  let agent = makeQLearningAgent env agentParams
  rewards <- performEpisodes numEpisodes env
  -- train the agent by through episodes of running the environment
  -- let results = Simulation.performEpisodes env agent
  -- plot the learning progress
  Plotting.plotData numEpisodes rewards
  -- render an episode with the agents learned policy
  Display.renderEpisode env agent
