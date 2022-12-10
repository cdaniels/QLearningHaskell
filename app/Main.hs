
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Display (renderEpisode)
import qualified Plotting (plotData)
import Environments
import Agents
import qualified Simulation (performEpisodes)


main :: IO ()
main = do
  -- specify environment paramaters
  let envParams = ("", 50)
  let agentParams = ("", 50)
  -- create the enviornment with specified parameters
  let env = makeCliffWalkingEnv envParams
  let agent = makeQLearningAgent env agentParams
  -- train the agent by through episodes of running the environment
  let results = Simulation.performEpisodes env agent
  -- plot the learning progress
  Plotting.plotData results
  -- render an episode with the agents learned policy
  Display.renderEpisode env agent
