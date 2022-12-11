
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import qualified Display (renderEpisode)
import qualified Plotting (plotData)

import Environments
import Agents

numRuns = 1
numEpisodes = 100

main :: IO ()
main = do
  -- train the agent by through episodes of running the environment
  -- rewards <- performEpisodes numEpisodes
  rewards <- performRuns numRuns numEpisodes
  -- print the results to the screen
  putStrLn $ "finished performing Episodes!! rewarData is:" ++ show rewards
  -- plot the learning progress
  Plotting.plotData numEpisodes numRuns rewards
  -- render an episode with the agents learned policy
  Display.renderEpisode
