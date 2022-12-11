
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import qualified Display (renderEpisode)
import qualified Plotting (plotData)
import Agents ( performRuns )

-- parameters to specify the length of the simulation
numRuns :: Int
numRuns = 5
numEpisodes :: Int
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
