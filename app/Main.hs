
{-# LANGUAGE ExtendedDefaultRules #-}

module Main where
import qualified Plotting (plotData)
import Agents ( performRuns )

-- parameters to specify the length of the simulation
numRuns :: Int
numRuns = 5
numEpisodes :: Int
numEpisodes = 100

main :: IO [Char]
main = do
  -- train the agent by through episodes of running the environment
  rewards <- performRuns numRuns numEpisodes
  -- print the results to the screen
  putStrLn $ "Finished performing Episodes!! rewardData is: " ++ show rewards
  -- plot the learning progress
  Plotting.plotData numEpisodes numRuns rewards
  -- prompt the user for input to stop the program (necessary to ensure that plot displays before exit)
  putStrLn "All done, press enter to close"
  getLine
