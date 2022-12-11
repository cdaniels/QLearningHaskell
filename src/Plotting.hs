module Plotting (plotData) where

import Graphics.Matplotlib


buildTitle numRuns = "RL Performance for CliffWalking (averaged over " ++ (show numRuns) ++ " runs)"

-- line plot made through maptplotlib bindings
mrewardplot xs ys numRuns = plot xs ys @@ [o2 "label" ["Q-Learning"]]
  % (title $ buildTitle numRuns)
  % xlabel "Episodes"
  % ylabel "Reward Sum"
  % grid True
  % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]


plotData :: Int -> Int -> [Double] -> IO ()
plotData numEpisodes numRuns rewards = onscreen $ mrewardplot [1..numEpisodes] rewards numRuns
