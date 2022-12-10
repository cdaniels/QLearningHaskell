module Plotting (plotData) where


import Graphics.Matplotlib

import Data.List
import Data.List.Split
import System.Random

uniforms :: (Random a, Num a) => [a]
uniforms = randoms (mkStdGen 42)

uniforms' lo hi = randomRs (lo,hi) (mkStdGen 42)

meventplot xs ys = plot xs ys
  % mp # "ax.add_collection(mcollections.EventCollection(data[0], linelength=0.05))"
  % mp # "ax.add_collection(mcollections.EventCollection(data[1], orientation='vertical', linelength=0.05))"
  % text "0.1" "0.6" "Ticks mark the actual data points"
  % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]
  -- where xs = [0..100]
  --       ys = map (\x -> (x ** 2)::Float) xs

plotData numEpisodes rewards = onscreen $ meventplot [1..numEpisodes] rewards
