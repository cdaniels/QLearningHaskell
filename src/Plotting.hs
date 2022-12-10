module Plotting (plotData) where


import Graphics.Matplotlib

import Data.List
import Data.List.Split
import System.Random

uniforms :: (Random a, Num a) => [a]
uniforms = randoms (mkStdGen 42)

uniforms' lo hi = randomRs (lo,hi) (mkStdGen 42)

-- * Generate normally distributed random values; taken from normaldistribution==1.1.0.3

-- -- | Box-Muller method for generating two normally distributed
-- -- independent random values from two uniformly distributed
-- -- independent random values.
-- boxMuller :: Floating a => a -> a -> (a,a)
-- boxMuller u1 u2 = (r * cos t, r * sin t) where r = sqrt (-2 * log u1)
--                                                t = 2 * pi * u2

-- -- | Convert a list of uniformly distributed random values into a
-- -- list of normally distributed random values. The Box-Muller
-- -- algorithms converts values two at a time, so if the input list
-- -- has an uneven number of element the last one will be discarded.
-- boxMullers :: Floating a => [a] -> [a]
-- boxMullers (u1:u2:us) = n1:n2:boxMullers us where (n1,n2) = boxMuller u1 u2
-- boxMullers _          = []



-- -- | Plural variant of 'normal', producing an infinite list of
-- -- random values instead of returning a new generator. This function
-- -- is analogous to 'Random.randoms'.
-- normals = boxMullers $ randoms (mkStdGen 42)

-- -- | Analogous to 'normals' but uses the supplied (mean, standard
-- -- deviation).
-- normals' (mean, sigma) g = map (\x -> x * sigma + mean) $ normals

-- mviolinplot = subplots @@ [o2 "ncols" 2, o2 "sharey" True]
--   % setSubplot "0"
--   % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals)
--   % setSubplot "1"
--   % violinplot (take 3 $ chunksOf 100 $ map (* 2) $ normals) @@ [o2 "showmeans" True, o2 "showmedians" True, o2 "vert" False]


-- mlineFunction = lineF (\x -> x**2) [0,0.01..1]

-- mQuadratic = plotMapLinear (\x -> x**2) (-2) 2 100 @@ [o1 "."] % title "Quadratic function"

-- | http://matplotlib.org/examples/pylab_examples/legend_demo3.html
-- mlegend = plotMapLinear (\x -> x ** 2) 0 1 100 @@ [o2 "label" "x^2"]
--   % plotMapLinear (\x -> x ** 3) 0 1 100 @@ [o2 "label" "x^3"]
--   % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]


meventplot xs ys = plot xs ys
  % mp # "ax.add_collection(mcollections.EventCollection(data[0], linelength=0.05))"
  % mp # "ax.add_collection(mcollections.EventCollection(data[1], orientation='vertical', linelength=0.05))"
  % text "0.1" "0.6" "Ticks mark the actual data points"
  % legend @@ [o2 "fancybox" True, o2 "shadow" True, o2 "title" "Legend", o2 "loc" "upper left"]
  -- where xs = [0..100]
  --       ys = map (\x -> (x ** 2)::Float) xs

plotData numEpisodes rewards = onscreen $ meventplot [1..numEpisodes] rewards
