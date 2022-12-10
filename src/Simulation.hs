module Simulation (performEpisodes) where



-- someFunc :: IO ()
performEpisodes env agent = "TODO" --

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
--   env <- basicEnv
--   void $ execStateT finalAction env
--   where
--     numEpisodes = 10000
--     decayRate = 0.9
--     minEpsilon = 0.01

--     finalAction :: StateT FrozenLakeEnvironment IO ()
--     finalAction = do
--       rewards <- forM [1..numEpisodes] $ \i -> do
--         resetEnv
--         when (i `mod` 100 == 99) $ do
--           fle <- get
--           let e = explorationRate fle
--           let newE = max minEpsilon (e * decayRate)
--           put $ fle { explorationRate = newE }
--         (_, reward) <- gameLoop chooseActionQTable
--         return reward
--       lift $ print (sum rewards)



-- gameLoop :: (MonadIO m) =>
--   StateT FrozenLakeEnvironment m Action ->
--   StateT FrozenLakeEnvironment m (Observation, Double)
-- gameLoop chooseAction = do
--   oldObs <- currentObservation <$> get
--   newAction <- chooseAction
--   (newObs, reward, done) <- stepEnv newAction
--   learnQTable oldObs newObs reward newAction
--   if done
--     then do
--       if reward > 0.0 
--         then liftIO $ putStrLn "Win"
--         else liftIO $ putStrLn "Lose"
--       return (newObs, reward)
--     else gameLoop chooseAction
