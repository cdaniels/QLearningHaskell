module Main (main) where

import Environments

import Test.QuickCheck
import Test.Hspec

main :: IO ()

main = hspec $ do

  -- describe "Agenst.argMax" $ do
  --   it "should return the index of the maximum arg in a list" $
  --     property $ argMaxReturnsMaxIndex 
  
  describe "Environments.convertPosTo1D" $ do
    it "1D and 2D conversions should be inverse operations" $
       -- (convertPosTo2D $ (convertPosTo1D (-1, 1))) `shouldBe` (-1, 1)
      property $ dimensionConversionIsReversable 

  describe "Environments.step1D" $ do
    it "should move up one y with up action" $
      (convertPosTo2D $ step1D MoveUp (convertPosTo1D (3, 3))) `shouldBe` (3, 2)
    it "should move down one y with down action" $
      (convertPosTo2D $ step1D MoveDown (convertPosTo1D (3, 3))) `shouldBe` (3, 4)
    it "should move down with down action at far side" $
      (convertPosTo2D $ step1D MoveDown (convertPosTo1D (11, 3))) `shouldBe` (11, 4)
    it "should move left one x with left action" $
      (convertPosTo2D $ step1D MoveLeft (convertPosTo1D (3, 3))) `shouldBe` (2, 3)
    it "should move right one x with right action" $
      (convertPosTo2D $ step1D MoveRight (convertPosTo1D (3, 3))) `shouldBe` (4, 3)


  describe "Environments.stepEnv" $ do
    it "should terminate when moving to the goal state" $
      stepEnv MoveDown (convertPosTo1D (11, 2)) `shouldBe` ((convertPosTo1D (11, 3)), 0, True)



dimensionConversionIsReversable :: Int -> Int -> Bool 
dimensionConversionIsReversable x y 
  | x > 0 && x < gridW && y > 0 && y < gridH = (convertPosTo2D $ (convertPosTo1D (x, y))) == (x, y)
  | otherwise       = True

