module Main (main) where

import Environments

import Test.QuickCheck
import Test.Hspec

main :: IO ()
main = do
  describe "Agenst.argMax" $ do
    it "should return the index of the maximum arg in a list" $
      property $ argMaxReturnsMaxIndex 

  -- describe "Data.RedBlackTree.fromList" $ do
  --   it "sorts and eliminates duplicates via a round-trip with toList" $
  --     property $ roundTripSort
  --   it "satisfies invariant 0 (Ordered)" $
  --     property $ ordered
  --   it "satisfies invariant 1 (Black Root)" $
  --     property $ blackRoot
  --   it "satisfies invariant 2 (No Red Chains)" $
  --     property $ noRedChains
  --   it "satisfies invariant 3 (Equal Paths)" $
  --     property $ equalPaths



-- argMaxReturnsMaxIndex :: [a] -> Bool
-- argMaxReturnsMaxIndex [0,1,6] = 

{-# language ExtendedDefaultRules, ScopedTypeVariables, QuasiQuotes, ParallelListComp #-}

