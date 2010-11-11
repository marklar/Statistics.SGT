{-# LANGUAGE BangPatterns #-}

-- Based on: http://www.grsampson.net/D_SGT.c
-- by Geoffrey Sampson.

module Main where

import Text.Printf
import Data.Vector as V
import System.Environment

import Statistics.SGT.Observations (fromFile)
import Statistics.SGT.Analysis (probabilities)
import Statistics.SGT.Types

-- | Confidence Factor
-- 1.96 corresponds to the p < 0.05 criterion.
-- In order to use the p < 0.1 criterion, change to 1.65.
confidenceFactor = 1.96 :: Prob

main = do
  [f] <- getArgs
  rows <- fromFile f
  printProbs rows $ probabilities confidenceFactor rows

printProbs :: V.Vector Row -> Probabilities -> IO ()
printProbs rows (Probs pZero pSeen) = do
  printf "0\t%.4f\n" pZero
  V.mapM_ putStr strs
   where strs = V.zipWith mkStr rows pSeen
         mkStr (Row r _) p = (printf "%d\t%.4f\n" r p) :: String
