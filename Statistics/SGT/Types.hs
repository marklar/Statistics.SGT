module Statistics.SGT.Types where

import qualified Data.Vector as V

type Prob = Double
type Freq = Int

data Row = Row { r :: Freq -- ^ frequency (num tokens) of type ("species")
               , n :: Freq -- ^ frequency of frequency
               }
           deriving (Show)

data Probabilities =
    Probs { pZero :: Prob          -- ^ Total P for unseen objects.
          , pSeen :: V.Vector Prob -- ^ P for types present in sample.
          }
    deriving (Show)
