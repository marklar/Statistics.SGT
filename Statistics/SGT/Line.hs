module Statistics.SGT.Line
where

import Data.Vector as V

import Statistics.SGT.Util

-- For best fit of data.
data Line =
    Line { intercept :: Double
         , slope     :: Double
         }
    deriving (Show)

-- Can't make type sig more abstract?  (Num a => ...)
findBestFit :: V.Vector Double -> V.Vector Double -> Line
findBestFit xs ys =
    Line intercept slope
    where
      -- intercept
      intercept = meanY - slope * meanX
      -- slope
      slope = ( sumOfProductDeviations xs meanX ys meanY /
                sumOfSquaredErrors     xs meanX )
      -- means
      meanX = mean xs
      meanY = mean ys
      mean zs = V.sum zs / dbl (V.length zs)

sumOfProductDeviations :: Num a => V.Vector a -> a -> V.Vector a -> a -> a
sumOfProductDeviations xs meanX ys meanY =
    V.sum $ V.zipWith prod xs ys
        where prod x y = (x-meanX) * (y-meanY)

-- SSE
sumOfSquaredErrors :: Num a => V.Vector a -> a -> a
sumOfSquaredErrors ns mean =
    V.sum $ V.map (\n -> sq $ n - mean) ns
