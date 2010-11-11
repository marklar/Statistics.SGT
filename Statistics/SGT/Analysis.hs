{-

Estimate the probability of elements based on observed data.
    http://en.wikipedia.org/wiki/Good-Turing_frequency_estimation

Observed:
  * X distinct species, numbered x = 1, ... , X.

Vectors:
  * Rx: frequencies.  # tokens observed for species x.
  * Nr: frequency of frequencies.  # times frequency r occurs in Rx.
    (e.g. N1: number of species with 1 observation.)

Total observations:
     N = ∑<r> (r · N<r>)

Calculation:

1. Estimate of the total probability of unseen objects.
       p<0> = N<1> / N.

2. Estimate probability for objects seen r times.
   (See: empirical Bayes method.)
       p<r> = (r+1) · S(N<r+1>)  /  N · S(N<r>)
   S(): smoothed (adjusted) value of frequency arg.

3. Smoothing:
   Would like to plot logN<r> versus logr.
   But problematic; for large r, many N<r> = 0.
   Instead, plot logZ<r> versus logr:
       Z<r> = N<r> / 0.5 · (t - q)
   q,r,t: consecutive subscripts with non-zero Nq,Nr,Nt.

   Now, fit a linear regression to log-log plot.
   For values of r:
      * too small: don't smooth.  S(Nr) = Nr
      * large enough: S(Nr) is read off the regression line.
      + Threshold: automatic procedure (using "confidence").

-}

module Statistics.SGT.Analysis
    ( probabilities
    )
where

import qualified Data.Vector as V

import Statistics.SGT.Types
import Statistics.SGT.Util
import Statistics.SGT.Line

probabilities :: Prob -> V.Vector Row -> Probabilities
probabilities confidenceFactor rows = Probs pZero ps
    where pZero = calcPZero rows
          ps = calcPs rows pZero confidenceFactor

----------------------

-- If no types were seen exactly 1x, then pZero = 0.
-- Else, % of all observations (bigN) which were for types seen 1x.
-- p<0> = N<1> / N
calcPZero :: V.Vector Row -> Prob
calcPZero rows =
    case rowWithR 1 rows of
      Nothing -> 0
      Just (Row _ n) -> n // bigN
    where
      bigN = sumOfProducts rows  -- total # of observations

sumOfProducts :: V.Vector Row -> Freq
sumOfProducts = V.sum . (V.map prod)
    where prod (Row a b) = a * b

----------------------

-- Estimate of probability for objects seen r times.
-- p<r> = (r+1) · S(N<r+1>)  /  N · S(N<r>)
calcPs :: V.Vector Row -> Prob -> Prob -> V.Vector Prob
calcPs rows pZero confidenceFactor =
    V.map calcP rStars
    where rStars = calcRStars confidenceFactor rows
          calcP rStar = (1.0 - pZero) * rStar / bigNPrime
          bigNPrime = V.sum $ V.zipWith prodN rows rStars
          prodN (Row _ n) = (*) (dbl n)

-- rStar: ADJUSTED count for types observed r times.
calcRStars :: Prob -> V.Vector Row -> V.Vector Prob
calcRStars confidenceFactor rows =
    V.imap rStar rows
    where
      bestFitLine = regress rows
      rStar idx (Row r n) =
          let y = dbl (r+1) * smooth bestFitLine (r+1) /
                  smooth bestFitLine r
          in
          case rowWithR (r+1) rows of
            Nothing -> y
            Just (Row _ nextN) ->
                if abs (x-y) <= confidence then y else x
                where
                  x = (dbl (r+1)) * (dbl nextN) / dbl n
                  confidence = confidenceFactor *
                               sqrt ( sq (dbl (r+1)) *
                                      dbl nextN / sq (dbl n) *
                                      (1 + (nextN // n)) )

-- Smooth: assumes Line is in LOG space.
smooth :: Line -> Freq -> Prob
smooth (Line intercept slope) r =
    exp $ intercept + slope * (dblLog r)

-- Linear regression.
-- Best Fit Line is in LOG space.
regress :: V.Vector Row -> Line
regress rows = findBestFit xs ys
    where xs = V.map (\(Row r _) -> dblLog r) rows
          ys = V.map log (calcZs rows)

-- For any Row, use the 'r' for neighbors fore and aft
-- to calculate a substitue for N<r>.
calcZs :: V.Vector Row -> V.Vector Prob
calcZs rows = V.imap z rows
    where z idx (Row _ n) =
              (2*n) // (deltaNeighborRs rows idx)

deltaNeighborRs :: V.Vector Row -> Int -> Freq
deltaNeighborRs rows idx = nextR - prevR
    where
      prevR = case rows V.!? (idx-1) of
               Nothing -> 0
               Just (Row r _) -> r
      nextR = case rows V.!? (idx+1) of
               Nothing -> 2 * r - prevR
                   where Row r _ = rows V.! idx
               Just (Row r _) -> r

-- Find the entry with the given 'r' (frequency), if any.
rowWithR :: Freq -> V.Vector Row -> Maybe Row
rowWithR r =
    V.find (\(Row r' _) -> r' == r)
