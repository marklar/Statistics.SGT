module Statistics.SGT.Util where

sq :: (Num a) => a -> a
sq n = n * n

-- for use with non-Double args
(//) :: Integral a => a -> a -> Double
(//) a b = fromIntegral a / fromIntegral b

dbl :: Integral a => a -> Double
dbl = fromIntegral

dblLog :: Integral a => a -> Double
dblLog = log . dbl
