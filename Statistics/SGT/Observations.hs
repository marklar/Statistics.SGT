{-# LANGUAGE BangPatterns #-}

module Statistics.SGT.Observations
    ( fromFile
    )
where
 
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector                as V

import Statistics.SGT.Types
 
-- Fill new vector from file containing [(Int,Int)].
fromFile :: FilePath -> IO (V.Vector Row)
fromFile fn = do
  s <- L.readFile fn
  return $ parse s
 
parse :: L.ByteString -> V.Vector Row
parse = V.unfoldr nextRow

-- Bang patterns: ensure strict accumulation of state in parsing.
nextRow :: L.ByteString -> Maybe (Row, L.ByteString)
nextRow (!s) = do
  (!r, !t)  <- nextInt s
  (!n, !t') <- nextInt t
  Just (Row r n, t')

nextInt :: L.ByteString -> Maybe (Int, L.ByteString)
nextInt (!s) = do
  (!i, !t) <- L.readInt $ L.dropWhile isWhite s
  Just (i, t)
    where isWhite c = c `elem` " \t\n"
