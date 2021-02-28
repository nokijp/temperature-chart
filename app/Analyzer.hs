module Analyzer
  ( median
  , splitRange
  , accumulateSolarTerm
  ) where

import Control.Arrow
import Data.Ratio
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time.JapaneseCalendar
import GHC.Exts

import Types

median :: Ord a => [a] -> a
median xs = sort xs !! round ((length xs - 1) % 2)

splitRange :: RealFrac a => [a] -> [b] -> [(b, b)]
splitRange chunkSizes xs = ranges
  where
    ranges = zipWith (curry $ (xs !!) *** (xs !!)) separatorIndices (tail separatorIndices)
    separatorIndices = (round . (* fromIntegral maxIndex)) <$> separatorPositions
    separatorPositions = scanl (+) 0 $ (/ sum chunkSizes) <$> chunkSizes
    maxIndex = length xs - 1

accumulateSolarTerm :: [Observed a] -> Map SolarTerm [HighLow a]
accumulateSolarTerm = groupMap (fst . latestSolarTerm jst . observedDate) observedTemperature

groupMap :: Ord k => (a -> k) -> (a -> v) -> [a] -> Map k [v]
groupMap toKey toValue xs = M.fromList keyValues
  where
    keyValues = (fst . head &&& fmap snd) <$> groupWith fst keyValuePairs
    keyValuePairs = (toKey &&& toValue) <$> xs
