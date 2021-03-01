module Analyzer
  ( median
  , splitRange
  , accumulateSolarTerm
  ) where

import Control.Arrow
import Data.Ratio
import Data.List
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

accumulateSolarTerm :: [Observed a] -> [(String, [HighLow a])]
accumulateSolarTerm observeds = first toJapaneseName <$> groupList (fst . latestSolarTerm jst . observedDate) observedTemperature observeds

groupList :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupList toKey toValue xs = keyValues
  where
    keyValues = (fst . head &&& fmap snd) <$> groupWith fst keyValuePairs
    keyValuePairs = (toKey &&& toValue) <$> xs
