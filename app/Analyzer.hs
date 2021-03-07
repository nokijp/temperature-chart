module Analyzer
  ( median
  , splitRange
  , accumulateMonthPart
  , accumulateSolarTerm
  ) where

import Control.Arrow
import Data.Ratio
import Data.List
import Data.Time.Calendar
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

accumulateMonthPart :: [Observed a] -> [(String, [HighLow a])]
accumulateMonthPart = accumulateTerm monthPart show

accumulateSolarTerm :: [Observed a] -> [(String, [HighLow a])]
accumulateSolarTerm = accumulateTerm (fst . latestSolarTerm jst) toJapaneseName

accumulateTerm :: Ord a => (Day -> a) -> (a -> String) -> [Observed b] -> [(String, [HighLow b])]
accumulateTerm toTerm toLabel observeds = first toLabel <$> groupList (toTerm . observedDate) observedTemperature observeds

groupList :: Ord k => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupList toKey toValue xs = keyValues
  where
    keyValues = (fst . head &&& fmap snd) <$> groupWith fst keyValuePairs
    keyValuePairs = (toKey &&& toValue) <$> xs

data MonthPart = EarlyPart Int | LaterPart Int deriving Eq
instance Ord MonthPart where
  EarlyPart i <= EarlyPart j = i <= j
  EarlyPart i <= LaterPart j = i <= j
  LaterPart i <= EarlyPart j = i < j
  LaterPart i <= LaterPart j = i <= j
instance Show MonthPart where
  show (EarlyPart m) = show m ++ "上"
  show (LaterPart m) = show m ++ "下"

monthPart :: Day -> MonthPart
monthPart day = if d <= gregorianMonthLength y m `div` 2
                then EarlyPart m
                else LaterPart m
  where
    (y, m, d) = toGregorian day
