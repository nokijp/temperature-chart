module FileParser
  ( readCSV
  ) where

import Data.Either
import Text.Read
import Data.List.Split
import Data.Time.Format

import Types

readCSV :: Read a => String -> ([String], [Observed a])
readCSV s = partitionEithers $ readObserved <$> lines s
  where
    readObserved :: Read a => String -> Either String (Observed a)
    readObserved line = maybe (Left line) Right $ do
      [d, h, l] <- Just $ splitOn "," line
      date <- parseTimeM False defaultTimeLocale "%-Y/%-m/%-d" d
      high <- readMaybe h
      low <- readMaybe l
      return $ Observed date $ HighLow high low
