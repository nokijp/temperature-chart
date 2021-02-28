module Types
  ( HighLow(..)
  , Observed(..)
  , Layer(..)
  , Range(..)
  ) where

import Data.Time.Calendar

data HighLow a = HighLow { highLowHigh :: a
                         , highLowLow :: a
                         } deriving Show

data Observed a = Observed { observedDate :: Day
                           , observedTemperature :: HighLow a
                           } deriving Show

data Layer = LayerLL | LayerLH | LayerM | LayerHL | LayerHH deriving Show
data Range a = Range { rangeLayer :: Layer
                     , rangeHigh :: a
                     , rangeLow :: a
                     } deriving Show
