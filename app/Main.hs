module Main
  ( main
  ) where

import Data.List
import Data.Ratio
import Diagrams.Prelude
import System.Environment
import System.IO

import Analyzer
import FileParser
import Drawer
import Types

main :: IO ()
main = do
  [csvFilePath, svgHighFilePath, svgLowFilePath] <- getArgs

  rawCSV <- readFile csvFilePath
  let (ignoredLines, observeds) = readCSV rawCSV
  mapM_ (hPutStrLn stderr . ("ignored: " ++)) ignoredLines

  let
    items = toItem <$> accumulateSolarTerm observeds
    toItem (label, temps) = let
                              hs = highLowHigh <$> temps
                              ls = highLowLow <$> temps
                            in ChartItem label
                                         (HighLow (median hs) (median ls))
                                         (HighLow (layerize layers hs) (layerize layers ls))
    layers = [ (LayerLL,  5 % 100)
             , (LayerLH, 20 % 100)
             , (LayerM,  50 % 100)
             , (LayerHL, 20 % 100)
             , (LayerHH,  5 % 100)
             ]
  draw def layers (svgHighFilePath, svgLowFilePath) items

layerize :: Ord a => [(Layer, Rational)] -> [a] -> [Range a]
layerize layers = zipWith toRange (fst <$> layers) . splitRange (snd <$> layers) . sort
  where
    toRange layer (l, h) = Range layer h l
