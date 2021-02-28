{-# LANGUAGE TypeFamilies, RankNTypes #-}

module Drawer
  ( ChartItem(..)
  , ChartStyle(..)
  , draw
  ) where

import Control.Arrow hiding ((|||))
import Diagrams
import Diagrams.Backend.SVG
import Diagrams.Prelude

import Types

data ChartItem a = ChartItem { chartItemLabel :: String
                             , chartItemMedian :: HighLow a
                             , chartItemRange :: HighLow [Range a]
                             }

data ChartStyle a = ChartStyle { chartStyleImageWidth :: a
                               , chartStyleImageHeight :: a
                               , chartStyleScaleX :: a
                               , chartStylePad :: a
                               , chartStyleXLabelFontSize :: a
                               , chartStyleYLabelFontSize :: a
                               , chartStyleYLabelInterval :: a
                               , chartStyleXAxisGap :: a
                               , chartStyleYAxisGap :: a
                               , chartStyleAxisWidth :: a
                               , chartStyleSubLineWidth :: a
                               , chartStyleAxisColor :: AlphaColour a
                               , chartStyleSubLineColor :: AlphaColour a
                               , chartStyleRangeOpacity :: Layer -> a
                               , chartStyleMedianLineWidth :: a
                               , chartStyleMedianLineColor :: AlphaColour a
                               , chartStyleLegendWidth :: a
                               , chartStyleLegendHeight :: a
                               , chartStyleLegendFontSize :: a
                               , chartStyleLegendGap :: a
                               }

instance Fractional a => Default (ChartStyle a) where
  def = ChartStyle { chartStyleImageWidth = 1200.0
                   , chartStyleImageHeight = 400.0
                   , chartStyleScaleX = 3.5
                   , chartStylePad = 0.1
                   , chartStyleXLabelFontSize = 0.04
                   , chartStyleYLabelFontSize = 0.04
                   , chartStyleYLabelInterval = 5.0
                   , chartStyleXAxisGap = 0.015
                   , chartStyleYAxisGap = 0.015
                   , chartStyleAxisWidth = 0.003
                   , chartStyleSubLineWidth = 0.001
                   , chartStyleAxisColor = opaque black
                   , chartStyleSubLineColor = black `withOpacity` 0.3
                   , chartStyleRangeOpacity = layerToOpacity
                   , chartStyleMedianLineWidth = 0.002
                   , chartStyleMedianLineColor = opaque black
                   , chartStyleLegendWidth = 0.15
                   , chartStyleLegendHeight = 0.1
                   , chartStyleLegendFontSize = 0.04
                   , chartStyleLegendGap = 0.05
                   }
    where
      layerToOpacity LayerLL = 0.1
      layerToOpacity LayerLH = 0.3
      layerToOpacity LayerM  = 0.5
      layerToOpacity LayerHL = 0.3
      layerToOpacity LayerHH = 0.1

draw :: ChartStyle Double -> [(Layer, Rational)] -> (FilePath, FilePath) -> [ChartItem Double] -> IO ()
draw style layers (pathHigh, pathLow) items = do
  let
    minTemperature = minimum [rangeLow range | item <- items, range <- highLowLow $ chartItemRange item]
    maxTemperature = maximum [rangeHigh range | item <- items, range <- highLowHigh $ chartItemRange item]
    minTemperatureBound = fromInteger (floor $ minTemperature / chartStyleYLabelInterval style) * chartStyleYLabelInterval style
    maxTemperatureBound = fromInteger (ceiling $ maxTemperature / chartStyleYLabelInterval style) * chartStyleYLabelInterval style
    temperatureBound = (minTemperatureBound, maxTemperatureBound)
    horizontalLineTemperatures = [minTemperatureBound, minTemperatureBound + chartStyleYLabelInterval style .. maxTemperatureBound]
  let
    highChart = chart red highLowHigh
    lowChart = chart blue highLowLow

    chart :: Colour Double -> (forall a. HighLow a -> a) -> Diagram SVG
    chart color select = content # centerXY # pad (1.0 + chartStylePad style)
      where
        content = yl ||| (((frameLines <> bars) # scaleX (chartStyleScaleX style) ||| legend style color layers) === xl)
        bars = rangesChart style color temperatureBound ((select . chartItemMedian &&& select . chartItemRange) <$> items)
        frameLines = chartFrame style temperatureBound horizontalLineTemperatures
        xl = xLabel style $ chartItemLabel <$> items
        yl = yLabel style temperatureBound $ (id &&& show) <$> horizontalLineTemperatures
  let
    svgSize = mkSizeSpec2D (Just $ chartStyleImageWidth style) (Just $ chartStyleImageHeight style)
  renderSVG pathHigh svgSize highChart
  renderSVG pathLow svgSize lowChart

legend :: ChartStyle Double -> Colour Double -> [(Layer, Rational)] -> Diagram SVG
legend style color layers = centerXY $ space ||| foldl1 (===) (drawLayer <$> (second percentString <$> layers))
  where
    drawLayer (layer, title) =  text title # fontSizeL (chartStyleLegendFontSize style)
                             <> rect (chartStyleLegendWidth style) (chartStyleLegendHeight style) # fcA (layerColor layer) # withoutLines
    layerColor layer = color `withOpacity` chartStyleRangeOpacity style layer
    percentString x = show (round $ x * 100 :: Int) ++ "%"
    space = strut unitX # scaleX (chartStyleLegendGap style)

yLabel :: ChartStyle Double -> (Double, Double) -> [(Double, String)] -> Diagram SVG
yLabel style (minValue, maxValue) labels = foldMap drawLabel labels ||| space
  where
    drawLabel (y, s) = alignedText 1.0 0.5 s # fontSizeL (chartStyleYLabelFontSize style) # translateY (shiftValue y)
    shiftValue y = (y - minValue) / (maxValue - minValue) - 0.5
    space = strut unitX # scaleX (chartStyleYAxisGap style)

xLabel :: ChartStyle Double -> [String] -> Diagram SVG
xLabel style labels = space === foldMap drawLabel (zip xs labels)
                    # withEnvelope (rect chartWidth 0.0 :: Diagram SVG)
  where
    drawLabel (x, s) = alignedText 0.5 1.0 s # fontSizeL (chartStyleXLabelFontSize style) # translateX x :: Diagram SVG
    xs = shiftValue <$> [0 .. length labels - 1]
    shiftValue i = (fromIntegral (i * 2 + 1) / fromIntegral (length labels * 2) - 0.5) * chartWidth
    chartWidth = chartStyleScaleX style
    space = strut unitY # scaleY (chartStyleXAxisGap style)

chartFrame :: ChartStyle Double -> (Double, Double) -> [Double] -> Diagram SVG
chartFrame style (minValue, maxValue) values =  (hrule 1.0 # translateY (-0.5) # styleAxis)
                                             <> (vrule 1.0 # translateX (-0.5) # styleAxis)
                                             <> foldMap (styleSubLine . line) (tail values)
  where
    line y = hrule 1.0 # translateY (shiftValue y) :: Diagram SVG
    shiftValue y = (y - minValue) / (maxValue - minValue) - 0.5
    styleAxis = lwG (chartStyleAxisWidth style) . lcA (chartStyleAxisColor style)
    styleSubLine = lwG (chartStyleSubLineWidth style) . lcA (chartStyleSubLineColor style)

rangesChart :: ChartStyle Double -> Colour Double -> (Double, Double) -> [(Double, [Range Double])] -> Diagram SVG
rangesChart style color bound items = normalizeX diagram # centerXY
  where
    normalizeX = scaleX $ 1.0 / fromIntegral (length items)
    diagram = foldl1 (|||) $ rangeChart style color bound <$> items

rangeChart :: ChartStyle Double -> Colour Double -> (Double, Double) -> (Double, [Range Double]) -> Diagram SVG
rangeChart style color (minValue, maxValue) (median, ranges) = medianLine <> foldMap rangeComponent ranges
  where
    rangeComponent :: Range Double -> Diagram SVG
    rangeComponent range = rangeRect range
                         # fcA (rangeColor range)
                         # withoutLines
                         # withEnvelope (square 1.0 # alignYMin :: Diagram SVG)
    rangeRect range = let
                        top = shiftValue $ rangeHigh range
                        bottom = shiftValue $ rangeLow range
                      in rect 1.0 (top - bottom) # alignYMin # translateY bottom
    rangeColor range = color `withOpacity` chartStyleRangeOpacity style (rangeLayer range)
    medianLine = hrule 1.0
               # translateY (shiftValue median)
               # lcA (chartStyleMedianLineColor style)
               # lwG (chartStyleMedianLineWidth style)
    shiftValue y = (y - minValue) / (maxValue - minValue)

withoutLines :: Diagram SVG -> Diagram SVG
withoutLines = lw none . lcA transparent
