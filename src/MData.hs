{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | A place to put functions until they get dumped or transferred elsewhere
module MData where

import Chart
import Prelude as P
import Prettychart
import DataFrame qualified as D
import DataFrame.Internal.Statistics qualified as D
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Exts
import GHC.Generics
import Optics.Core
import Data.Text qualified as T

data BoxPlotOptions =
  BoxPlotOptions {
    q2Style :: Style,
    q3Style :: Style,
    q1Style :: Style,
    q4Style :: Style
  } deriving (Generic, Show, Eq)

defaultBoxPlotOptions = BoxPlotOptions defaultRectStyle defaultRectStyle defaultLineStyle defaultLineStyle

boxPlot :: BoxPlotOptions -> VU.Vector Double -> ChartOptions
boxPlot o v = c
  where
    qs = VU.toList $ D.quantiles' (VU.fromList [0,1,2,3,4]) 4 v

    l1 = LineChart (view #q1Style o) [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l4 = LineChart (view #q4Style o) [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r2 = RectChart (view #q2Style o)  [Rect 0 1 (qs !! 1) (qs !! 2)]
    r3 = RectChart (view #q3Style o) [Rect 0 1 (qs !! 2) (qs !! 3)]
    c = (mempty :: ChartOptions) &
      set (#markupOptions % #chartAspect) (FixedAspect 0.25) &
      set #hudOptions defaultHudOptions &
      over (#hudOptions % #axes) (P.drop 1) &
      set #chartTree (named "boxplot" [l1,r2,r3,l4])

data ScatterPlotOptions =
  ScatterPlotOptions {
    scatterStyles :: [Style],
    maxScatters :: Int,
    maxPoints :: Int,
    titleX :: TitleOptions,
    titleY :: TitleOptions
  } deriving (Generic, Show, Eq)

defaultScatterPlotOptions = ScatterPlotOptions (P.take 8 $ zipWith (\s c -> defaultGlyphStyle & set #glyphShape s & set #color c) (gpalette <$> [0..7]) (palette <$> [0..7])) 8 1000 (defaultTitleOptions mempty & set (#style % #size) 0.06 & set #place PlaceBottom) (defaultTitleOptions mempty & set (#style % #size) 0.06 & set #buffer 0.1 & set #place PlaceLeft)

-- | first vector is the x axis values
scatterPlot :: ScatterPlotOptions -> (Maybe T.Text, VU.Vector Double) -> (Maybe T.Text, VU.Vector Double) -> ChartOptions
scatterPlot o (t0, v0) (t1, v1) = ch'
  where
    c = GlyphChart (head (view #scatterStyles o)) (P.take (view #maxPoints o) $ zipWith Point (VU.toList v0) (VU.toList v1))
    ch' = (mempty :: ChartOptions) & set #chartTree (named "scatterPlot" [c]) & set #hudOptions ho
    ho = defaultHudOptions & maybe id (\tx -> over #titles ((Priority 8 (view #titleX o & set #text tx)):)) t0 & maybe id (\ty -> over #titles ((Priority 8 (view #titleY o & set #text ty)):)) t1



{-
hist :: [Double] -> Double -> [Int]
hist values binCount =
    let countBin b = length [v | v <- values, v >= b && v < b + binWidth]
     in map countBin bins
-}
