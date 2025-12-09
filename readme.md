
# mdata

This repo contains:

-   a [dataframe](https://github.com/mchav/dataframe) example taken from a kaggle run.
-   [perf](https://github.com/tonyday567/perf) which is being used to measure performance of common usage patterns.
-   [chart-svg](https://github.com/tonyday567/chart-svg) | dataframe integration and development
-   a live chart build using [prettychart](https://github.com/tonyday567/prettychart)
-   some CI infrastructure to begin to measure integration.


# Imports and Pragmas

This snippet represents a current state being developed.

    :r
    
    :set -XNoImplicitPrelude
    :set -XImportQualifiedPost
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XQuasiQuotes
    
    -- base, text & bytestring encoding (compatability check, also)
    import Prelude as P
    import NumHask.Prelude qualified as N
    import Control.Category ((>>>))
    import Data.Function
    import Data.Maybe
    import Data.Bool
    import Data.List qualified as List
    import Control.Monad
    import Data.Bifunctor
    import Data.ByteString.Char8 qualified as C
    import Data.Text qualified as T
    
    -- prettyprinter (dev help)
    import Prettyprinter
    
    -- common dataframe imports
    import DataFrame qualified as D
    import DataFrame.Functions qualified as F
    import DataFrame.Internal.Expression qualified as D
    import DataFrame.Internal.Statistics qualified as D
    import qualified Data.Vector.Algorithms.Intro as VA
    import qualified Data.Vector.Unboxed as VU
    import qualified Data.Vector.Unboxed.Mutable as VUM
    
    -- common chart-svg imports
    import Chart
    import Prettychart
    import Chart.Examples
    import Optics.Core hiding ((|>),(<|))
    import Control.Lens qualified as Lens
    import Data.Data.Lens qualified as Lens
    
    -- dev helpers
    import Perf
    import Flow
    
    -- functions not yet transferred elsewhere
    import MData
    
    -- example data from https://www.kaggle.com/competitions/playground-series-s5e11
    dfTest <- D.readCsv "data/s5e11/test.csv"

    Configuration is affected by the following files:
    - cabal.project
    Build profile: -w ghc-9.12.2 -O1
    In order, the following will be built (use -v for more details):
     - mdata-0.1.0.0 (interactive) (lib) (first run)
    Preprocessing library for mdata-0.1.0.0...
    GHCi, version 9.12.2: https://www.haskell.org/ghc/  :? for help
    [1 of 1] Compiling MData            ( src/MData.hs, interpreted )
    Ok, one module loaded.
    Ok, one module reloaded.


## Live charts

This gives you a browser page and live charting capabilities.

    (display, quit) <- startChartServer (Just "mdata")
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1

    Setting phgahsceir>s  to stun... (port 9160) (ctrl-c to quit)

<http://localhost:9160/>

testing, testing; one, two, three

    disp unitExample

    True


## piePlot

idiomatic dataframe style?

    df0 = mempty |> D.insert "item" ["person","woman","man","camera","tv"] |> D.insert "value" [20,23.1,31,16,10]
    v = F.col @Double "value"
    xs = D.columnAsList @Double "value" df0
    xs' = (/ sum xs) <$> xs
    df = D.insert "prop" xs' df0
    df

    -------------------------------------
     item  | value  |        prop
    -------|--------|--------------------
    [Char] | Double |       Double
    -------|--------|--------------------
    person | 20.0   | 0.1998001998001998
    woman  | 23.1   | 0.2307692307692308
    man    | 31.0   | 0.3096903096903097
    camera | 16.0   | 0.15984015984015984
    tv     | 10.0   | 9.99000999000999e-2


## stacked bar


### version 1: single stacked vertical bar chart

    ls = T.pack <$> D.columnAsList @String "item" df
    vs = D.columnAsList @Double "prop" df
    bd = BarData (fmap pure vs) ["item"] ls

    bc = barChart (defaultBarOptions |> set #displayValues False |> set #barStacked Stacked |> set (#barRectStyles % each % #borderSize) 0) bd
    disp bc

![img](other/bar1.svg)


### version 2: skinny

    bc' = Lens.transformOnOf Lens.template Lens.uniplate (over chroma' (*1.5) .> over opac' (*0.6)) bc |> set (#markupOptions % #chartAspect) (FixedAspect 0.4)
    
    disp (bc')

![img](other/bar2.svg)


### version 3: remove legend and embed labels

    
    acc0 = List.scanl' (+) 0 vs <> [1]
    mids = zipWith (\a0 a1 -> (a0+a1)/2) acc0 (List.drop 1 acc0)
    ct = zipWith (\c (t,a) -> TextChart (defaultTextStyle |> set #size 0.05 |> set #color (palette c |> over lightness' (*0.6))) [(t, Point zero (0.5-a))]) [0..] (zip ls mids)
    
    bc'' = bc' |> set (#hudOptions % #legends) mempty |> over #chartTree (<> named "labels" ct)
    
    disp (bc'')

    True


## pie secants

Pie chart convention starts at the y-axis and lays out secant slices clockwise.

\`ra\` maps (0,1) (the proportional pie slice) into a point on a unit circle (by this convetion).

    ra = (+(-0.25)) .> (*(-2 * pi)) .> ray @(Point Double)
    secantPie (Secant o r a0 a1) = singletonPie o (ArcPosition (o N.+ ra a0) (o N.+ ra a1) (ArcInfo (Point r r) 0 False True))

This is a very common scan for a Column.

    acc0 = List.scanl' (+) 0 vs <> [1]
    mids = zipWith (\a0 a1 -> (a0+a1)/2) acc0 (List.drop 1 acc0)
    
    xs = zipWith (\a0 a1 -> secantPie (Secant (0.05 N.*| ra ((a0+a1)/2)) one a0 a1)) acc0 (List.drop 1 acc0)
    
    cs = zipWith (\c x -> PathChart (defaultPathStyle |> set #borderSize 0 |> set #color (paletteO c 0.3)) x) [0..] xs
    
    ct = zipWith (\c (t,a) -> TextChart (defaultTextStyle |> set #size 0.05 |> set #color (palette c & over lightness' (*0.6))) [(t, 0.7 N.*| ra a)]) [0..] (zip ls mids)
    co = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) ChartAspect & set #chartTree ((cs <> ct) |> unnamed)
    disp co
    writeChartOptions "other/pie.svg" co

    True

![img](other/pie.svg)

