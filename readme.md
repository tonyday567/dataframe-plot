
# dataframe-plot

dataframe plots and operations.

Charts for dataframes built with chart-svg.


# ToDo plan

-   write main chart functions
    -   write with a [Double] data type, HistogramOption style.
    -   example
    -   main options

-   write \`D.plot df\`
    -   Using dataframe metadata for default axis titles


# Next Examples

-   Histogram

At defaults

    df = dfTest
    
    -- This doesnt work
    -- c0 = D.columnAsList "interest_rate" df
    c0 = D.columnAsList @Double "interest_rate" df
    h = histogram defaultHistogramOptions c0
    
    :t h
    disp h
    rend "other/histogram.svg" h

    h :: ChartOptions
    True

![img](other/histogram.svg)

skinny, with a title & no axis

    h2 = h |> setTitle "penguins" |> set (#hudOptions % #axes) [] |> set (#markupOptions % #chartAspect) (FixedAspect 0.5)
    
    disp h2
    rend "other/histogram2.svg" h2

    True

![img](other/histogram2.svg)

    h3 = h |> over (#chartTree % chart' % #chartData % rectData') (fmap (fmap N.flipAxes))
    
    disp h3
    rend "other/histogram3.svg" h3

    True

![img](other/histogram3.svg)

    :t N.flipAxes

    N.flipAxes :: Rect a -> Rect a

-   Line
    -   multiple series, markers, dashed lines
-   Scatter
    
    single scatter
    multi-scatter
    multi-glyphed
-   Bar
    -   simple, grouped, stacked, horizontal
-   Pie


# ToDo design notes


## open issues

-   Compare <> usage with ggplot&rsquo;s \`+\` operator


## chart-svg design (and dataframe-plot by usage):

-   Outputs SVG Text as the primary output (browser embeddable, iHaskell compatible, easily saved)
    -   markup-parse and Markup used as an intermediary type between a ChartOption and Text types is exposed in the chart-svg API.
-   Math-based content exposed is based in numhask-space: histogram/quantile computation, time calcs, Rect and Point math.
-   ChartData is two-dimensional via either \`Point x y\` or \`Rect x0 x1 y0 y1\`.
-   Maximally configurable with no hard-coded values.
-   ChartOptions is simply MarkupOptions (SVG settings), HudOptions (axes, titles, legends), and a ChartTree containing a tree of named charts. ChartOptions are showable, serializable and can be considered a compressed form of an eventual SVG text-based chart.
-   A Chart divides into Style (shared by all Chart types) and ChartData, a sum type splitting into 6 basic chart primitives.
-   SVG inspired usage of Path, opacity and automatic scaling.


## (accepted?) limitations

-   Design leads to a large API.
-   Relies on lens
-   More complex than desired ergonomic interface
-   Requires understanding of SVG internals for advanced use


## API Anaysis

Starting with a postscript, suffixed or pipe-based approach with operators similar to many jupyter providers:

    Plt.histogram someData
      |> Plt.numBins 30
      |> Plt.colourWith otherData
      |> Plt.render
    
    Plt.scatter penguinHeight
      |> Plt.against penguinWeight
      |> Plt.colourWith penguinSpecies
      |> Plt.setTitle "my penguin chart"
      |> Plt.render


### `histogram df`

It&rsquo;s natural (ie a paper-thin layer) to have `histogram df` produce a ChartOption with reasonable defaults for a histogram. It might be better to separate api&rsquo;s but assuming this anyway is quite good for early development.

`histogram == histogramWith defaultHistogramOptions` is a really nice way of tucking the config away until needed. It can be wrapped in a ReaderT and form part of a bigger, pure-layer cake if needed. I find that embedding that ReaderT decision is an antipattern, and destroyes functional coherence, and prefer the function hook.


### Yes to setTitle

`chart-svg` is biased in that it provides `optics-core` lenses, using them internally, in examples, and exposed in the API.

So `Plt.setTitle "my penguin chart"` is roughly:

\`

    o = defaultHistogramOptions |> set #histogramTitle "my penguin chart"
    histogramWith o df

However, should a histogram reliably have a title?

You can always add a title to a ChartOptions, or remove it, or any hud element, or the data etc etc, so a case can be made for having a `setTitle :: ChartOptions -> ChartOptions` in the pipeline that covers all charts rather than a title option in each chart type.

So then we can say:

    histogram df |> setTitle "my penguin chart"

I think this logic stretches to all the axis and tick manipulations you can think of.


### No to setBins

Because bins (called grain in numhask-space) is a parameter used to make the charts not decorate their edges like title, you can&rsquo;t use the same pattern. If you change your mind on bins, you will be rerunning histogram.

    o = defaultHistogramOptions |> set #grain 10
    histogramWith o df

To write a setBins, you need the df again rather than a ChartOptions, so you can&rsquo;t write it.


### dataframe versus Column versus [Double]

Plt.colourWith penguinSpecies looks like it assumes a dataframe under the hood, or that the shape of penguinSpecies matches penguinHeight.

`scatter df` feels like an operation on a two column dataframe, and then you color the glyphs according to a row-name mapping of penguinHeight with penguinSpecies. I would likely use dataframes to organize that internally to chart-svg if I had to.

Stepping through the options:

Here&rsquo;s a single scatter no species colouring:

    scatter :: ScatterOptions -> [(Double,Double)] -> ChartOptions

multiple penguin species can be coloured like:

    scatters :: [(ScatterOptions, [Double, Double])] -> ChartOptions
    scatters xs = zipWith scatter .> mconcat

You can see here how data (species) bleeds into Style (colour). This is not the grammar of plots. `ChartData` is an enforcement of specifying XY-ity of the data so automatic scaling is easy (survives the combinators).


### why dataframe is a nice input.

    scatter :: ScatterOptions -> DataFrame -> ChartOptions

Does scatter mean a single styled series of points or a multiply-coloured cast of penguins can be included in ScatterOptions. It could be:

    defaultScatterOptions = ScatterOptions ScatterSchema
    
    data ScatterSchema = [("x", 0), ("y", 1), ("colors", Just 2)]

which can mean that the x and y data for Points is in columns 0 and 1 and the colors are in column 2 (but could be absent).


### Combinators

-   the combinators in the library could be better developed.
    -   explain how Chart, ChartTree & ChartOptions combine.
    -   `vert`, `hori`, `stack` need work
    -   sharing axes is possible but might be fragile.


# Development

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
    import NumHask.Space qualified as N
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
    
    -- random variates
    import System.Random.Stateful
    import System.Random.MWC
    import System.Random.MWC.Distributions
    
    -- dev helpers
    import Perf
    import Flow
    
    -- dataframe chart-svg interface
    import DataFrame.Plot
    
    -- example data from https://www.kaggle.com/competitions/playground-series-s5e11
    dfTest <- D.readCsv "other/s5e11/test.csv"
    v = F.col @Double "value"
    df0 = mempty |> D.insert "item" ["person","woman","man","camera","tv"] |> D.insert "value" [20,23.1,31,16,10]
    xs = D.columnAsList @Double "value" df0
    xs' = (/ sum xs) <$> xs
    df = D.insert "prop" xs' df0
    
    -- initialize a random seed
    -- uniformRM (0,1) g :: IO Double
    g <- initialize $ VU.fromList [1,2,3]
    
    rend = writeChartOptions


## Live charts

This gives you a browser page and live charting capabilities.

    -- live charts
    (display, quit) <- startChartServer Nothing
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1

    Setting phasergsh ctio>  stun... (port 9160)g h(ccit>r l-c to quit)

<http://localhost:9160/>

testing, testing; one, two, three

    disp unitExample

    True

# reference

Comparable python:

<https://www.kaggle.com/code/ravitejagonnabathula/predicting-loan-payback>

notebook best practice:

<https://marimo.io/blog/lessons-learned>

converting to ipynb:

<https://pandoc.org/installing.html>

    pandoc readme.md -o mdata.ipynb

chart-svg api tree

![img](https://hackage-content.haskell.org/package/chart-svg-0.8.2.1/docs/other/ast.svg)

