
# kaggle

A reproducable build for dataHaskell kaggle.


# build

Built using:

    cabal init  --non-interactive kaggle -d "base,dataframe,perf,chart-svg,prettychart"

Run in ghci using:

    import Prelude
    :set -XImportQualifiedPost
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XTupleSections
    :set -XQuasiQuotes
    import Control.Category ((>>>))
    import Data.Function
    import Data.Maybe
    import Data.Bool
    import Data.List qualified as List
    import Control.Monad
    import Data.Bifunctor
    import DataFrame as D
    import Chart
    import Prettychart
    import Chart.Examples
    import Optics.Core hiding ((|>))
    import Perf
    import Data.ByteString.Char8 qualified as C
    import Data.Text qualified as T
    
    
    
    import Prettyprinter
    (display, quit) <- startChartServer (Just "kaggle")
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1

    Setting phasegrhsc it>o  stun... (port 9160) (ctrl-c to quit)


### Example chart display

    :t lineExample
    disp lineExample

    lineExample :: ChartOptions
    True

This gives you a browser page and live charting capabilities.


### file read testing

It&rsquo;s a good chunky first example.

    s <- readFile "other/test.csv"
    length s

    23021430

    rf = readFile "other/test.csv"
    (m,n) <- tickIO (length <$> rf)
    print n
    toSecs m

    23021430
    0.144087667

    (m,df) <- tickIO (D.readCsv "other/test.csv")
    print $ toSecs m
    :t df

    0.944859458
    df :: DataFrame


# s5e11

<https://www.kaggle.com/competitions/playground-series-s5e11>


## dataframe

    df <- D.readCsv "data/s5e11/test.csv"

    describeColumns df

    -----------------------------------------------------------------
        Column Name      | # Non-null Values | # Null Values |  Type
    ---------------------|-------------------|---------------|-------
            Text         |        Int        |      Int      |  Text
    ---------------------|-------------------|---------------|-------
    grade_subgrade       | 254569            | 0             | Text
    loan_purpose         | 254569            | 0             | Text
    employment_status    | 254569            | 0             | Text
    education_level      | 254569            | 0             | Text
    marital_status       | 254569            | 0             | Text
    gender               | 254569            | 0             | Text
    interest_rate        | 254569            | 0             | Double
    loan_amount          | 254569            | 0             | Double
    credit_score         | 254569            | 0             | Int
    debt_to_income_ratio | 254569            | 0             | Double
    annual_income        | 254569            | 0             | Double
    id                   | 254569            | 0             | Int


## box plot

    import DataFrame.Internal.Statistics

    c = (either (error . show) id) (columnAsDoubleVector "interest_rate" df)
    mean' c

    12.352323495791923

    import qualified Data.Vector.Algorithms.Intro as VA
    import qualified Data.Vector.Unboxed as VU
    import qualified Data.Vector.Unboxed.Mutable as VUM

    q4s = VU.toList $ quantiles' (VU.fromList [0,1,2,3,4]) 4 c
    :t q4s
    q4s

    q4s :: [Double]
    [3.2,10.98,12.37,13.69,21.29]


## box plot constructor

A box plot is:

-   (maybe) a vertical tick at the min
-   a LineChart from min to q1
-   a RectChart from q1 to q2
-   a RectChart from q2 to q3
-   a LineChart q3 to max
-   (maybe) a vertical tick at the max

    l1 = LineChart defaultLineStyle [[Point (q4s !! 0) 0.5, Point (q4s !! 1) 0.5]]
    l2 = LineChart defaultLineStyle [[Point (q4s !! 3) 0.5, Point (q4s !! 4) 0.5]]
    r1 = RectChart defaultRectStyle [Rect (q4s !! 1) (q4s !! 2) 0 1]
    r2 = RectChart defaultRectStyle [Rect (q4s !! 2) (q4s !! 3) 0 1]

    c = (mempty :: ChartOptions) & set #hudOptions defaultHudOptions & set #chartTree (unnamed [l1,r1,r2,l2])

    disp c

    True

    writeChartOptions "other/c.svg" c

![img](other/c.svg)


## vertical version

    qs = q4s
    l1 = LineChart defaultLineStyle [[Point 0.5 (qs !! 0), Point 0.5 (qs !! 1)]]
    l2 = LineChart defaultLineStyle [[Point 0.5 (qs !! 3), Point 0.5 (qs !! 4)]]
    r1 = RectChart defaultRectStyle [Rect 0 1 (qs !! 1) (qs !! 2)]
    r2 = RectChart defaultRectStyle [Rect 0 1 (qs !! 2) (qs !! 3)]

    c = (mempty :: ChartOptions) & set (#markupOptions % #chartAspect) (FixedAspect 0.25) & set #hudOptions defaultHudOptions & over (#hudOptions % #axes) (Prelude.drop 1) & set #chartTree (named "boxplot" [l1,r1,r2,l2])
    disp c
    writeChartOptions "other/box1.svg" c

    True

![img](other/box1.svg)


# reference

Comparable python:

<https://www.kaggle.com/code/ravitejagonnabathula/predicting-loan-payback>

notebook best practice:
<https://marimo.io/blog/lessons-learned>

converting to ipynb:
<https://pandoc.org/installing.html>

chart-svg api tree

![img](https://hackage-content.haskell.org/package/chart-svg-0.8.2.1/docs/other/ast.svg)

