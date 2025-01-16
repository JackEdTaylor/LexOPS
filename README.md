
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LexOPS <img src="man/figures/hex.png" align="right" style="padding-left:10px;background-color:white" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Version:
0.4.0](https://img.shields.io/badge/version-0.4.0-blue.svg)](https://github.com/JackEdTaylor/LexOPS/releases)
[![DOI:
10.3758/s13428-020-01389-1](https://zenodo.org/badge/DOI/10.3758/s13428-020-01389-1.svg)](https://doi.org/10.3758/s13428-020-01389-1)
<!-- badges: end -->

LexOPS is an R package for generating matched stimuli for factorial
design experiments. You can use the functions on any dataframe, but
there is an inbuilt database of example features for English words for
psycholinguistics studies in English (`LexOPS::lexops`).

## Installation

LexOPS can be installed as an R package with:

``` r
devtools::install_github("JackEdTaylor/LexOPS@*release")
```

## How to Use

:book: In-depth walkthrough of the package:
<https://jackedtaylor.github.io/LexOPSdocs/>

:mortar_board: Paper about the package: [Taylor, Beith, and Sereno
(2020)](https://doi.org/10.3758/s13428-020-01389-1)

## TL;DR

LexOPS makes it easy to generate matched stimuli in a reproducible way.
The functions work on any dataframe, but there is an inbuilt dataset,
`LexOPS::lexops`, containing psycholinguistic variables for English
words.

### The “Generate Pipeline”

The following example pipeline generates 50 words per condition (200 in
total), for a study with a 2 x 2, syllables (1, 2) by concreteness (low,
high) design. Words are matched by length exactly, and by word frequency
within a tolerance of ±0.2 Zipf.

``` r
library(LexOPS)

stim <- lexops |>
  split_by(Syllables.CMU, 1:1 ~ 2:2) |>
  split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
  control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
  control_for(Length) |>
  generate(n = 50, match_null = "balanced")
```

    #> Generated 50/50 (100%). 157 total iterations, 0.32 success rate.

A preview of what was generated:

``` r
# create a table of the first 5 rows of the output
stim |>
  head(5) |>
  knitr::kable()
```

| item_nr | A1_B1  | A1_B2  | A2_B1  | A2_B2  | match_null |
|--------:|:-------|:-------|:-------|:-------|:-----------|
|       1 | gist   | yank   | iffy   | tofu   | A1_B2      |
|       2 | oomph  | speck  | hyper  | rabbi  | A2_B1      |
|       3 | worst  | voice  | lucky  | cover  | A1_B1      |
|       4 | suave  | stoop  | avail  | lilac  | A2_B2      |
|       5 | shrewd | starch | bygone | condom | A2_B1      |

### Review Generated Stimuli

The `plot_design()` function produces a plot summarising the generated
stimuli.

``` r
plot_design(stim)
```

<img src="man/figures/README-fig1-1.png" width="100%" />

### Convert to Long Format

The `long_format()` function coerces the generated stimuli into long
format.

``` r
# present the same 20 words as in the earlier table
long_format(stim) |>
  head(20) |>
  knitr::kable()
```

| item_nr | condition | match_null | string | Zipf.SUBTLEX_UK | Length | Syllables.CMU | CNC.Brysbaert |
|---:|:---|:---|:---|---:|---:|---:|---:|
| 1 | A1_B1 | A1_B2 | gist | 2.974489 | 4 | 1 | 1.81 |
| 1 | A1_B2 | A1_B2 | yank | 2.933782 | 4 | 1 | 4.10 |
| 1 | A2_B1 | A1_B2 | iffy | 2.928732 | 4 | 2 | 1.68 |
| 1 | A2_B2 | A1_B2 | tofu | 3.045984 | 4 | 2 | 4.86 |
| 2 | A1_B1 | A2_B1 | oomph | 3.074134 | 5 | 1 | 1.52 |
| 2 | A1_B2 | A2_B1 | speck | 3.011706 | 5 | 1 | 4.46 |
| 2 | A2_B1 | A2_B1 | hyper | 3.208953 | 5 | 2 | 2.00 |
| 2 | A2_B2 | A2_B1 | rabbi | 3.315872 | 5 | 2 | 4.64 |
| 3 | A1_B1 | A1_B1 | worst | 4.915294 | 5 | 1 | 1.54 |
| 3 | A1_B2 | A1_B1 | voice | 4.887075 | 5 | 1 | 4.13 |
| 3 | A2_B1 | A1_B1 | lucky | 5.030973 | 5 | 2 | 1.76 |
| 3 | A2_B2 | A1_B1 | cover | 4.863260 | 5 | 2 | 4.23 |
| 4 | A1_B1 | A2_B2 | suave | 2.910580 | 5 | 1 | 1.48 |
| 4 | A1_B2 | A2_B2 | stoop | 3.045984 | 5 | 1 | 4.63 |
| 4 | A2_B1 | A2_B2 | avail | 2.877579 | 5 | 2 | 1.33 |
| 4 | A2_B2 | A2_B2 | lilac | 3.017955 | 5 | 2 | 4.69 |
| 5 | A1_B1 | A2_B1 | shrewd | 3.244739 | 6 | 1 | 1.92 |
| 5 | A1_B2 | A2_B1 | starch | 3.291232 | 6 | 1 | 4.29 |
| 5 | A2_B1 | A2_B1 | bygone | 3.091935 | 6 | 2 | 1.69 |
| 5 | A2_B2 | A2_B1 | condom | 3.225935 | 6 | 2 | 4.87 |

### Shiny App

The package has an interactive shiny app, which supports most code
functionality, with useful additional features like visualising
distributions and relationships. It’s a friendly front-end to the
package’s functions. A demo version of the LexOPS shiny app is available
online at <https://jackt.shinyapps.io/lexops/>, but it is faster and
more reliable to run it locally, with:

``` r
LexOPS::run_shiny()
```

![](man/figures/shiny-preview.png)

### Matching on Custom Dataframes

As well as the built-in dataframe `LexOPS::lexops`, you can generate
matches from any dataframe object.

Here is an example using `mtcars`. We pick five automatic and five
manual models of car, matched for acceleration (within ±5 `qsec`) and
the number of carburetor barrels (`carb`; exactly).

``` r
mtcars |>
  tibble::as_tibble(rownames = "car_id") |>
  set_options(id_col = "car_id") |>
  split_by(am, 0:0 ~ 1:1) |>
  control_for(qsec, -5:5) |>
  control_for(carb, 0:0) |>
  generate(5)
```

    #>   item_nr                 A1             A2 match_null
    #> 1       1           Merc 280 Ford Pantera L         A2
    #> 2       2           Merc 230     Volvo 142E         A1
    #> 3       3  Hornet Sportabout  Porsche 914-2         A1
    #> 4       4 Cadillac Fleetwood      Mazda RX4         A1
    #> 5       5   Dodge Challenger    Honda Civic         A2
