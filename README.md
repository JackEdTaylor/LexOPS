
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LexOPS <img src="man/figures/hex.png" align="right" style="padding-left:10px;background-color:white" />

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![Version: 0.3.0](https://img.shields.io/badge/version-0.3.0-blue.svg)](https://github.com/JackEdTaylor/LexOPS/releases)
[![DOI: 10.3758/s13428-020-01389-1](https://zenodo.org/badge/DOI/10.3758/s13428-020-01389-1.svg)](https://doi.org/10.3758/s13428-020-01389-1)
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

:mortar\_board: Paper about the package: [Taylor, Beith, and Sereno
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

stim <- lexops %>%
  split_by(Syllables.CMU, 1:1 ~ 2:2) %>%
  split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
  control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
  control_for(Length) %>%
  generate(n = 50)
```

    #> Generated 50/50 (100%). 157 total iterations, 0.32 success rate.

A preview of what was generated:

``` r
# create a table of the first 5 rows of the output
stim %>%
  head(5) %>%
  knitr::kable()
```

| item\_nr | A1\_B1 | A1\_B2 | A2\_B1 | A2\_B2 | match\_null |
| -------: | :----- | :----- | :----- | :----- | :---------- |
|        1 | fresh  | frame  | basis  | river  | A1\_B2      |
|        2 | gist   | cuff   | akin   | tuba   | A2\_B1      |
|        3 | fate   | slip   | rely   | lily   | A1\_B1      |
|        4 | shrewd | wrench | equate | muzzle | A2\_B2      |
|        5 | famed  | reins  | ethic  | totem  | A2\_B1      |

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
long_format(stim) %>%
  head(20) %>%
  knitr::kable()
```

| item\_nr | condition | match\_null | string | Zipf.SUBTLEX\_UK | Length | Syllables.CMU | CNC.Brysbaert |
| -------: | :-------- | :---------- | :----- | ---------------: | -----: | ------------: | ------------: |
|        1 | A1\_B1    | A1\_B2      | fresh  |         4.893319 |      5 |             1 |          1.97 |
|        1 | A1\_B2    | A1\_B2      | frame  |         4.755413 |      5 |             1 |          4.30 |
|        1 | A2\_B1    | A1\_B2      | basis  |         4.625308 |      5 |             2 |          1.83 |
|        1 | A2\_B2    | A1\_B2      | river  |         4.926899 |      5 |             2 |          4.89 |
|        2 | A1\_B1    | A2\_B1      | gist   |         2.974489 |      4 |             1 |          1.81 |
|        2 | A1\_B2    | A2\_B1      | cuff   |         3.272077 |      4 |             1 |          4.61 |
|        2 | A2\_B1    | A2\_B1      | akin   |         3.083126 |      4 |             2 |          1.71 |
|        2 | A2\_B2    | A2\_B1      | tuba   |         2.951008 |      4 |             2 |          4.86 |
|        3 | A1\_B1    | A1\_B1      | fate   |         4.224395 |      4 |             1 |          1.53 |
|        3 | A1\_B2    | A1\_B1      | slip   |         4.332324 |      4 |             1 |          4.10 |
|        3 | A2\_B1    | A1\_B1      | rely   |         4.318882 |      4 |             2 |          1.93 |
|        3 | A2\_B2    | A1\_B1      | lily   |         4.253363 |      4 |             2 |          4.69 |
|        4 | A1\_B1    | A2\_B2      | shrewd |         3.244739 |      6 |             1 |          1.92 |
|        4 | A1\_B2    | A2\_B2      | wrench |         3.150581 |      6 |             1 |          4.93 |
|        4 | A2\_B1    | A2\_B2      | equate |         2.976769 |      6 |             2 |          1.93 |
|        4 | A2\_B2    | A2\_B2      | muzzle |         3.066804 |      6 |             2 |          4.59 |
|        5 | A1\_B1    | A2\_B1      | famed  |         3.415895 |      5 |             1 |          1.81 |
|        5 | A1\_B2    | A2\_B1      | reins  |         3.327180 |      5 |             1 |          4.56 |
|        5 | A2\_B1    | A2\_B1      | ethic  |         3.303191 |      5 |             2 |          1.59 |
|        5 | A2\_B2    | A2\_B1      | totem  |         3.239804 |      5 |             2 |          4.00 |

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
