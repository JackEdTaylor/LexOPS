
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LexOPS

<!-- badges: start -->

<!-- badges: end -->

LexOPS is an R package and Shiny App for generating word stimuli, for
use in Psychology experiments. It can generate stimuli for a factorial
design specified by the user, controlling for selected lexical
variables. It also has several features for visualising variables’
distributions and relationships. The app has an inbuilt database of
features for English words, but the user can also provide their own list
of features, for English words and/or for words in other languages.

## Installation

LexOPS can be installed as an R package with:

``` r
devtools::install_github("JackEdTaylor/LexOPS")
```

## Shiny App

The LexOPS Shiny App can be run with:

``` r
LexOPS::run_shiny()
```

## Reproducible Code

Stimuli can also be generated using reproducible code. For example, the
following example pipeline generates 10 words (all nouns) per condition
(60 words in total), for a study with a 2 x 3, concreteness (low, high)
by emotional valence (negative, neutral, positive) experimental design.
Words are controlled for by length exactly, and by word frequency within
a tolerance of ±0.25 Zipf, relative to neutral abstract words.

``` r
library(LexOPS)

stim <- lexops %>%
  subset(PoS.SUBTLEX_UK == "noun") %>%
  split_by(list("CNC.Brysbaert", c(1, 2), c(4, 5))) %>%
  split_by(list("VAL.Warriner", c(1, 3), c(4.5, 5.5), c(7, 9))) %>%
  control_for(list("Zipf.SUBTLEX_UK", c(-0.25, 0.25))) %>%
  control_for("Length") %>%
  generate(n = 5, match_null = "A1_B2")

print(stim)
```

| item\_nr | A1\_B1     | A1\_B2     | A1\_B3     | A2\_B1     | A2\_B2     | A2\_B3     | match\_null |
| -------: | :--------- | :--------- | :--------- | :--------- | :--------- | :--------- | :---------- |
|        1 | treachery  | semblance  | dreamland  | kidnapper  | waistline  | lifesaver  | A1\_B2      |
|        2 | arrogance  | mortality  | greatness  | orphanage  | propeller  | waterfall  | A1\_B2      |
|        3 | fascism    | whatnot    | empathy    | autopsy    | charger    | nightie    | A1\_B2      |
|        4 | repression | moderation | politeness | guillotine | tablecloth | waterfront | A1\_B2      |
|        5 | revenge    | closure    | bravery    | tsunami    | carrier    | blanket    | A1\_B2      |

The generated stimuli can then also be easily converted into long
format, with the `long_format()`
function.

``` r
long_format(stim)
```

| item\_nr | condition | match\_null | string     | Zipf.SUBTLEX\_UK | Length | CNC.Brysbaert | VAL.Warriner |
| -------: | :-------- | :---------- | :--------- | ---------------: | -----: | ------------: | -----------: |
|        1 | A1\_B1    | A1\_B2      | treachery  |         2.778521 |      9 |          1.69 |         2.70 |
|        1 | A1\_B2    | A1\_B2      | semblance  |         2.640218 |      9 |          2.00 |         5.00 |
|        1 | A1\_B3    | A1\_B2      | dreamland  |         2.645126 |      9 |          1.73 |         7.00 |
|        1 | A2\_B1    | A1\_B2      | kidnapper  |         2.668864 |      9 |          4.29 |         1.77 |
|        1 | A2\_B2    | A1\_B2      | waistline  |         2.678007 |      9 |          4.34 |         4.90 |
|        1 | A2\_B3    | A1\_B2      | lifesaver  |         2.752641 |      9 |          4.28 |         7.24 |
|        2 | A1\_B1    | A1\_B2      | arrogance  |         3.387701 |      9 |          1.74 |         2.55 |
|        2 | A1\_B2    | A1\_B2      | mortality  |         3.396439 |      9 |          1.46 |         5.15 |
|        2 | A1\_B3    | A1\_B2      | greatness  |         3.344096 |      9 |          1.69 |         7.76 |
|        2 | A2\_B1    | A1\_B2      | orphanage  |         3.376071 |      9 |          4.65 |         2.95 |
|        2 | A2\_B2    | A1\_B2      | propeller  |         3.259217 |      9 |          4.90 |         4.95 |
|        2 | A2\_B3    | A1\_B2      | waterfall  |         3.564967 |      9 |          4.90 |         7.79 |
|        3 | A1\_B1    | A1\_B2      | fascism    |         3.007490 |      7 |          1.83 |         2.50 |
|        3 | A1\_B2    | A1\_B2      | whatnot    |         3.097136 |      7 |          1.92 |         5.48 |
|        3 | A1\_B3    | A1\_B2      | empathy    |         3.195423 |      7 |          1.63 |         7.29 |
|        3 | A2\_B1    | A1\_B2      | autopsy    |         2.996766 |      7 |          4.29 |         2.62 |
|        3 | A2\_B2    | A1\_B2      | charger    |         3.262762 |      7 |          4.39 |         5.28 |
|        3 | A2\_B3    | A1\_B2      | nightie    |         2.886067 |      7 |          4.30 |         7.11 |
|        4 | A1\_B1    | A1\_B2      | repression |         3.162603 |     10 |          1.66 |         2.81 |
|        4 | A1\_B2    | A1\_B2      | moderation |         2.969894 |     10 |          1.90 |         5.30 |
|        4 | A1\_B3    | A1\_B2      | politeness |         2.756434 |     10 |          1.79 |         7.33 |
|        4 | A2\_B1    | A1\_B2      | guillotine |         3.093676 |     10 |          4.64 |         1.63 |
|        4 | A2\_B2    | A1\_B2      | tablecloth |         2.883256 |     10 |          4.85 |         5.33 |
|        4 | A2\_B3    | A1\_B2      | waterfront |         3.162603 |     10 |          4.67 |         7.50 |
|        5 | A1\_B1    | A1\_B2      | revenge    |         4.016505 |      7 |          1.54 |         2.75 |
|        5 | A1\_B2    | A1\_B2      | closure    |         3.976769 |      7 |          1.78 |         5.21 |
|        5 | A1\_B3    | A1\_B2      | bravery    |         3.834670 |      7 |          1.96 |         7.38 |
|        5 | A2\_B1    | A1\_B2      | tsunami    |         3.793340 |      7 |          4.33 |         2.33 |
|        5 | A2\_B2    | A1\_B2      | carrier    |         3.884383 |      7 |          4.20 |         5.19 |
|        5 | A2\_B3    | A1\_B2      | blanket    |         3.925162 |      7 |          5.00 |         7.05 |
