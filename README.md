
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LexOPS

<!-- badges: start -->

<!-- badges: end -->

LexOPS is an R package for generating word stimuli, for use in
Psychology experiments. It can generate stimuli for a factorial design
specified by the user, controlling for selected lexical variables. The
package has an inbuilt database of features for English words
(`LexOPS::lexops`), but the user can also use their own list of
features, for English words and/or for words in other languages.

## Installation

LexOPS can be installed as an R package with:

``` r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("JackEdTaylor/LexOPS")
```

## Shiny App

The package features an interactive shiny app, with several useful
features for visualising variables’ distributions and relationships
while generating stimuli. The LexOPS shiny app is available online at
<https://jackt.shinyapps.io/lexops/>, but it is usually faster and more
relilable to run it locally, with:

``` r
LexOPS::run_shiny()
```

## Generating Stimuli

Stimuli can also be generated using reproducible code. For example, the
following example pipeline generates 5 words (all nouns) per condition
(30 words in total), for a study with a 2 x 3, concreteness (low, high)
by emotional valence (negative, neutral, positive) experimental design.
Words are controlled for by length exactly, and by word frequency within
a tolerance of ±0.25 Zipf, relative to neutral abstract words.

``` r
library(LexOPS)

stim <- lexops %>%
  subset(PoS.SUBTLEX_UK == "noun") %>%
  split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
  split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
  control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
  control_for(Length) %>%
  generate(n = 5, match_null = "A1_B2")

print(stim)
```

| item\_nr | A1\_B1    | A1\_B2    | A1\_B3    | A2\_B1    | A2\_B2    | A2\_B3    | match\_null |
| -------: | :-------- | :-------- | :-------- | :-------- | :-------- | :-------- | :---------- |
|        1 | loathing  | dominion  | epiphany  | catheter  | forklift  | daybreak  | A1\_B2      |
|        2 | hardship  | mischief  | tranquil  | dictator  | pendulum  | smoothie  | A1\_B2      |
|        3 | betrayal  | protocol  | kindness  | smallpox  | textbook  | doughnut  | A1\_B2      |
|        4 | fascism   | paradox   | empathy   | measles   | lacquer   | sunrise   | A1\_B2      |
|        5 | ignorance | precedent | greatness | courtroom | columnist | milkshake | A1\_B2      |

The generated stimuli can then also be easily converted into long
format, with the `long_format()`
function.

``` r
long_format(stim)
```

| item\_nr | condition | match\_null | string    | Zipf.SUBTLEX\_UK | Length | CNC.Brysbaert | VAL.Warriner |
| -------: | :-------- | :---------- | :-------- | ---------------: | -----: | ------------: | -----------: |
|        1 | A1\_B1    | A1\_B2      | loathing  |         2.865997 |      8 |          1.89 |         2.42 |
|        1 | A1\_B2    | A1\_B2      | dominion  |         2.716925 |      8 |          1.96 |         4.62 |
|        1 | A1\_B3    | A1\_B2      | epiphany  |         2.806325 |      8 |          1.60 |         7.06 |
|        1 | A2\_B1    | A1\_B2      | catheter  |         2.789157 |      8 |          4.48 |         2.84 |
|        1 | A2\_B2    | A1\_B2      | forklift  |         2.888860 |      8 |          4.79 |         4.74 |
|        1 | A2\_B3    | A1\_B2      | daybreak  |         2.926185 |      8 |          4.21 |         7.16 |
|        2 | A1\_B1    | A1\_B2      | hardship  |         3.521810 |      8 |          1.79 |         2.80 |
|        2 | A1\_B2    | A1\_B2      | mischief  |         3.483196 |      8 |          1.90 |         4.78 |
|        2 | A1\_B3    | A1\_B2      | tranquil  |         3.443924 |      8 |          1.90 |         7.11 |
|        2 | A2\_B1    | A1\_B2      | dictator  |         3.510649 |      8 |          4.29 |         2.77 |
|        2 | A2\_B2    | A1\_B2      | pendulum  |         3.303191 |      8 |          4.69 |         5.17 |
|        2 | A2\_B3    | A1\_B2      | smoothie  |         3.273227 |      8 |          4.62 |         7.89 |
|        3 | A1\_B1    | A1\_B2      | betrayal  |         3.437675 |      8 |          1.76 |         2.24 |
|        3 | A1\_B2    | A1\_B2      | protocol  |         3.513962 |      8 |          1.97 |         5.10 |
|        3 | A1\_B3    | A1\_B2      | kindness  |         3.557867 |      8 |          1.74 |         7.65 |
|        3 | A2\_B1    | A1\_B2      | smallpox  |         3.266279 |      8 |          4.25 |         2.02 |
|        3 | A2\_B2    | A1\_B2      | textbook  |         3.383265 |      8 |          4.86 |         5.00 |
|        3 | A2\_B3    | A1\_B2      | doughnut  |         3.500556 |      8 |          4.96 |         7.50 |
|        4 | A1\_B1    | A1\_B2      | fascism   |         3.007490 |      7 |          1.83 |         2.50 |
|        4 | A1\_B2    | A1\_B2      | paradox   |         3.242278 |      7 |          1.54 |         5.40 |
|        4 | A1\_B3    | A1\_B2      | empathy   |         3.195423 |      7 |          1.63 |         7.29 |
|        4 | A2\_B1    | A1\_B2      | measles   |         3.123871 |      7 |          4.69 |         2.57 |
|        4 | A2\_B2    | A1\_B2      | lacquer   |         3.055571 |      7 |          4.28 |         4.95 |
|        4 | A2\_B3    | A1\_B2      | sunrise   |         3.385045 |      7 |          4.69 |         7.35 |
|        5 | A1\_B1    | A1\_B2      | ignorance |         3.483904 |      9 |          1.60 |         2.84 |
|        5 | A1\_B2    | A1\_B2      | precedent |         3.414237 |      9 |          1.63 |         5.25 |
|        5 | A1\_B3    | A1\_B2      | greatness |         3.344096 |      9 |          1.69 |         7.76 |
|        5 | A2\_B1    | A1\_B2      | courtroom |         3.324125 |      9 |          4.63 |         2.84 |
|        5 | A2\_B2    | A1\_B2      | columnist |         3.267445 |      9 |          4.14 |         5.47 |
|        5 | A2\_B3    | A1\_B2      | milkshake |         3.283447 |      9 |          4.97 |         7.26 |
