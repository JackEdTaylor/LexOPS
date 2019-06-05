
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
lexops %>%
  subset(PoS.SUBTLEX_UK == "noun") %>%
  split_by(list("CNC.Brysbaert", c(1, 2), c(4, 5))) %>%
  split_by(list("VAL.Warriner", c(1, 3), c(4.5, 5.5), c(7, 9))) %>%
  control_for(list("Zipf.SUBTLEX_UK", c(-0.25, 0.25))) %>%
  control_for("Length") %>%
  generate(n = 10, match_null = "A2_B2")
```

| A1\_B1    | A1\_B2    | A1\_B3    | A2\_B1    | A2\_B2    | A2\_B3    | match\_null |
| :-------- | :-------- | :-------- | :-------- | :-------- | :-------- | :---------- |
| fascism   | sarcasm   | empathy   | blister   | sawdust   | tropics   | A2\_B2      |
| treachery | formality | amazement | cockroach | wolverine | blueberry | A2\_B2      |
| betrayal  | tendency  | intimacy  | intruder  | radiator  | lemonade  | A2\_B2      |
| revenge   | bonkers   | fantasy   | coroner   | venison   | massage   | A2\_B2      |
| inability | obscurity | sincerity | frostbite | parchment | speedboat | A2\_B2      |
| hardship  | protocol  | kindness  | asbestos  | employee  | doughnut  | A2\_B2      |
| mistake   | version   | ability   | traffic   | officer   | husband   | A2\_B2      |
| jealousy  | suspense  | tranquil  | incision  | preacher  | lollipop  | A2\_B2      |
| sadness   | quantum   | clarity   | tsunami   | laundry   | popcorn   | A2\_B2      |
| loathing  | recourse  | fondness  | wildfire  | ointment  | duckling  | A2\_B2      |
