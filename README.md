
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
  generate(n = 10, match_null = "A1_B2")
```

| A1\_B1     | A1\_B2     | A1\_B3     | A2\_B1     | A2\_B2     | A2\_B3     | match\_null |
| :--------- | :--------- | :--------- | :--------- | :--------- | :--------- | :---------- |
| treachery  | semblance  | dreamland  | kidnapper  | waistline  | lifesaver  | A1\_B2      |
| arrogance  | mortality  | greatness  | orphanage  | propeller  | waterfall  | A1\_B2      |
| fascism    | whatnot    | empathy    | autopsy    | charger    | nightie    | A1\_B2      |
| repression | moderation | politeness | guillotine | tablecloth | waterfront | A1\_B2      |
| revenge    | closure    | bravery    | tsunami    | carrier    | blanket    | A1\_B2      |
| distrust   | validity   | fondness   | swastika   | banister   | erection   | A1\_B2      |
| hardship   | mischief   | tranquil   | attacker   | pussycat   | handmade   | A1\_B2      |
| hypocrisy  | formality  | amazement  | courtroom  | pacemaker  | starlight  | A1\_B2      |
| paranoia   | holiness   | serenity   | wildfire   | scaffold   | macaroni   | A1\_B2      |
| betrayal   | tendency   | optimism   | asbestos   | pavement   | laughter   | A1\_B2      |
