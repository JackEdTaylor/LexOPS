
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
following example pipeline generates 20 words (all nouns) per condition
(120 words in total), for a study with a 2 x 3, concreteness (low, high)
by emotional valence (negative, neutral, positive) experimental design.
Words are controlled for by length exactly, and by word frequency within
a tolerance of ±0.2 Zipf, relative to neutral abstract words.

``` r
lexops %>%
  subset(PoS.SUBTLEX_UK == "noun") %>%
  split_by(list("CNC.Brysbaert", c(1, 2), c(4, 5))) %>%
  split_by(list("VAL.Warriner", c(1, 3), c(4.5, 5.5), c(7, 9))) %>%
  control_for(list("Zipf.SUBTLEX_UK", c(-0.25, 0.25))) %>%
  control_for("Length") %>%
  generate(n = 20, match_null = "A2_B2")
```

| A1\_B1    | A1\_B2    | A1\_B3    | A2\_B1    | A2\_B2    | A2\_B3    | match\_null |
| :-------- | :-------- | :-------- | :-------- | :-------- | :-------- | :---------- |
| ignorance | mentality | greatness | courtroom | publisher | blueberry | A2\_B2      |
| hatred    | motive    | wisdom    | bomber    | buzzer    | pillow    | A2\_B2      |
| sadness   | analogy   | clarity   | burglar   | bedding   | pumpkin   | A2\_B2      |
| distrust  | audacity  | fondness  | gangrene  | scaffold  | beverage  | A2\_B2      |
| boredom   | paradox   | empathy   | asshole   | pageant   | pianist   | A2\_B2      |
| betrayal  | protocol  | kindness  | assassin  | pussycat  | doughnut  | A2\_B2      |
| paranoia  | theology  | optimist  | dictator  | fragment  | doughnut  | A2\_B2      |
| distrust  | subtlety  | serenity  | gangrene  | scaffold  | duckling  | A2\_B2      |
| fascism   | whatnot   | empathy   | garbage   | foreman   | mermaid   | A2\_B2      |
| madness   | closure   | bravery   | missile   | carrier   | popcorn   | A2\_B2      |
| treachery | euphemism | amazement | kidnapper | margarine | snowflake | A2\_B2      |
| betrayal  | protocol  | optimism  | asbestos  | backside  | lollipop  | A2\_B2      |
| boredom   | quantum   | empathy   | hostage   | bracket   | blossom   | A2\_B2      |
| jealousy  | mischief  | kindness  | asbestos  | pipeline  | lollipop  | A2\_B2      |
| madness   | concept   | bravery   | tsunami   | terrace   | concert   | A2\_B2      |
| revenge   | bonkers   | fantasy   | coroner   | plaster   | blanket   | A2\_B2      |
| treachery | sociology | dreamland | cellulite | bystander | lifesaver | A2\_B2      |
| boredom   | concise   | empathy   | monsoon   | mascara   | tropics   | A2\_B2      |
| hatred    | motive    | wisdom    | litter    | tenant    | cookie    | A2\_B2      |
| inability | euphemism | sincerity | excrement | guerrilla | starlight | A2\_B2      |
