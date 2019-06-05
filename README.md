
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
following code generates 20 words per condition (120 in total), for a
study with a concreteness (low, high) by emotional valence (negative,
neutral, positive) experimental design. Words are controlled for by
length exactly, and by word frequency within a tolerance of ±0.2 Zipf.

``` r
lexops %>%
  split_by(list("CNC.Brysbaert", c(1, 2), c(4, 5))) %>%
  split_by(list("VAL.Warriner", c(1, 3), c(4.5, 5.5), c(7, 9))) %>%
  control_for(list("Zipf.SUBTLEX_UK", c(-0.25, 0.25))) %>%
  control_for("Length") %>%
  generate(n = 20, match_null = "A2_B2")
```

| A1\_B1     | A1\_B2     | A1\_B3     | A2\_B1     | A2\_B2     | A2\_B3     | match\_null |
| :--------- | :--------- | :--------- | :--------- | :--------- | :--------- | :---------- |
| insane     | slight     | wisdom     | litter     | ladder     | muffin     | A2\_B2      |
| hopeless   | conclude   | thankful   | asbestos   | morphine   | laughter   | A2\_B2      |
| fascism    | spatial    | optimum    | cyanide    | leotard    | tropics    | A2\_B2      |
| unromantic | indisposed | hospitable | pickpocket | sauerkraut | lovemaking | A2\_B2      |
| stupid     | manage     | superb     | prison     | tackle     | rabbit     | A2\_B2      |
| paranoia   | probable   | carefree   | incision   | gauntlet   | erection   | A2\_B2      |
| illicit    | sarcasm    | empathy    | cyanide    | catfish    | tropics    | A2\_B2      |
| hectic     | endure     | heroic     | heroin     | stitch     | kitten     | A2\_B2      |
| unfit      | hyper      | amaze      | filth      | hinge      | brook      | A2\_B2      |
| unromantic | submissive | hospitable | chickenpox | kickboxing | lovemaking | A2\_B2      |
| ashamed    | ongoing    | liberty    | crushed    | cabbage    | rainbow    | A2\_B2      |
| greed      | macho      | bliss      | venom      | dwarf      | doggy      | A2\_B2      |
| negative   | convince   | accurate   | prisoner   | trousers   | comedian   | A2\_B2      |
| crappy     | untold     | excite     | rapist     | crease     | stereo     | A2\_B2      |
| annoy      | alpha      | bliss      | trash      | forge      | fudge      | A2\_B2      |
| guilt      | alpha      | bliss      | thief      | sperm      | mommy      | A2\_B2      |
| loathing   | holiness   | carefree   | incision   | ballpark   | macaroni   | A2\_B2      |
| annoy      | heady      | amaze      | stain      | forty      | tulip      | A2\_B2      |
| stupid     | reduce     | unique     | prison     | county     | silver     | A2\_B2      |
| insolent   | gumption   | wellness   | glaucoma   | payphone   | paycheck   | A2\_B2      |
