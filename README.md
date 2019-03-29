# LexOPS

LexOPS is an R Shiny App for generating word stimuli, for use in Psychology experiments. It can generate stimuli for a factorial design specified by the user, controlling for selected lexical variables. It also has several features for visualising variables' distributions and relationships. The app has an inbuilt database of features for English words, but the user can also provide their own list of features, for English words and/or for words in other languages.

## Run the Shiny App

There are two main options for running LexOPS.

### 1. Run Locally (Recommended)

This is the recommended method for running LexOPS. If you [have R installed](https://cloud.r-project.org/), you can run the latest version of LexOPS on your own computer with the following R commands:

``` r
install.packages("shiny")
shiny::runGitHub("JackEdTaylor/LexOPS")
```

### 2. Run on the LexOPS Server

If you can't do any of the above, the next easiest way to run LexOPS is as a web app (hosted on a dedicated server). This will probably be much slower than running LexOPS on your own computer, but also means you won't have to install anything. LexOPS is available online at:

[http://lexops.co.uk/run](http://lexops.co.uk/run)
