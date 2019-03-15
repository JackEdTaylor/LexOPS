# LexOPS
LexOPS is an R Shiny App for generating word stimuli, for use in Psychology experiments. It can generate stimuli for a factorial design specified by the user, controlling for selected lexical variables. It also has several features for visualising variables' distributions and relationships. The app has an inbuilt database of features for English words, but the user can also provide their own list of features, for other languages.

## Run the Shiny App

There are two main options for running LexOPS.

### 1. Run in own R Session

This is the recommended method for running LexOPS. If you [have R installed](https://cloud.r-project.org/), you can run LexOPS on your own computer by either (a) using the `shiny` function, `runGitHub()`, or by (b) cloning the repository. Note that these methods may automatically install or update some of the app's dependencies.

#### a) Using `shiny::runGitHub()`

This is a fast and easy way to run LexOPS from your own computer. Open R and run the following R commands to install `shiny` and then run the latest version of the app.

```
install.packages("shiny")
shiny::runGitHub("JackEdTaylor/LexOPS")
```

#### b) Cloning the Repository

If you don't want to have to download LexOPS each time you use it, just [clone this GitHub repository](https://help.github.com/en/articles/cloning-a-repository), then [set the working directory](http://rfunction.com/archives/1001) to the directory containing the `ui.r` and `server.r` scripts, and in R, run:

```
install.packages("shiny")
shiny::runApp()
```

### 2. Run on the LexOPS Server

If you can't do any of the above, the easiest way to run LexOPS is as a web app (hosted on a dedicated server). This will probably be much slower than running LexOPS on your own computer, but also means you won't have to install anything. LexOPS is available online at:

[http://lexops.co.uk/run](http://lexops.co.uk/run)
