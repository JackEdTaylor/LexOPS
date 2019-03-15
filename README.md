# LexOPS
LexOPS is an R Shiny App for generating word stimuli, for use in Psychology experiments. It can generate stimuli for a factorial design specified by the user, controlling for selected lexical variables. The app has an inbuilt database of features for English words, but the user can also provide their own list of features, for other languages.

## Run the Shiny App

There are two main options for running LexOPS.

### 1) Run in own R Session

This can be done by either using shiny command, `runGitHub()`, or by cloning the repository.

#### Using `shiny::runGitHub()`

This way is a fast and easy way to run LexOPS from your own computer. Open R and run the following R commands to install `shiny` and then run the latest version of the app.

```
install.packages("shiny")
shiny::runGitHub("JackEdTaylor/LexOPS")
```

#### Cloning the Repository

Just clone this GitHub repository, set the working directory to the directory containing the `ui.r` and `server.r` scripts, then in R, run:

```
shiny::runApp()
```

Similarly, this may automatically update dependencies.

### 2) Run on the LexOPS Server

The easiest way to run the app if you can't do any of the above is to go to:

[http://lexops.co.uk/run](http://lexops.co.uk/run)

Here, the app is available as a web app, run on a dedicated server. This will probably be much slower than running LexOPS on your own computer, but also means you won't have to install anything.
