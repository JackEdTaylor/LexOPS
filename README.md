# LexOPS
LexOPS is an R Shiny App for generating word stimuli, for use in Psychology experiments. It can generate stimuli for a factorial design specified by the user, controlling for selected lexical variables. The app has an inbuilt database of features for English words, but the user can also provide their own list of features, for other languages.

## Run the Shiny App

The app can be run in several ways...

### 1) Run in own R Session

This way is a fast and easy way to run LexOPS from your own computer. The following R commands will install the required packages and then run the latest version of the app.

```
install.packages("shiny")
shiny::runGitHub("JackEdTaylor/LexOPS")
```

Note that the first time you run this, R may install or update dependencies.

### 2) Clone the Repository

Just clone this GitHub repository, set the working directory to the directory containing the `ui.r` and `server.r` scripts, then in R, run:

```
shiny::runApp()
```

Similarly, this may automatically update dependencies.

### 3) Run on the LexOPS Server

The easiest way to run the app if you can't do any of the above is to go to:

[http://lexops.co.uk/run](http://lexops.co.uk/run)

Here, the app is available as a web app, run on a dedicated server. This will be slower, but means you won't have to install anything.
