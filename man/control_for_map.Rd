% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/control_for_map.R
\name{control_for_map}
\alias{control_for_map}
\title{Control for confounding variables via a function.}
\usage{
control_for_map(x, fun, var, tol = NA, name = NA, standard_eval = FALSE, ...)
}
\arguments{
\item{x}{A data frame containing the IV and strings, or a LexOPS_pipeline object resulting from one of \code{split_by()}, \code{control_for()}, etc..}

\item{fun}{The function to use to calculate the control varibale. Should be an object of class "function".}

\item{var}{The column to provide the value which will be the first argument of the function.}

\item{tol}{The tolerance of the control. For numeric variables, this should be in the form lower:upper (e.g. \code{-0.1:0.1} will control within +/- 0.1). For categorical variables, this can be kept as \code{NA}.}

\item{name}{What the output column should be named. If \code{NA} (default), will automatically assign as \code{sprintf("control_fun_\%i", nr)}, where \code{nr} is the number of the control function.}

\item{standard_eval}{Logical; bypasses non-standard evaluation, and allows more standard R objects in \code{var} and \code{tol}. If \code{TRUE}, \code{var} should be a character vector referring to a column in \code{df} (e.g. \code{"Zipf.SUBTLEX_UK"}), and \code{tol} should be a vector of length 2, specifying the tolerance (e.g. \code{c(-0.1, 0.5)}). Default = \code{FALSE}.}

\item{...}{Arguments to be passed to \code{fun}}
}
\value{
Returns \code{df}, with details on the variables to be controlled for added to the attributes. Run the \code{\link{generate}} function to then generate the actual stimuli.
}
\description{
Some variables (e.g. similarity measures) are hard to control for with \code{\link{control_for}} as they need to be recalculated for each word relative to each other, rather than there being a single value for each possible stimulus. The \code{control_for_map} function declares a function that the \code{\link{generate}} function should apply to each match_null within an iteration of stimulus generation. The function given as \code{fun} should be able to take the data in the column given in \code{var} as the first argument, and should be able to take the match_null's value in that column as the second argument.
}
\examples{

# Create two levels of arousal, controlling for orthographic similarity
# (as optimal string alignment; default for `stringdist()`)
library(stringdist)
lexops |>
 split_by(AROU.Warriner, 1:3 ~ 7:9) |>
 control_for_map(stringdist, string, 0:4)

# Create two levels of arousal, controlling for orthographic Levenshtein distance
# (passed via `method` argument to `stringdist()`)
library(stringdist)
lexops |>
 split_by(AROU.Warriner, 1:3 ~ 7:9) |>
 control_for_map(stringdist, string, 0:4, method="lv")

# Create two levels of arousal, controlling for phonological Levenshtein distance
library(stringdist)
lexops |>
 split_by(AROU.Warriner, 1:3 ~ 7:9) |>
 control_for_map(stringdist, eSpeak.br_1letter, 0:2, method="lv")

# Bypass non-standard evaluation
library(stringdist)
lexops |>
 split_by(AROU.Warriner, 1:3 ~ 7:9) |>
 control_for_map(stringdist, "eSpeak.br_1letter", c(0, 2), standard_eval=TRUE, method="lv")

}
