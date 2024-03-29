% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_item.R
\name{match_item}
\alias{match_item}
\title{Get suitable matches for a single item on one or several dimensions.}
\usage{
match_item(
  df = LexOPS::lexops,
  target,
  ...,
  id_col = "string",
  filter = TRUE,
  standard_eval = FALSE
)
}
\arguments{
\item{df}{A data frame to reorder, containing the target string (default = LexOPS::lexops).}

\item{target}{The target string}

\item{...}{Should specify the variables and tolerances in the form \verb{Length = 0:0, Zipf.SUBTLEX_UK = -0.1:0.1, PoS.SUBTLEX_UK}. Numeric variables can include tolerances (as elements 2:3 of a vector). Numeric variables with no tolerances will be matched exactly.}

\item{id_col}{A character vector specifying the column identifying unique observations (e.g. in \code{LexOPS::lexops}, the \code{id_col} is \code{"string"}).}

\item{filter}{Logical. If TRUE, matches outside the tolerances specified in vars are removed. If FALSE, a new column, matchFilter is calculated indicating whether or not the string is within all variables' tolerances. (Default = TRUE.)}

\item{standard_eval}{Logical; bypasses non-standard evaluation, and allows more standard R object of list. If \code{TRUE}, \code{...} should be a single list specifying the variables to match by and their tolerances, in the form \code{list("numericVariable1Name", c("numericVariable2Name", -1.5, 3), "characterVariableName")}. Default = \code{FALSE}.}
}
\value{
Returns data frame based on \code{df}. If \code{filter} == TRUE, will only contain matches. If \code{filter} == FALSE, will be the original \code{df} object, with a new column, "matchFilter".
}
\description{
Suggests items that are suitable matches for a target item, based on selected variables of a data frame. Note that unlike functions in the generate pipeline (e.g. \code{control_for()}), multiple variables' tolerances can be defined in one function.
}
\examples{

# Match by number of syllables exactly
lexops |>
  match_item("thicket", Syllables.CMU)

# Match by number of syllables exactly, but keep all entries in the original dataframe
lexops |>
  match_item("thicket", Syllables.CMU, filter = FALSE)

# Match by number of syllables exactly, and rhyme
lexops |>
  match_item("thicket", Syllables.CMU, Rhyme.CMU)

# Match by length exactly, and closely by frequency (within 0.2 Zipf either way)
lexops |>
  match_item("thicket", Length, Zipf.SUBTLEX_UK = -0.2:0.2)

# The syntax makes matching by multiple variables easiy and readable
lexops |>
  match_item(
    "elephant",
    BG.SUBTLEX_UK = -0.005:0.005,
    Length = 0:0,
    Zipf.SUBTLEX_UK = -0.1:0.1,
    PoS.SUBTLEX_UK,
    RT.ELP = -10:10
  )

# Match using standard evaluation
lexops |>
  match_item("thicket", list("Length", c("Zipf.SUBTLEX_UK", -0.2, 0.2)), standard_eval = TRUE)

# Find matches within an orthographic levenshtein distance of 5 from "thicket":
library(dplyr)
library(stringdist)
targ_word <- "thicket"
lexops |>
  mutate(old = stringdist(targ_word, string, method="lv")) |>
  match_item(targ_word, old = 0:5)

# Find matches within a phonological levenshtein distance of 2 from "thicket":
# (note that this method requires 1-letter phonological transcriptions)
library(dplyr)
library(stringdist)
targ_word <- "thicket"
targ_word_pronun <- lexops |>
  filter(string == "thicket") |>
  pull(eSpeak.br_1letter)
lexops |>
  mutate(pld = stringdist(targ_word_pronun, eSpeak.br_1letter, method="lv")) |>
  match_item(targ_word, pld = 0:2)

}
\seealso{
\code{\link{lexops}} for the default data frame and associated variables.
}
