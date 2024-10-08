% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_design.R
\name{plot_design}
\alias{plot_design}
\title{Plot the generated stimuli's design}
\usage{
plot_design(
  df,
  include = "design",
  dodge_width = 0.1,
  point_size = 0.5,
  line_width = 1,
  force = TRUE,
  id_col = "string"
)
}
\arguments{
\item{df}{Output from \code{generate()} or \code{long_format()}}

\item{include}{A character vector indicating which variables to include in the plot. This can be those specified by \code{split_by()} and \code{control_for()} (\code{"design"}), only those specified in \code{split_by()} (\code{"splits"}), or only those specified by \code{control_for()} (\code{"controls"}). Alternatively, this can be a character vector of the variables that should be plotted, that were in the original dataframe. Default is \code{"design"}.}

\item{dodge_width}{The width to give to \code{ggplot2::position_dodge} (default is 0.2)}

\item{point_size}{Size of points (default = 0.75)}

\item{line_width}{Thickness of lines (default = 1)}

\item{force}{Logical, should the function be forced to try and work if attributes are missing (default is \code{TRUE})? If \code{TRUE}, will expect the dataframe to have a structure similar to that produced by \code{long_format()}, where \code{condition} is character or factor, and \code{item_nr} is numeric or factor. Other variables will be plot-able if given to the \code{include} argument.}

\item{id_col}{A character vector specifying the column identifying unique observations (e.g. in \code{LexOPS::lexops}, the \code{id_col} is \code{"string"}). Ignored unless \code{force=TRUE}, in which case it is required.}
}
\value{
A ggplot object showing how conditions differ in independent variables, and are matched for in controls.
}
\description{
Takes the output from \code{generate()} or \code{long_format()}, and plots conditions' distributions on numeric variables used in the generate pipeline (i.e. indepdent variables, controls). Alternatively, distributions of any numeric variables in the original dataframe can be queried.
}
\examples{

stim <- lexops |>
  dplyr::filter(PK.Brysbaert >= .75) |>
  split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
  split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
  control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
  control_for(Length) |>
  generate(n = 50, match_null = "balanced")
plot_design(stim)

}
