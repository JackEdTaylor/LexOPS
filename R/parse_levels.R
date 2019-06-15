#' A non-standard evaluation parser
#'
#' This is just a handy function I use for parsing two non-standard variables, a column name and associated tolerances/levels, into a single list object.
#'
#' @param var A column name (non-standard evaluation).
#' @param levels Levels in the form of tilde-separated vectors (e.g. `c("noun", "name") ~ c("verb", "adjective")`) or character vectors (e.g. `"noun" ~ "verb"`). Numeric levels can be in the form `1.5:2.75 ~ 3.5:4.75`.
#'
#' @return Returns a list object in the form `list("column_name", c(1, 2), c(3, 4), c(5, 6))`.
#' @examples
#'
#' # Numeric sections of a distribution can be specified as lower:upper bound...
#'
#' parse_levels(substitute(Zipf.SUBTLEX_UK), substitute(1:2 ~ 3:4 ~ 5:6))
#'
#' # ...or c(lower, upper)
#'
#' parse_levels(substitute(Zipf.SUBTLEX_UK), substitute(c(1, 2) ~ c(3, 4) ~ c(5, 6)))
#'
#' # Non-numeric categories can be specified like so
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK), substitute("noun" ~ "verb"))
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK), substitute(c("noun", "name") ~ c("adjective", "verb")))
#'
#' # Can give no value for `levels` (i.e. `levels` is `NA`),
#' # e.g. when matching exactly, or categorically
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK))
#'
#' @export

parse_levels <- function(var, levels = NA) {
  var <- deparse(var)

  levels <- deparse(levels) %>%
    strsplit("~", fixed = TRUE) %>%
    dplyr::first() %>%
    { gsub(" ", "", .) }

  if (all(grepl(":", levels))) {
    levels <- strsplit(levels, ":", fixed = TRUE) %>%
      lapply(as.numeric)
  } else {
    levels <- lapply(levels, function(l) eval(parse(text = l)) )
  }

  out <- if (all(is.na(levels))) {
    list(var)
  } else {
    rlang::prepend(levels, var)
  }

  out
}
