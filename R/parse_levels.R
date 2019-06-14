#' A non-standard evaluation parser
#'
#' This is just a handy function I use for parsing two non-standard variables into a single list object.
#'
#' @param var A column name (non-standard evaluation).
#' @param levels Levels in the form of tilde-separated vectors (e.g. `c(1, 2) ~ c(3, 4) ~ c(5, 6)`) or character vectors (e.g. `"noun" ~ "verb"`).
#'
#' @return Returns a list object in the form `list("column_name", c(1, 2), c(3, 4), c(5, 6))`.
#' @examples
#'
#' parse_levels(substitute(Zipf.SUBTLEX_UK), substitute(c(1, 2) ~ c(3, 4) ~ c(5, 6)))
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK), substitute("noun" ~ "verb"))
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK), substitute(c("noun", "name") ~ c("adjective", "verb")))
#'
#' parse_levels(substitute(PoS.SUBTLEX_UK))
#'
#' @export

parse_levels <- function(var, levels = NA) {
  var <- deparse(var)

  levels <- deparse(levels) %>%
    strsplit("~", fixed = TRUE) %>%
    dplyr::first() %>%
    { gsub(" ", "", .) } %>%
    lapply(function(l) eval(parse(text = l)) )

  out <- if (all(is.na(levels))) {
    list(var)
  } else {
    rlang::prepend(levels, var)
  }

  out
}
