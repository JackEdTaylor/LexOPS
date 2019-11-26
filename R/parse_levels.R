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
#' # Can give vector of variables in non-standard eval
#'
#' parse_levels(substitute(c(Zipf.SUBTLEX_UK, Length)), substitute(0:2.5))
#' parse_levels(substitute(c(Zipf.SUBTLEX_UK, Length)))
#'
#' @export

parse_levels <- function(var, levels = NA) {
  var <- deparse(var) %>%
    paste0(collapse = "") %>%
    parse.unvectorise()

  levels <- deparse(levels) %>%
    paste0(collapse = "") %>%
    strsplit("~", fixed = TRUE) %>%
    dplyr::first() %>%
    gsub(" ", "", .)

  if (all(grepl(":", levels))) {
    levels <- strsplit(levels, ":", fixed = TRUE) %>%
      lapply(as.numeric)
  } else {
    levels <- lapply(levels, function(l) eval(parse(text = l)) )
  }

  out <- if (all(is.na(levels))) {
    list(var)
  } else {
    prepend_list(levels, var)
  }

  out
}

#' A non-standard evaluation parser for elipses
#'
#' This is a version of `parse_levels()` that supports elipses. This is useful for specifying multiple parameters in one function. This function was specifically designed for a non-standard evaluation update to `match_word()`.
#'
#' @param ... Variables and tolerances, in the form `num_variable1 ~ -1:3, num_variable2 ~ -0.5:0.5, char_variable1`. Variables and tolerances should be separated by a `~`, and lower and upper boundaries of tolerances should be separated by `:`. Variables without tolerances are also supported (useful for character variables or matching by numeric variables exactly, i.e. shorthand for `0:0`).
#'
#' @return Returns a list object in the form `list(c("num_variable1", -1, 3), c(num_variable2, -0.5, 0.5), char_variable1)`.
#' @examples
#'
#' parse_elipsis(substitute(c(Length ~ 0:0, Zipf.SUBTLEX_UK ~ -0.1:0.1, PoS.SUBTLEX_UK)))
#'
#' parse_elipsis(substitute(c(Length ~ 0:0,
#'                            Zipf.SUBTLEX_UK ~ -0.1:0.1,
#'                            PoS.SUBTLEX_UK,
#'                            BG.SUBTLEX_UK ~ -0.005:0.005)))
#'
#' @export

parse_elipsis <- function(...) {
  deparse(...) %>%
    paste0(collapse = "") %>%
    lapply(parse.unvectorise) %>%
    lapply(strsplit, "~", fixed = TRUE) %>%
    dplyr::first() %>%
    lapply(function(x) {
      # remove spaces
      x <- x %>%
        lapply(function(x_x) gsub(" ", "", x_x)) %>%
        unlist()
      # get var and (optionally) levels for this section
      var <- x[1]
      if (length(x == 2) & !is.na(x[2])) {
        if (all(grepl(":", x[2]))) {
          levels <- strsplit(x[2], ":", fixed = TRUE) %>%
            lapply(as.numeric) %>%
            unlist()
        } else {
          levels <- lapply(x[2], function(l) eval(parse(text = l)) ) %>%
            unlist()
        }
        c(var, levels)
      } else {
        var
      }
    })
}

parse.unvectorise <- function(vec_str) {
  if (grepl("^c\\(.+\\)$", vec_str)) {
    vec_str %>%
      gsub("^c\\(", "", .) %>%
      gsub("\\)$", "", .) %>%
      strsplit(", *") %>%
      unlist()
  } else {
    vec_str
  }
}
