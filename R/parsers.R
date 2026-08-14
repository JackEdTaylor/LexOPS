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
#' # Can give any level as just `NULL` or `NA` to store the level as `NA`
#' # (e.g. for `split_by_map()` function in future)
#'
#' parse_levels(substitute(example_variable), substitute(NULL ~ 1.2:4 ~ 5.2:6))
#' parse_levels(substitute(example_variable), substitute(NA ~ 1.2:4 ~ 5.2:6))
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
  var <- parse.unvectorise(paste0(deparse(var), collapse = ""))

  levels <- paste0(deparse(levels), collapse = "")
  levels <- strsplit(levels, "~", fixed = TRUE)[[1]]
  levels <- gsub(" ", "", levels)

  # treat literal "NA" or "NULL" (from deparse) as actual NA
  if (length(levels) == 1 && levels %in% c("NA", "NULL")) levels <- NA

  if (all(grepl(":", levels)) || (any(grepl(":", levels)) && any(levels %in% c("NA", "NULL")))) {
    levels <- strsplit(levels, ":", fixed = TRUE)
    levels <- lapply(levels, function(x) {
      if (length(x) == 1 && any(x %in% c("NA", "NULL"))) NA else as.numeric(x)
    })
  } else {
    # Attempt to evaluate each level; if it's a bare word (e.g. a), treat as string "a"
    levels <- lapply(levels, function(l) {
      l_trim <- gsub(" ", "", l)
      # if it's explicitly quoted, evaluate as-is
      if (grepl('^\".*\"$', l_trim) || grepl("^\'.*\'$", l_trim)) {
        eval(parse(text = l_trim))
      } else if (grepl("^[A-Za-z0-9_.-]+$", l_trim)) {
        # bare word: return as string
        l_trim
      } else if (grepl("^c\\(.*\\)$", l_trim)) {
        # unquoted strings in a character vector: add quotes and then evaluate the expression
        l_requoted <- sub("^c\\(", "c(\"", l_trim)
        l_requoted <- sub("\\)$", "\")", l_requoted)
        l_requoted <- gsub(",", "\",\"", l_requoted)
        eval(parse(text = l_requoted))
      } else {
        # fallback to evaluating the expression (e.g., c("a","b"))
        eval(parse(text = l))
      }
    })
  }

  out <- if (all(is.na(levels))) {
    list(var)
  } else {
    prepend_list(levels, var)
  }

  out
}

#' A non-standard evaluation parser for ellipses
#'
#' This is a version of `parse_levels()` that supports ellipses. This is useful for specifying multiple parameters in one function. This function was specifically designed for a non-standard evaluation update to `match_item()`.
#'
#' @param ... Variables and tolerances, in the form `num_variable1 = -1:3, num_variable2 = -0.5:0.5, char_variable1`. Variables and tolerances should be separated by a `=`, and lower and upper boundaries of tolerances should be separated by `:`. Variables without tolerances are also supported (useful for character variables or matching by numeric variables exactly, i.e. shorthand for `0:0`).
#'
#' @return Returns a list object in the form `list(c("num_variable1", -1, 3), c("num_variable2", -0.5, 0.5), "char_variable1")`.
#' @examples
#'
#' parse_ellipsis(substitute(c(Length = 0:0, Zipf.SUBTLEX_UK = -0.1:0.1, PoS.SUBTLEX_UK)))
#'
#' parse_ellipsis(substitute(c(Length = 0:0,
#'                            Zipf.SUBTLEX_UK = -0.1:0.1,
#'                            PoS.SUBTLEX_UK,
#'                            BG.SUBTLEX_UK = -0.005:0.005)))
#'
#' @export

parse_ellipsis <- function(...) {
  s <- paste0(deparse(...), collapse = "")
  vec <- parse.unvectorise(s)
  parts <- lapply(vec, function(el) strsplit(el, "=", fixed = TRUE)[[1]])
  lapply(parts, function(x) {
    # remove spaces
    x <- gsub(" ", "", x)
    # get var and (optionally) levels for this section
    var <- x[1]
    if (length(x) == 2 && !is.na(x[2]) && nzchar(x[2])) {
      if (grepl(":", x[2])) {
        levels <- as.numeric(strsplit(x[2], ":", fixed = TRUE)[[1]])
      } else {
        levels <- eval(parse(text = x[2]))
      }
      c(var, levels)
    } else {
      var
    }
  })
}

parse.unvectorise <- function(vec_str) {
  if (grepl("^c\\(.+\\)$", vec_str)) {
    inner <- gsub("^c\\(|\\)$", "", vec_str)
    strsplit(inner, ", *")[[1]]
  } else {
    vec_str
  }
}
