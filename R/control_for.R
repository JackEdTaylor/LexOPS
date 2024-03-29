#' Control for confounding variables.
#'
#' Specifies variables that should be controlled for in the generated stimuli, after splitting. Can be run multiple times to control for multiple variables.
#'
#' @param x A data frame containing the IV and strings, or a LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param var The column to treat as a control (non-standard evaluation).
#' @param tol The tolerance of the control. For numeric variables, this should be in the form lower:upper (e.g. `-0.1:0.1` will control within +/- 0.1). For categorical variables, this can be kept as `NA`.
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `var` and `tol`. If `TRUE`, `var` should be a character vector referring to a column in `df` (e.g. `"Zipf.SUBTLEX_UK"`), and `tol` should be a vector of length 2, specifying the tolerance (e.g. `c(-0.1, 0.5)`). Default = `FALSE`.
#'
#' @return Returns `df`, with details on the variables to be controlled for added to the attributes. Run the `generate()` function to then generate the actual stimuli.
#' @examples
#'
#' # Create 3 levels of syllables, for 1-3, 4-6, and 7-20 syllables, and control for word frequency
#' lexops |>
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2)
#'
#' # Control for multiple variables
#' lexops |>
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(PoS.SUBTLEX_UK)
#'
#' # Bypass non-standard evaluation
#' lexops |>
#'  split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) |>
#'  control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) |>
#'  control_for("PoS.SUBTLEX_UK", standard_eval = TRUE)
#'
#' @export

control_for <- function(x, var, tol = NA, standard_eval = FALSE) {
  # extract df if class is LexOPS_pipeline
  if (is.LexOPS_pipeline(x)) {
    df <- x$df
  } else {
    df <- x
  }

  # parse the column name and tolerance into a list object
  var <- if (standard_eval) {
    if (all(is.na(tol))) {
      list(var)
    } else {
      list(var, tol)
    }
  } else {
    parse_levels(substitute(var), substitute(tol))
  }

  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check the variable specified in var is in the dataframe
  if (!var[[1]] %in% colnames(df)) stop(sprintf("'%s' not in df?", var[[1]]))
  # check numeric and non-numeric variables are correctly specified
  if (length(var) > 1) {
    if (length(var)!=2) stop(sprintf("Expected list length of 1 (variable) or 2 (variable, tolerance), not %i.", length(var)))
    if (!is.numeric(df[[var[[1]]]]) & is.numeric(var[[2]])) stop("Did not expect tolerance for non-numeric variable.")
  }
  if (length(var)==1 & is.numeric(df[[var[[1]]]])) warning(sprintf("No tolerance given for numeric variable '%s', will control for exactly.", var[[1]]))

  # get pipeline info
  lp_info <- if (is.LexOPS_pipeline(x)) {
    if (is.null(x$info)) {
      list()
    } else {
      x$info
    }
  } else {
    list()
  }

  # get options from pipeline info
  if (!is.null(lp_info$options)) {
    id_col <- lp_info$options$id_col
    cond_col <- lp_info$options$cond_col
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
  } else {
    id_col <- "string"
    cond_col <- "LexOPS_splitCond"
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
  }

  # check that the conditions are present in the attributes
  if (is.null(cond_col)) {
    # if the column containing the condition info is missing and not defined manually, throw error
    stop("Could not identify split conditions column! Make sure you run split_by() before generate().")
  }

  # add the specified controls to df's attributes
  if (is.null(lp_info$controls)) {
    lp_info$controls <- list(var)
  } else {
    lp_info$controls[[length(lp_info$controls)+1]] <- var
  }

  # make a LexOPS pipeline object
  lp <- as.LexOPS_pipeline(df)

  # add the info to the output object
  lp$info <- lp_info

  lp
}

# # should throw errors
#
# lexops |>
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2))
#
# lexops |>
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) |>
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2), cond_col = "splitID")
#
# lexops |>
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) |>
#   control_for(PoS.SUBTLEX_UK, c(-0.1, 0.1))
#
# # should give warning
#
# lexops |>
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) |>
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2)) |>
#   control_for(Length)
#
# # should work
#
# lexops |>
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) |>
#   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#   control_for(PoS.SUBTLEX_UK)
#
# # should be identical output to above
# lexops |>
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) |>
#   control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) |>
#   control_for("PoS.SUBTLEX_UK", standard_eval = TRUE)
