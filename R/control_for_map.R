#' Control for confounding variables via a function.
#'
#' Some variables (e.g. similarity measures) are hard to control for with \code{\link{control_for}} as they need to be recalculated for each word relative to each other, rather than there being a single value for each possible stimulus. The `control_for_map` function declares a function that the \code{\link{generate}} function should apply to each match_null within an iteration of stimulus generation. The function given as `fun` should be able to take the data in the column given in `var` as the first argument, and should be able to take the match_null's value in that column as the second argument.
#'
#' @param x A data frame containing the IV and strings, or a LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param fun The function to use to calculate the control varibale. Should be an object of class "function".
#' @param var The column to provide the value which will be the first argument of the function.
#' @param tol The tolerance of the control. For numeric variables, this should be in the form lower:upper (e.g. `-0.1:0.1` will control within +/- 0.1). For categorical variables, this can be kept as `NA`.
#' @param name What the output column should be named. If `NA` (default), will automatically assign as `sprintf("control_fun_%i", nr)`, where `nr` is the number of the control function.
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `var` and `tol`. If `TRUE`, `var` should be a character vector referring to a column in `df` (e.g. `"Zipf.SUBTLEX_UK"`), and `tol` should be a vector of length 2, specifying the tolerance (e.g. `c(-0.1, 0.5)`). Default = `FALSE`.
#' @param ... Arguments to be passed to `fun`
#'
#' @return Returns `df`, with details on the variables to be controlled for added to the attributes. Run the \code{\link{generate}} function to then generate the actual stimuli.
#' @examples
#'
#' # Create two levels of arousal, controlling for orthographic similarity
#' # (as optimal string alignment; default for `stringdist()`)
#' library(stringdist)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(stringdist, string, 0:4)
#'
#' # Create two levels of arousal, controlling for orthographic Levenshtein distance
#' # (passed via `method` argument to `stringdist()`)
#' library(stringdist)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(stringdist, string, 0:4, method="lv")
#'
#' # Create two levels of arousal, controlling for phonological Levenshtein distance
#' library(stringdist)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(stringdist, eSpeak.br_1letter, 0:2, method="lv")
#'
#' # Bypass non-standard evaluation
#' library(stringdist)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(stringdist, "eSpeak.br_1letter", c(0, 2), standard_eval=TRUE, method="lv")
#'
#' @export

control_for_map <- function(x, fun, var, tol = NA, name = NA, standard_eval = FALSE, ...) {
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

  # prepend var list with the function
  fun_ellipsed <- function(var_col, match_null) fun(var_col, match_null, ...)
  var <- prepend(var, fun_ellipsed)

  # extract df if class is LexOPS_pipeline
  if (is.LexOPS_pipeline(x)) {
    df <- x$df
  } else {
    df <- x
  }

  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check the variable specified in var is in the dataframe
  if (!var[[2]] %in% colnames(df)) stop(sprintf("'%s' not in df?", var[[2]]))
  # check fun is of class "function"
  if (!is.function(fun)) stop("Argument `fun` should be an object of class, 'function'.")
  # check numeric and non-numeric variables are correctly specified
  if (length(var) > 2) {
    if (length(var)!=3) stop(sprintf("Expected list length of 2 (function, variable) or 3 (function, variable, tolerance), not %i.", length(var)))
  }

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

  # prepend var with what the output should be called
  # var's final structure should be `list(name, fun, var, tol)`
  var <- if (is.na(name)) {
    prepend(var, sprintf("control_map_%i", length(lp_info$control_functions)+1 ))
  } else {
    prepend(var, name)
  }

  # check that the conditions are present in the attributes
  if (is.null(cond_col)) {
    # if the column containing the condition info is missing and not defined manually, throw error
    stop("Could not identify split conditions column! Make sure you run split_by() before generate().")
  }

  # add the specified controls to df's attributes
  if (is.null(lp_info$control_functions)) {
    lp_info$control_functions <- list(var)
  } else {
    lp_info$control_functions[[length(lp_info$control_functions)+1]] <- var
  }

  # make a LexOPS pipeline object
  lp <- as.LexOPS_pipeline(df)

  # add the info to the output object
  lp$info <- lp_info

  lp
}
