#' Control for confounding variables via a function.
#'
#' Some variables (e.g. similarity measures) are hard to control for with \code{\link{control_for}} as they need to be recalculated for each word relative to each other, rather than there being a single value for each possible stimulus. The `control_for_map` function declares a function that the \code{\link{generate}} function should apply to each match_null within an iteration of stimulus generation. The function given as `fun` should be able to take the data in the column given in `var` as the first argument, and should be able to take the match_null's value in that column as the second argument.
#'
#' @param df A data frame that is the result from \code{\link{split_by}}.
#' @param fun The function to use to calculate the control varibale. Should be an object of class "function".
#' @param var The column to provide the value which will be the first argument of the function.
#' @param tol The tolerance of the control. For numeric variables, this should be in the form lower:upper (e.g. `-0.1:0.1` will control within +/- 0.1). For categorical variables, this can be kept as `NA`.
#' @param name What the output column should be named. If `NA` (default), will automatically assign as `sprintf("control_fun_%i", nr)`, where `nr` is the number of the control function.
#' @param cond_col Prefix with which the columns detailing the splits were labelled by \code{\link{split_by}}. This is rarely needed (default = NA), as by default the function gets this information from `df`'s attributes.
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `var` and `tol`. If `TRUE`, `var` should be a character vector referring to a column in `df` (e.g. `"Zipf.SUBTLEX_UK"`), and `tol` should be a vector of length 2, specifying the tolerance (e.g. `c(-0.1, 0.5)`). Default = `FALSE`.
#'
#' @return Returns `df`, with details on the variables to be controlled for added to the attributes. Run the \code{\link{generate}} function to then generate the actual stimuli.
#' @examples
#'
#' # Create two levels of arousal, controlling for orthographic similarity
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(levenshtein.distance, string, 0:4)
#'
#' # Create two levels of arousal, controlling for phonological similarity
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(levenshtein.distance, eSpeak.br_1letter, 0:2)
#'
#' # Bypass non-standard evaluation
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
#'  control_for_map(levenshtein.distance, "eSpeak.br_1letter", c(0, 2), standard_eval=TRUE)
#'
#' @export

control_for_map <- function(df, fun, var, tol = NA, name = NA, cond_col = NA, standard_eval = FALSE) {
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
  var <- prepend(var, fun)

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

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # prepend var with what the output should be called
  # var's final structure should be `list(name, fun, var, tol)`
  var <- if (is.na(name)) {
    prepend(var, sprintf("control_map_%i", length(LexOPS_attrs$control_functions)+1 ))
  } else {
    prepend(var, name)
  }

  # check that the splits have been performed (will be stored in the attributes)
  if (is.null(LexOPS_attrs$splitCol) & is.na(cond_col)) {
    stop("Unknown split conditions column! Make sure you run split_by() before control_for().")
  } else if (!is.na(cond_col)) {
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
    if (length(colnames(df)[grepl(cond_col_regex, colnames(df))]) == 0) stop(sprintf("No columns found for the manually defined cond_col '%s'.", cond_col))
  }

  # add the specified controls to df's attributes
  if (is.null(LexOPS_attrs$control_functions)) {
    LexOPS_attrs$control_functions <- list(var)
  } else {
    LexOPS_attrs$control_functions[[length(LexOPS_attrs$controls)+1]] <- var
  }

  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  df
}
