#' Control for confounding variables.
#'
#' Specifies variables that should be controlled for in the generated stimuli, after splitting. Can be run multiple times to control for multiple variables.
#'
#' @param df A data frame that is the result from `split_by()`.
#' @param var A list object specifying the variable to be controlled for in the form, `list("variable_column", c(-0.2, 0.2))`, where the first item is the column that specified the IV, and the second argument is a vector indicating the tolerance.
#' @param stringCol The column containing the strings (default = "string").
#' @param condCol Prefix with which the columns detailing the splits were labelled by `split_by()`. This is rarely needed (default = NA), as by default the function gets this information from `df`'s attributes.
#'
#' @return Returns `df`, with details on the variables to be controlled for added to the attributes. Run the `generate()` function to then generate the actual stimuli.
#' @examples
#'
#' # Create 3 levels of syllables, for 1-3, 4-6, and 7-20 syllables, and control for word frequency
#' lexops %>%
#'   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#'   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2)))
#'
#' # Control for multiple variables
#' lexops %>%
#'   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#'   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2))) %>%
#'   control_for("PoS.SUBTLEX_UK")
#'
#' @export

control_for <- function(df, var, stringCol = "string", condCol = NA) {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check the variable specified in var is in the dataframe
  if (!var[[1]] %in% colnames(df)) stop(sprintf("'%s' not in df?", var[[1]]))
  # check stringCol is a string
  if (!is.character(stringCol)) stop(sprintf("Expected stringCol to be of class string, not %s", class(stringCol)))
  # check numeric and non-numeric variables are correctly specified
  if (length(var) > 1) {
    if (length(var)!=2) stop(sprintf("Expected list length of 1 (variable) or 2 (variable, tolerance), not %i.", length(var)))
    if (!is.numeric(df[[var[[1]]]]) & is.numeric(var[[2]])) stop("Did not expect tolerance for non-numeric variable.")
  }
  if (length(var)==1 & is.numeric(df[[var[[1]]]])) warning(sprintf("No tolerance given for numeric variable '%s', will control for exactly.", var[[1]]))

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # check that the splits have been performed (will be stored in the attributes)
  if (is.null(LexOPS_attrs$splitCol) & is.na(condCol)) {
    stop("Unknown split conditions column! Make sure you run split_by() before control_for().")
  } else if (!is.na(condCol)) {
    condCol_regex <- sprintf("^%s_[A-Z]$", condCol)
    if (length(colnames(df)[grepl(condCol_regex, colnames(df))]) == 0) stop(sprintf("No columns found for the manually defined condCol '%s'.", condCol))
  }

  # add the specified controls to df's attributes
  if (is.null(LexOPS_attrs$controls)) {
    LexOPS_attrs$controls <- list(var)
  } else {
    LexOPS_attrs$controls[[length(LexOPS_attrs$controls)+1]] <- var
  }

  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  df
}

# # should throw errors
#
# lexops %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2)))
#
# lexops %>%
#   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2)), condCol = "splitID")
#
# lexops %>%
#   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("PoS.SUBTLEX_UK", c(-0.1, 0.1)))
#
# # should give warning
#
# lexops %>%
#   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2))) %>%
#   control_for(list("Length"))
#
# # should work
#
# lexops %>%
#   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2))) %>%
#   control_for("PoS.SUBTLEX_UK")
