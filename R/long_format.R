#' Convert to long format
#'
#' Converts the generated stimuli from the `generate()` function into long format.
#'
#' @param df A data frame that is the result from `control_for()` or `split_by()`.
#' @param include A string indicating whether the long-format result should include all variables in the original data frame (`"all"`), only those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Set to `NA` for only the data in the data frame. The default is `"design"`.
#' @param stringCol The column containing the strings (default = `"string"`).
#' @return Returns the generated stimuli, but converted into long format.
#' @examples
#'
#' lexops %>%
#'   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) %>%
#'   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2)) %>%
#'   generate(n = 20) %>%
#'   long_format(include = "all")
#'
#' @export

long_format <- function(df, include = "design", stringCol = "string") {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check that include is an expected input
  if (!include %in% c("all", "design", "splits", "controls", NA)) stop('`include` must be one of "all", "design", "splits", "controls", or NA')
  # check stringCol is a string
  if (!is.character(stringCol)) stop(sprintf("Expected stringCol to be of class string, not %s", class(stringCol)))

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # check that the generate function has been run
  if (is.null(LexOPS_attrs$generated)) {
    stop("`long_format()` should only be run on a dataframe generated through the LexOPS `generate()` function")
  } else {
    # check the meta-data is present
    if (is.null(LexOPS_attrs$meta_df)) {
      stop("`long_format` requires that the `control_for()` function has been run before the `generate()` function (or else the data is already in long format anyway)")
    }
  }

  # get a vector specifying which columns to include
  long_cols <- if (include == "all") {
    colnames(LexOPS_attrs$meta_df)[!colnames(LexOPS_attrs$meta_df) %in% c(stringCol, "LexOPS_cond")]
  } else if (is.na(include)) {
    NA
  } else {
    splits <- sapply(LexOPS_attrs$splits, dplyr::first)
    controls <- sapply(LexOPS_attrs$controls, dplyr::first)
    if (include == "design") {
      colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% c(splits, controls)]
    } else if (include == "splits") {
      colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% splits]
    } else if (include == "controls") {
      colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% controls]
    }
  }

  # the specified columns from meta_df
  meta_df <- LexOPS_attrs$meta_df[, colnames(LexOPS_attrs$meta_df) %in% c(long_cols, stringCol)]

  # put in long format, and include the specified variables
  out <- tidyr::gather(df, "condition", !!stringCol, -match_null, -item_nr) %>%
    dplyr::left_join(meta_df, by = stringCol) %>%
    dplyr::select(item_nr, condition, match_null, dplyr::everything()) %>%
    dplyr::arrange(item_nr)

  # add the attrributes to the data frame
  attr(out, "LexOPS_attrs") <- LexOPS_attrs

  out

}


# # should work
# lexops %>%
#   dplyr::filter(PK.Brysbaert >= .75) %>%
#   split_by(list("Length", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2))) %>%
#   control_for("PoS.SUBTLEX_UK") %>%
#   generate(n = 50) %>%
#   long_format()
