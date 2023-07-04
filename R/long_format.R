#' Convert to long format
#'
#' Converts the generated stimuli from the `generate()` function into long format.
#'
#' @param df A data frame that is the result from `control_for()` or `split_by()`.
#' @param include A string indicating whether the long-format result should include all variables in the original data frame (`"all"`), only those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Set to `NA` for only the data in the wide-format data frame. The default is `"design"`.
#' @param include_candids Logical; whether to include data about unused candidates in the result. If `TRUE`, will store whether each word was actually used in column, `is_stim`. Default is `FALSE`.
#'
#' @return Returns the generated stimuli, but converted into long format, containing requested variables from the original `df`, and the variables of `item_nr`, `condition`, `euclidean_distance` (from the match_null).
#' @examples
#'
#' lexops |>
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   generate(n = 20) |>
#'   long_format(include = "all")
#'
#' @export

long_format <- function(df, include = "design", include_candids = FALSE) {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check that include is an expected input
  if (!include %in% c("all", "design", "splits", "controls", NA)) stop('`include` must be one of "all", "design", "splits", "controls", or NA')

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_info"))) list() else attr(df, "LexOPS_info")

  # get options from attributes
  if (!is.null(LexOPS_attrs$options)) {
    id_col <- LexOPS_attrs$options$id_col
  } else {
    id_col <- "string"
  }

  # check id_col is a string
  if (!is.character(id_col)) stop(sprintf("Expected id_col to be of class string, not %s", class(id_col)))

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
  long_cols <- if (is.na(include)) {
    NULL
  } else {
    if (include == "all") {
      colnames(LexOPS_attrs$meta_df)[!colnames(LexOPS_attrs$meta_df) %in% c(id_col, "LexOPS_cond")]
    } else {
      splits <- sapply(LexOPS_attrs$splits, dplyr::first)
      controls <- c( sapply(LexOPS_attrs$controls, dplyr::first), sapply(LexOPS_attrs$control_functions, dplyr::first) )
      if (include == "design") {
        colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% c(splits, controls)]
      } else if (include == "splits") {
        colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% splits]
      } else if (include == "controls") {
        colnames(LexOPS_attrs$meta_df)[colnames(LexOPS_attrs$meta_df) %in% controls]
      }
    }
  }

  # the specified columns from meta_df
  meta_df <- LexOPS_attrs$meta_df[, colnames(LexOPS_attrs$meta_df) %in% c(long_cols, id_col)]

  # ensure the id_col is the same type as in the meta_df
  if (!is.character(meta_df[[id_col]])) meta_df[[id_col]] <- as.character(meta_df[[id_col]])

  if (include_candids) {
    # gather outside pipeline for later reference
    out_stim <- tidyr::gather(df, "condition", !!id_col, -match_null, -item_nr)
    # put in long format, and include the specified variables
    out <- dplyr::full_join(out_stim, meta_df, by = id_col) %>%
      dplyr::mutate(is_stim = dplyr::if_else(string %in% out_stim$string, TRUE, FALSE)) %>%
      dplyr::select(item_nr, condition, match_null, dplyr::everything()) %>%
      dplyr::arrange(item_nr)
  } else {
    # put in long format, and include the specified variables
    out <- tidyr::gather(df, "condition", !!id_col, -match_null, -item_nr) %>%
      dplyr::left_join(meta_df, by = id_col) %>%
      dplyr::select(item_nr, condition, match_null, dplyr::everything()) %>%
      dplyr::arrange(item_nr)
  }

  # make note that the df is long format
  LexOPS_attrs$is.long_format <- TRUE

  # add the attrributes to the data frame
  attr(out, "LexOPS_info") <- LexOPS_attrs

  out

}


# # should work
# lexops %>%
#   dplyr::filter(PK.Brysbaert >= .75) %>%
#   split_by(Length, 1:3 ~ 4:6 ~ 7:20) %>%
#   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#   control_for(PoS.SUBTLEX_UK) %>%
#   generate(n = 50) %>%
#   long_format()
