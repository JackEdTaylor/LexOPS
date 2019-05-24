#' Add a split to the data.
#'
#' Specifies splits for one IV for a factorial design. Can be called multiple times for multiple splits.
#'
#' @param df A data frame containing the IV and strings.
#' @param split A list object specifying the levels of the split in the form, `list("IV_column", c(1, 3), c(4, 6), ...)`, where the first item is the column that specified the IV. Subsequent arguments specify all levels of the split. Splits must be non-overlapping.
#' @param filter Logical. If TRUE, words which fit no conditions are removed.
#' @param stringCol The column containing the strings (default = "string").
#' @param condCol Prefix with which to name the column where the condition will be stored (default = "LexOPS_splitCond"). Each time split_by() is run on a dataframe, a new condCol is added to the data frame, e.g., the first time will add splitCond_A, the second time will ad split_cond_B, etc. If multiple split_by() functions are used on a data frame (e.g. with pipes), the value of condCol must be the same each time the function is called. The default is usually sufficient. This values will be stored in the data frame's attributes.
#'
#' @return Returns `df`, with a new column (name defined by `condCol` argument) identifying which level of the IV each string belongs to.
#' @examples
#'
#' # Create 3 levels of syllables, for 1-3, 4-6, and 7-20 syllables
#' lexops %>%
#'   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20)))
#'
#' # Create 2 levels of position of speech, noun and verb
#' lexops %>%
#'   split_by(list("PoS.SUBTLEX_UK", "noun", "verb"))
#'
#' # Perform two splits
#' lexops %>%
#'   split_by(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#'   split_by(list("PoS.SUBTLEX_UK", "noun", "verb"))
#'
#' @seealso \code{\link{lexops}} for the default data frame and associated variables.
#'
#' @export

split_by <- function(df = LexOPS::lexops, split, filter = TRUE, stringCol = "string", condCol = "LexOPS_splitCond") {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check the variable specified in split is in the dataframe
  if (!split[[1]] %in% colnames(df)) stop(sprintf("'%s' not in df?", split[[1]]))
  # check stringCol is a string
  if (!is.character(stringCol)) stop(sprintf("Expected stringCol to be of class string, not %s", class(stringCol)))
  # check numeric and non-numeric variables are correctly specified
  if (is.numeric(df[[split[[1]]]]) & !all(sapply(split[2:length(split)], is.numeric))) {
    stop("Expected numeric tolerances for numeric variable.")
  } else if (!is.numeric(df[[split[[1]]]]) & all(sapply(split[2:length(split)], is.numeric))) {
    stop("Expected non-numeric tolerances for non-numeric variable.")
  }

  # check the attributes, and add the condCol if not already defined. Throw error if condCol is not the same as that in the previous split
  if (is.null(attr(df, "LexOPS_splitcol"))) {
    attr(df, "LexOPS_splitcol") <- condCol
  } else {
    if (attr(df, "LexOPS_splitcol") != condCol) {
      stop(sprintf("Inconsistent naming of condCol ('%s' != '%s'). The condCol argument must have the same value each time split_by() is run on the data.", condCol, attr(df, "LexOPS_splitcol")))
    }
  }

  # detect any other condCols in the df, and work out what to suffix this new condCol
  condCol_regex <- sprintf("^%s_[A-Z]$", condCol)
  other_splits <- colnames(df)[grepl(condCol_regex, colnames(df))]

  last_split_nr <- if (length(other_splits) > 0) {
    other_splits %>%  # detect any other condCols in the df
      substr(nchar(condCol)+2, nchar(condCol)+2) %>%  # get the letters of all these matches
      match(LETTERS) %>%  # get the numbers of these
      max()  # get how many splits have been done
  } else {
    0
  }

  condCol_this_split <- sprintf("%s_%s", condCol, LETTERS[last_split_nr+1])

  # find and label columns that fit each level
  if (is.numeric(df[[split[[1]]]])) {
    fit <- sapply(split[2:length(split)], function(tol) {
      dplyr::between(df[[split[[1]]]], tol[1], tol[2])
    })
  } else {
    fit <- sapply(split[2:length(split)], function(tol) {
     df[[split[[1]]]] == tol[[1]]
    })
  }

  # check for overlapping tolerances
  rowwise_true <- apply(fit, 1, function(row) sum(row, na.rm=TRUE))
  if (any(rowwise_true > 1)) stop("Overlapping tolerances? Ensure that tolerances for levels are mutually exclusive.")
  columnwise_true <- apply(fit, 2, function(row) sum(row, na.rm=TRUE))
  if (any(columnwise_true==0)) warning("No entries fit at least one tolerance. Check tolerances.")

  # get which level of the split each word refers to
  index_true <- unlist(apply(fit, 1, function(row) {
    if (all(is.na(row))) {
      NA
    } else if (all(!row)) {
      NA
    } else {
      sprintf("%s%i", LETTERS[last_split_nr+1], which(row))
    }
  }))

  # add to the df as a new column
  df[[condCol_this_split]] <- ifelse(index_true==0, df[[condCol_this_split]], index_true) %>%
    as.factor()

  if (filter) {
    df <- dplyr::filter(df, !is.na(!!(dplyr::sym(condCol_this_split))))
  }

  df
}

# # should throw each of the possible errors
#
# "non_df_object" %>% split_by(list("Syllables.CMU", c(1, 2), c(3, 4), c(5, 9)))
#
# LexOPS::lexops %>% split_by(list("Syllables.CMU", "few", "lots"))
#
# LexOPS::lexops %>% split_by(list("PoS.SUBTLEX_UK", c(1, 3), c(4, 6)))
#
# LexOPS::lexops %>% split_by(list("PoS.SUBTLEX_UK", "noun", "verb")) %>% split_by(list("Length", c(1, 3), c(4, 7), c(8, 30)), condCol = "spliteroo")
#
# # should work
#
# LexOPS::lexops %>% split_by(list("Syllables.CMU", c(1, 2), c(3, 4), c(5, 9)))
#
# LexOPS::lexops %>% split_by(list("Length", c(1, 3), c(4, 7), c(8, 30)))
#
# LexOPS::lexops %>% split_by(list("PoS.SUBTLEX_UK", "noun", "verb"))
#
# LexOPS::lexops %>% split_by(list("PoS.SUBTLEX_UK", "noun", "verb")) %>% split_by(list("Length", c(1, 3), c(4, 7), c(8, 30)))
