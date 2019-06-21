#' Add a random split to the data.
#'
#' Adds a split to the data comparable to that made by `split_by()`, but split randomly through the data. All entries in `df` are assigned a level randomly.
#'
#' @param df A data frame containing the IV and strings.
#' @param nlevels An integer, specifying how many levels this random split should have (default = 2).
#' @param cond_col Prefix with which to name the column where the condition will be stored (default = "LexOPS_splitCond"). Each time split_by() or split_random() is run on a dataframe, a new cond_col is added to the data frame, e.g., the first time will add splitCond_A, the second time will add split_cond_B, etc. If multiple split_by() functions are used on a data frame (e.g. with pipes), the value of cond_col must be the same each time the function is called. The default is usually sufficient.
#'
#' @return Returns `df`, with a new column (name defined by `cond_col` argument) identifying which level of the randomly generated IV each string belongs to.
#' @examples
#'
#' # 2 (syllables: few, many) by 2 (random: level 1, level 2) design
#' lexops %>%
#'   split_by(Syllables.CMU, 1:3 ~ 4:6) %>%
#'   split_random(nlevels = 2) %>%
#'   control_for(Length) %>%
#'   generate(n = 100)
#'
#' @export

split_random <- function(df, nlevels = 2, cond_col = "LexOPS_splitCond"){
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # check the attributes, and add the cond_col if not already defined. Throw error if cond_col is not the same as that in the previous split
  if (is.null(LexOPS_attrs$splitCol)) {
    LexOPS_attrs$splitCol <- cond_col
  } else {
    if (LexOPS_attrs$splitCol != cond_col) {
      stop(sprintf("Inconsistent naming of cond_col ('%s' != '%s'). The cond_col argument must have the same value each time split_by() is run on the data.", cond_col, LexOPS_attrs$splitCol))
    }
  }

  # Get next column name and split prefix
  current_splits <- names(df)[stringr::str_which(names(df), paste0("^", cond_col, "_[:upper:]$"))]

  if(length(current_splits) == 0){
    prefix <-  "A"
    }else{
      current_prefix <- stringr::str_extract(current_splits, sprintf("(?<=^%s_)[:upper:]", cond_col))
      prefix <- dplyr::first(LETTERS[LETTERS != current_prefix])
    }

  new_column <- paste(cond_col, prefix, sep = "_")

  # generate the random variable
  random_levels <- paste(prefix, 1:nlevels, sep = "")
  random_var <- sample(random_levels, nrow(df), replace = TRUE)

  df[[new_column]] <- random_var

  # define split info for a random split
  split <- list("Random Split", random_levels)

  # add split info
  if (is.null(LexOPS_attrs$splits)) {
    LexOPS_attrs$splits <- list(split)
  } else {
    LexOPS_attrs$splits[[length(LexOPS_attrs$splits)+1]] <- split
  }

  # also add that this split is random
  if (is.null(LexOPS_attrs$random_splits)) {
    LexOPS_attrs$random_splits <- c(length(current_splits)+1)
  } else {
    LexOPS_attrs$random_splits <- c(LexOPS_attrs$random_splits, length(current_splits)+1)
  }

  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  df
}
