#' Add a random split to the data.
#'
#' Adds a split to the data comparable to that made by `split_by()`, but split randomly through the data. All entries in `df` are assigned a level randomly.
#'
#' @param df A data frame containing the IV and strings.
#' @param nlevels An integer, specifying how many levels this random split should have (default = 2).
#' @param seed An integer used to set the seed, to reproduce random splits. If `NA`, a random seed will not be set. Default is `NA`.
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

split_random <- function(df, nlevels = 2, seed = NA){
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # get options from attributes
  if (!is.null(LexOPS_attrs$options)) {
    id_col <- LexOPS_attrs$options$id_col
    cond_col <- LexOPS_attrs$options$cond_col
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
  if (!is.na(seed)) set.seed(seed)
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
