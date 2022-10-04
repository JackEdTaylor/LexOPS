#' Add a random split to the data.
#'
#' Adds a split to the data comparable to that made by `split_by()`, but split randomly through the data. All entries in `df` are assigned a level randomly.
#'
#' @param x A data frame containing the IV and strings, or a LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
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

split_random <- function(x, nlevels = 2, seed = NA){

  # extract df if class is LexOPS_pipeline
  if (is.LexOPS_pipeline(x)) {
    df <- x$df
  } else {
    df <- x
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

  # get options from attributes
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

  # Get next column name and split prefix
  current_splits <- names(df)[stringr::str_which(names(df), paste0("^", cond_col, "_[:upper:]$"))]

  if (length(current_splits) == 0) {
    prefix <-  "A"
  } else {
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
  if (is.null(lp_info$splits)) {
    lp_info$splits <- list(split)
  } else {
    lp_info$splits[[length(lp_info$splits)+1]] <- split
  }

  # also add that this split is random
  if (is.null(lp_info$random_splits)) {
    lp_info$random_splits <- c(length(current_splits)+1)
  } else {
    lp_info$random_splits <- c(lp_info$random_splits, length(current_splits)+1)
  }

  # make a LexOPS pipeline object
  lp <- as.LexOPS_pipeline(df)

  # add the info to the output object
  lp$info <- lp_info

  lp
}
