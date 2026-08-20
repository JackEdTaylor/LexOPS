#' Add a random split to the data.
#'
#' Adds a split to the data comparable to that made by `split_by()`, but split randomly through the data. All entries in `df` are assigned a level randomly.
#'
#' @param x A data frame containing the IV and strings, or a LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param nlevels An integer, specifying how many levels this random split should have (default = 2).
#' @param seed Deprecated: set seeds externally (e.g., `set.seed()`).
#' @param equal_size Logical; should LexOPS create equal (or as close to equal as possible) numbers of candidates for each level? When `FALSE`, will sample N levels with replacement, when `TRUE` will sample N rows. Setting to `TRUE` is recommended, as it will generally create more candidate matches. Default is `FALSE` to avoid altering reproducibility of existing pipelines.
#'
#' @return Returns `df`, with a new column (name defined by `cond_col` argument) identifying which level of the randomly generated IV each string belongs to.
#' @examplesIf rlang::is_installed("lexopsdata")
#'
#' # 2 (syllables: few, many) by 2 (random: level 1, level 2) design
#' lexops |>
#'   split_by(Syllables.CMU, 1:3 ~ 4:6) |>
#'   split_random(nlevels = 2, equal_size = TRUE)
#'
#' @export

split_random <- function(x, nlevels = 2, seed = NA, equal_size = FALSE){

  # give informative error if the user tries to provide a seed
  if (!is.na(seed)) {
    stop("The seed argument is deprecated. Seeds are now set externally. For more information see https://github.com/JackEdTaylor/LexOPS/releases/tag/0.5.0")
  }

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
  current_splits <- grep(paste0("^", cond_col, "_[A-Z]$"), names(df), value = TRUE)

  if (length(current_splits) == 0) {
    prefix <- "A"
  } else {
    current_prefix <- sub(paste0("^", cond_col, "_"), "", current_splits)
    prefix <- setdiff(LETTERS, current_prefix)[1]
  }

  new_column <- paste(cond_col, prefix, sep = "_")

  # generate the random variable
  random_levels <- paste(prefix, 1:nlevels, sep = "")

  # sample to have equally sized groups if requested
  if (equal_size) {
    # equally-sized groups, with over represented categories selected randomly
    random_var <- sample(rep(sample(random_levels), length.out=nrow(df)))
  } else {
    warning("It is generally recommended to set equal_size=TRUE when using split_random(), as this can usually generate more stimuli")
    # sample randomly with replacement
    random_var <- sample(random_levels, size=nrow(df), replace = TRUE)
  }

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
