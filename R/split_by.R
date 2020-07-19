#' Add a split to the data.
#'
#' Specifies splits for one IV for a factorial design. Can be called multiple times for multiple splits.
#'
#' @param df A data frame containing the IV and strings.
#' @param var The column to treat as an independent variable (non-standard evaluation).
#' @param levels The boundaries to use as levels of this variable (non-standard evaluation). These should be specified in the form `1:3 ~ 4:6 ~ 7:9` or `c(1, 3) ~ c(4, 6) ~ c(7, 9)` for numeric variables, and (e.g.) `"noun" ~ "verb" ~ c"adjective"` for character variables, where levels are separated by the `~` operator. Levels must be non-overlapping.
#' @param filter Logical. If TRUE, words which fit no conditions are removed.
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `var` and `levels`. If `TRUE`, `var` should be a character vector referring to a column in `df` (e.g. `"Zipf.SUBTLEX_UK"`), and `levels` should be a list containing multiple vectors of length 2, each specifying the boundaries of one level's bin (e.g. `list(c(1, 3), c(4, 6), c(7, 20))`). Default = `FALSE`.
#'
#' @return Returns `df`, with a new column (name defined by `cond_col` argument of `set_options()`) identifying which level of the IV each string belongs to.
#' @examples
#'
#' # Create 3 levels of syllables, for 1-3, 4-6, and 7-20 syllables
#' lexops %>%
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20)
#'
#' # Same split as above, but supplying boundaries as vectors
#' lexops %>%
#'   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20))
#'
#' # Create 2 levels of position of speech, noun and verb
#' lexops %>%
#'   split_by(PoS.SUBTLEX_UK, "noun" ~ "verb")
#'
#' # split into two levels: (1) nouns or names, and (2) adjectives or adverbs
#' lexops %>%
#'  split_by(PoS.SUBTLEX_UK, c("noun", "name") ~ c("adjective", "adverb"))
#'
#' # Perform two splits
#' lexops %>%
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
#'   split_by(PoS.SUBTLEX_UK, c("noun", "name") ~ c("adjective", "adverb"))
#'
#' # Bypass non-standard evaluation
#' lexops %>%
#'   split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) %>%
#'   split_by("PoS.SUBTLEX_UK", list(c("noun", "name"), "verb"), standard_eval = TRUE)
#'
#' @seealso \code{\link{lexops}} for the default data frame and associated variables.
#'
#' @export

split_by <- function(df, var, levels, filter = TRUE, standard_eval = FALSE){

  # convert var and levels to a list, `split`, which will be added to the attributes
  split <- if (standard_eval) {
    if (is.list(levels)) {
      prepend(levels, var)
    } else {
      list(var, levels)
    }
  } else {
    parse_levels(substitute(var), substitute(levels))
  }

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

  # add split info
  if (is.null(LexOPS_attrs$splits)) {
    LexOPS_attrs$splits <- list(split)
  } else {
    LexOPS_attrs$splits[[length(LexOPS_attrs$splits)+1]] <- split
  }

  # check that all the defined levels have at least one possible value in the distribution or values
  if (is.numeric(df[[split[[1]]]])) {
    true_levels <- lapply(split[-1], function(x) {
      split_var <- split[[1]]
      nrow(dplyr::filter(df, dplyr::between(!!dplyr::sym(split_var), x[1], x[2]))) > 0
    })
  } else {
    true_levels <- lapply(split[-1], function(x) {
      split_var <- split[[1]]
      nrow(dplyr::filter(df, any(x %in% !!dplyr::sym(split_var)))) > 0
    })
  }
  if (!all(unlist(true_levels))) {
    warning(sprintf("No entries could be found for some levels. Check all levels of %s are possible.", split[[1]]))
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

  # Extract column from split list
  column <- split[[1]]

  # convert to factor if stored as character
  if (is.character(df[[column]])) {
    warning(sprintf("Column %s is type character so will be treated as a factor.", column))
    df[[column]] <- as.factor(df[[column]])
  }

  # Run appropriate split_by function
  if (is.factor(df[[column]])) {
    breaks_lengths <- lapply(split[-1], length)
    if (any(breaks_lengths > 1)) {
      breaks <- split[-1]
      df <- split_by.factor_group(df, column, breaks, new_column, prefix, filter)
    } else {
      breaks <- unlist(split[-1])
      df <- split_by.factor(df, column, breaks, new_column, prefix, filter)
    }
  } else {
    col_type <- typeof(df[[column]])

    if (col_type == "integer") {
      breaks <- split[-1]
      df <- split_by.integer(df, column, breaks, new_column, prefix, filter)
    }

    if (col_type == "double") {
      breaks <- split[-1]
      df <- split_by.double(df, column, breaks, new_column, prefix, filter)
    }
  }

  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  return(df)
}

split_by.integer <- function(df, column, breaks, new_column, prefix, filter){

  overlap_check <- check_overlapping(breaks)
  if(!overlap_check) warning("overlapping breaks: some levels of this variable will overlap")

  df_filter <- purrr::map(breaks, ~seq(.x[1], .x[2], 1)) %>%
    purrr::map(~tibble::tibble(!!column := .x)) %>%
    dplyr::bind_rows(.id = new_column) %>%
    dplyr::mutate(!!new_column := paste0(prefix, eval(rlang::sym(new_column))))

  if(filter){
    df <- dplyr::inner_join(df, df_filter, by = column)
  }else{
    df <- dplyr::left_join(df, df_filter, by = column)
  }

  df[[new_column]] <- as.factor(df[[new_column]])

  return(df)
}

split_by.double <- function(df, column, breaks, new_column, prefix, filter){

  # Check that splits are ordered and don't overlap
  check_ordered(breaks)

  overlap_check <- check_overlapping(breaks)
  if(!overlap_check) stop("overlapping breaks: overlapping breaks not permitted for split of type double - try assigning conditions manually as a factor instead")

  cont_breaks <- check_continuous(breaks)

  if(!cont_breaks){
    # Label gaps between cuts as NA
    n_cuts <- length(breaks)
    cut_labs <- c(paste0(prefix, 1))

    for(i in 2:n_cuts){
      cut_labs <- c(cut_labs, NA, paste0(prefix, i))
    }

    cuts <- unlist(breaks)

  }else{
    cuts <- unique(unlist(breaks))
    cut_labs <- paste0(prefix, 1:(length(cuts) - 1))
  }

  df[[new_column]] <- cut(df[[column]], cuts, cut_labs, right = FALSE)
  df$tmp <- as.character(df[[new_column]])

  if(filter){
    df <- df %>%
      tidyr::drop_na(tmp) %>%
      dplyr::select(-tmp)
  }

  return(df)
}

split_by.factor <- function(df, column, breaks, new_column, prefix, filter){

  if(all(breaks %in% levels(df[[column]]))){
    breaks <- factor(breaks, levels = levels(df[[column]]))
  }else{
    stop("not all breaks are existing factors")
  }

  df_filter <- tibble::tibble(!!column := breaks,
                              !!new_column := paste0(prefix, 1:length(breaks)))

  if(filter){
    df <- dplyr::inner_join(df, df_filter, by = column)
  }else{
    df <- dplyr::left_join(df, df_filter, by = column)
  }

  df[[new_column]] <- as.factor(df[[new_column]])

  return(df)
}

split_by.factor_group <- function(df, column, breaks, new_column, prefix, filter){

  if(all(unlist(breaks) %in% levels(df[[column]]))){
    breaks <- lapply(breaks, factor, levels = levels(df[[column]]))
  }else{
    stop("not all breaks are existing factors")
  }

  breaks_lengths <- sapply(breaks, length)

  breaks_base <- paste0(prefix, 1:length(breaks))

  breaks_IDs <- lapply(1:length(breaks), function(i) {
      paste(breaks_base[i], letters[1:breaks_lengths[i]], sep="_")
  }) %>%
    unlist()

  df_filter <- tibble::tibble(!!column := unlist(breaks),
                              !!new_column := breaks_IDs)

  if(filter){
    df <- dplyr::inner_join(df, df_filter, by = column)
  }else{
    df <- dplyr::left_join(df, df_filter, by = column)
  }

  # remove the last section of the new_column, which will be _a, _b, _c, etc.
  # note that \K is a special escape for PERL
  df[[new_column]] <- gsub("([A-Z]{1,2}\\d*)\\K(_[a-z]{1,2})", "", df[[new_column]], perl = TRUE)

  df[[new_column]] <- as.factor(df[[new_column]])

  return(df)
}

# Checks
check_ordered <- function(x){
  check_order <- purrr::map(x, ~.x[1] < .x[2]) %>%
    unlist() %>%
    all()

  if(!check_order) stop("lower bounds must be lower than upper bounds")
}

check_overlapping <- function(x) {
  overlap_check <- logical()

  # order by lower bound of each pair (in case not ordered linearly)
  x_low <- sapply(x, function(x_i) x_i[[1]])
  x_sorted <- x[order(x_low)]

  # check the ordered levels don't overlap
  for(i in 1:(length(x) - 1)){
    overlap_check <- c(overlap_check, x_sorted[[i]][2] <= x_sorted[[i + 1]][1])
  }

  overlap_check <- all(overlap_check)

  overlap_check
}

check_continuous <- function(breaks){
  n_breaks <- length(breaks)

  if(n_breaks == 1) return(TRUE)

  cont_check <- logical()

  for(i in 1:(n_breaks - 1)){
    cont_check <- c(cont_check, breaks[[1]][2] == breaks[[2]][1])
  }

  if(all(cont_check)){
    return(TRUE)
  }else if(any(cont_check)){
    warning("Mixture of continuous and discontinuous breaks provided. Result will include gaps between breaks.")
    return(TRUE)
  }else{
    return(FALSE)
  }
}
