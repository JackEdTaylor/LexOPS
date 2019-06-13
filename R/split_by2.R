#' Add a split to the data.
#'
#' Specifies splits for one IV for a factorial design. Can be called multiple times for multiple splits.
#'
#' @param df A data frame containing the IV and strings.
#' @param split A list object specifying the levels of the split in the form, `list("IV_column", c(1, 3), c(4, 6), ...)` for
#' integer columns; the form `list("IV_column", c(1, 4, 7))` for continuous columns; the form `list("IV_column", c("noun", "verb"))`
#' for factor columns. Splits must be non-overlapping.
#' @param filter Logical. If TRUE, words which fit no conditions are removed.
#' @param condCol Prefix with which to name the column where the condition will be stored (default = "LexOPS_splitCond"). Each time
#' split_by2() is run on a dataframe, a new condCol is added to the data frame, e.g., the first time will add splitCond_A, the second
#' time will ad split_cond_B, etc. If multiple split_by2() functions are used on a data frame (e.g. with pipes), the value of condCol
#' must be the same each time the function is called. The default is usually sufficient.
#'
#' @return Returns `df`, with a new column (name defined by `condCol` argument) identifying which level of the IV each string belongs to.
#' @examples
#'
#' # Create 3 levels of syllables, for 1-3, 4-6, and 7-20 syllables
#' lexops %>%
#'   split_by2(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20)))
#'
#' # Create 2 levels of position of speech, noun and verb
#' lexops %>%
#'   split_by2(list("PoS.SUBTLEX_UK", "noun", "verb"))
#'
#' # Perform two splits
#' lexops %>%
#'   split_by2(list("Syllables.CMU", c(1, 3), c(4, 6), c(7, 20))) %>%
#'   split_by2(list("PoS.SUBTLEX_UK", "noun", "verb"))
#'
#' @seealso \code{\link{lexops}} for the default data frame and associated variables.
#'
#' @export

split_by2 <- function(df, split, condCol = "LexOPS_splitCond", filter = TRUE){

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # add split info
  if (is.null(LexOPS_attrs$splits)) {
    LexOPS_attrs$splits <- list(split)
  } else {
    LexOPS_attrs$splits[[length(LexOPS_attrs$splits)+1]] <- split
  }

  # check the attributes, and add the condCol if not already defined. Throw error if condCol is not the same as that in the previous split
  if (is.null(LexOPS_attrs$splitCol)) {
    LexOPS_attrs$splitCol <- condCol
  } else {
    if (LexOPS_attrs$splitCol != condCol) {
      stop(sprintf("Inconsistent naming of condCol ('%s' != '%s'). The condCol argument must have the same value each time split_by() is run on the data.", condCol, LexOPS_attrs$splitCol))
    }
  }

  # Get next column name and split prefix
  current_splits <- names(df)[stringr::str_which(names(df), paste0("^", condCol, "_[:upper:]$"))]

  if(length(current_splits) == 0){
    prefix <-  "A"
    }else{
      current_prefix <- stringr::str_extract(current_splits, sprintf("(?<=^%s_)[:upper:]", condCol))
      prefix <- dplyr::first(LETTERS[LETTERS != current_prefix])
    }

  new_column <- paste(condCol, prefix, sep = "_")

  # Extract column from split list
  column <- split[[1]]

  # Run appropriate split_by function
  if(is.factor(df[[column]])){
    breaks <- unlist(split[-1])
    df <- split_by.factor(df, column, breaks, new_column, prefix, filter)
  }else{
    col_type <- typeof(df[[column]])

    if(col_type == "integer"){
      breaks <- split[-1]
      df <- split_by.integer(df, column, breaks, new_column, prefix, filter)
    }

    if(col_type == "double"){
      breaks <- split[[-1]]
      df <- split_by.double(df, column, breaks, new_column, prefix, filter)
    }
  }

  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  return(df)
}

split_by.integer <- function(df, column, breaks, new_column, prefix, filter){

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

  labels <- paste0(prefix, 1:(length(breaks) - 1))

  df[[new_column]] <- cut(df[[column]], breaks, labels, right = FALSE)

  if(filter) df <- tidyr::drop_na(df, new_column)

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

# Use rlang ':=' within LexOPS
`:=` <- rlang::`:=`
