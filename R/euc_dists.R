#' Calculate a word's Euclidean distance from other words
#'
#' Caclulates the Euclidean distance of a word from all other words in a df, on selected variables.
#'
#' @param df A data frame.
#' @param target The target string (word) that euclidean distances are required for.
#' @param vars The variables to be used as dimensions which Euclidean distance should be calculated over. Can be a vector of variable names (e.g. `c(Zipf.SUBTLEX_UK, Length)`), or, `"all"`, to use all numeric variables in the data frame. The default is `"all"`.
#' @param scale,center How should variables be scaled and/or centred before calculating Euclidean distance? For options, see the `scale` and `center` arguments of \code{\link[base]{scale}}. Default for both is `TRUE`. Scaling can be useful when variables are in differently scaled.
#' @param weights An (optional) list of weights, in the same order as `vars`. After any scaling is applied, the values will be multiplied by these weights. Default is `NA`, meaning no weights are applied.
#' @param id_col The column containing the strings (default = `"string"`).
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `vars`. If `TRUE`, `vars` should be a character vector referring to columns in `df` (e.g. `c("Length", "Zipf.SUBTLEX_UK")`). Default = `FALSE`.
#'
#' @return Returns a vector of Euclidean distances, in the order of rows in `df`.
#' @examples
#'
#' # Get the distance of every entry in the `lexops` dataset from the word "thicket".
#' # (Note: This will be calculated using the dimensions of frequency, arousal, and size)
#' lexops %>%
#'   euc_dists("thicket", c(Zipf.SUBTLEX_UK, AROU.Warriner, SIZE.Glasgow_Norms))
#'
#' # no scaling or centering
#' lexops %>%
#'   euc_dists(
#'     "thicket",
#'     c(Zipf.SUBTLEX_UK, AROU.Warriner, SIZE.Glasgow_Norms),
#'     scale = FALSE,
#'     center = FALSE
#'   )
#'
#' # Add Euclidean distance as new column
#' # (Also sort ascendingly by distance; barbara will have a distance of 0 so will be first)
#' lexops %>%
#'   dplyr::mutate(ed = euc_dists(., "barbara", c(Length, Zipf.SUBTLEX_UK, BG.SUBTLEX_UK))) %>%
#'   dplyr::arrange(ed)
#'
#' # bypass non-standard evaluation
#' lexops %>%
#'   euc_dists(
#'     "thicket",
#'     c("Zipf.SUBTLEX_UK", "AROU.Warriner", "SIZE.Glasgow_Norms"),
#'     standard_eval = TRUE
#'   )
#' @export

euc_dists <- function(df = LexOPS::lexops, target, vars = "all", scale = TRUE, center = TRUE, weights = NA, id_col = "string", standard_eval = FALSE) {
  if (!standard_eval) {
    vars <- parse_levels(substitute(vars)) %>%
      unlist()
    # revert to "all" if parse_levels(vars) returns "\"all\""
    if (all(vars=="\"all\"") & length(vars)==1) vars <- "all"
  }

  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # if there are no vars specified (e.g. empty vector) return NAs
  if (length(vars) == 0) {
    warning("No numeric columns specified in `vars`")
    return(rep(NA, nrow(df)))
  }
  # check that vars is an expected input
  if (!is.vector(vars) & all(vars != "all")) stop('Expected vars to be either a vector of column names, or "all"')
  # if vars == "all", change vars to all numeric columns
  if (all(vars == "all")) {
    vars <- colnames(df)[sapply(df, is.numeric)]
    if (length(vars) == 0) {
      warning("No numeric columns detected in `df`")
      return(rep(NA, nrow(df)))
    }
  }
  # check that all vars are in df
  if (!all(vars %in% colnames(df))) {
    unknown_cols <- vars[!vars %in% colnames(df)]
    stop(sprintf("%i unknown columns in `df`: %s", length(unknown_cols), paste(unknown_cols, collapse = ", ")))
  }
  # check that all vars are numeric
  if (!all(sapply(df[, vars], is.numeric))) {
    non_numeric_cols <- colnames(df[, vars])[!sapply(df[, vars], is.numeric)]
    stop(sprintf("%i non-numeric columns specified in `vars`: %s", length(non_numeric_cols), paste(non_numeric_cols, collapse = ", ")))
  }
  # if specified, check weights is a numeric vector of correct length
  if (any(!is.na(weights))) {
    if (!is.numeric(weights) | length(weights)!=length(vars)) stop("`weights` should be a numeric vector of length equal to that of vars")
  }
  # check id_col is a string
  if (!is.character(id_col)) stop(sprintf("Expected id_col to be of class string, not %s", class(id_col)))

  # calculate Euclidean distance of word from all others on specified dimensions

  # firstly, scale all specified dimensions as required
  df[, vars] <- lapply(df[, vars], base::scale, center, scale)

  # get the vector for the target word
  target_dims <- df[df[[id_col]] == target, vars]

  # get all the other vectors (including the target)
  dims <- df[, vars]

  # apply weights
  if (any(!is.na(weights))) {
    dims <- lapply(1:ncol(dims), function(i) dims[, i] * weights[i]) %>%
      as.data.frame()
  }

  # get the distance squared
  dist_sq <- as.data.frame(
    lapply(colnames(dims), function(d) {
      (dims[d] - c(target_dims[[d]]))**2
      })
    )

  # get the row-wise euclidean distance
  sqrt(rowSums(dist_sq))

}
