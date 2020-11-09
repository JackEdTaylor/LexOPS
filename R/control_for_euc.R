#' Control for Euclidean distance in several numeric variables
#'
#' This function is a wrapper for \code{\link{control_for_map}} that allows you to easily control for Euclidean distance.
#'
#' @param df A data frame that is the result from \code{\link{split_by}}.
#' @param vars The columns from which to calculate Euclidean distance.
#' @param tol The desired control tolerance, in Euclidean distance (will be interpreted as scaled Euclidean distance if `scaled == TRUE`).
#' @param name What the output column should be named. If `NA` (default), will automatically assign as `sprintf("control_fun_%i", nr)`, where `nr` is the number of the control function.
#' @param scale,center How should variables be scaled and/or centred \emph{before} calculating Euclidean distance? For options, see the `scale` and `center` arguments of \code{\link[base]{scale}}. Default for both is `TRUE`. Scaling can be useful when variables are in differently scaled.
#' @param weights An (optional) list of weights, in the same order as `vars`. After any scaling is applied, the values will be multiplied by these weights. Default is `NA`, meaning no weights are applied.
#' @param euc_df The dataframe to calculate the Euclidean distance from. By default, the function will use `df`. Giving a different dataframe to `euc_df` can be useful in some cases, such as when `df` has been filtered for generating stimuli, but you want to calculate Euclidean Distance from a full distribution.
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R objects in `vars` and `tol`. If `TRUE`, `vars` should be a character vector referring to columns in `df` (e.g. `c("Zipf.SUBTLEX_UK", "Length")`), and `tol` should be a vector of length 2, specifying the tolerance (e.g. `c(0, 0.5)`). Default = `FALSE`.
#'
#' @return Returns `df`, with details on the variables to be controlled for added to the attributes. Run the \code{\link{generate}} function to then generate the actual stimuli.
#' @examples
#'
# control for length and frequency
#' stim <- lexops %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   control_for_euc(c(Zipf.BNC.Written, Length), 0:0.005) %>%
#'   generate(10)
#'
#' # bypass non-standard evaluation
#' stim <- lexops %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   control_for_euc(c("Zipf.BNC.Written", "Length"), c(0, 0.005), standard_eval = TRUE) %>%
#'   generate(10)
#'
#' # generate stimuli from a filtered dataframe, but calculate
#' # Euclidean distance from an (original) unfiltered dataframe
#' library(dplyr)
#' stim <- lexops %>%
#'   filter(
#'     Zipf.SUBTLEX_UK <= 5,
#'     between(Length, 3, 12),
#'     PK.Brysbaert >= 0.9
#'   ) %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   control_for_euc(
#'     c(Zipf.SUBTLEX_UK, Length),
#'     0:0.005,
#'     name = "Euclidean Distance",
#'     euc_df = lexops
#'   ) %>%
#'   generate(10)
#'
#' @export

control_for_euc <- function(df, vars, tol, name = NA, scale = TRUE, center = TRUE, weights = NA, euc_df = NA, standard_eval = FALSE) {

  control <- if (standard_eval) {
    if (is.list(levels)) {
      prepend(tol, vars)
    } else {
      list(vars, tol)
    }
  } else {
    parse_levels(substitute(vars), substitute(tol))
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

  if (all(is.na(euc_df))) euc_df <- df

  control_for_euc.calc_euc <- function(matches, target, ed_vars = control[[1]], scale_ = scale, center_ = center, weights_ = weights, euc_df_ = euc_df, id_col_ = id_col) {
    if (!target %in% dplyr::pull(euc_df_, !!dplyr::sym(id_col_))) return(NA)
    # get all euclidean distances
    df_ed <- dplyr::mutate(
      dplyr::select(euc_df_, !!dplyr::sym(id_col_)),
      control_for_euc_val = LexOPS::euc_dists(
        df = dplyr::select(euc_df_, !!dplyr::sym(id_col_), dplyr::all_of(ed_vars)),
        target = target,
        vars = ed_vars,
        scale = scale_,
        center = center_,
        weights = weights_,
        id_col = id_col_,
        standard_eval = TRUE
      )
    )
    # return result
    out_df <- data.frame(matches, stringsAsFactors = FALSE) %>%
      magrittr::set_colnames(id_col_) %>%
      dplyr::left_join(df_ed, by = id_col_) %>%
      dplyr::pull(control_for_euc_val) %>%
      as.numeric()
  }

  control_for_map(
    df,
    fun = control_for_euc.calc_euc,
    var = id_col,
    tol = control[[2]],
    name = name,
    standard_eval = TRUE
  )
}
