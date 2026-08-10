#' Plot the cumulative frequency of stimuli generated
#'
#' Plots iterations by number of items generated, when given a dataframe which has been passed through `generate()`.
#'
#' @param df Output from `generate()` or `long_format()`
#' @param line_width Thickness of lines (default = 1)
#'
#' @return A ggplot object showing how conditions differ in independent variables, and are matched for in controls.
#'
#' @examplesIf requireNamespace(c("lexopsdata"), quietly=TRUE)
#'
#' stim <- lexops |>
#'   dplyr::filter(PK.Brysbaert >= .75) |>
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(Length) |>
#'   generate(n = 50, match_null = "balanced")
#' plot_iterations(stim)
#'
#' @export

plot_iterations <- function(df, line_width = 1) {
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_info"))) list() else attr(df, "LexOPS_info")
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_design()`")

  # Build iteration dataframe with base R
  iterations <- seq_len(max(LexOPS_attrs$successful_iterations))
  df_iter <- data.frame(iteration = iterations, stringsAsFactors = FALSE)
  df_iter$was_successful <- as.integer(df_iter$iteration %in% LexOPS_attrs$successful_iterations)
  df_iter$stim_generated <- cumsum(df_iter$was_successful)

  ggplot2::ggplot(df_iter, ggplot2::aes(x = iteration, y = stim_generated)) +
    ggplot2::geom_line(linewidth = line_width) +
    ggplot2::labs(x = "Iteration", y = "Number of Items Generated (Cumulative)") +
    ggplot2::theme_bw()

}
