#' Plot the cumulative frequency of stimuli generated
#'
#' Plots iterations by number of items generated, when given a dataframe which has been passed through `generate()`.
#'
#' @param df Output from `generate()` or `long_format()`
#' @param line_width Thickness of lines (default = 1)
#'
#' @return A ggplot object showing how conditions differ in independent variables, and are matched for in controls.
#'
#' @examples
#'
#' stim <- lexops %>%
#'   dplyr::filter(PK.Brysbaert >= .75) %>%
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   control_for(Length) %>%
#'   generate(n = 50, match_null = "balanced")
#' plot_iterations(stim)
#'
#' @export

plot_iterations <- function(df, line_width = 1) {
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_info"))) list() else attr(df, "LexOPS_info")
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_design()`")

  # Create a tibble which will contain the result of each iteration
  dplyr::tibble(iteration = 1:max(LexOPS_attrs$successful_iterations)) %>%
    dplyr::mutate(was_successful = ifelse(iteration %in% LexOPS_attrs$successful_iterations, 1, 0)) %>%
    dplyr::mutate(stim_generated = cumsum(was_successful)) %>%
    ggplot2::ggplot(ggplot2::aes(x = iteration, y = stim_generated)) +
    ggplot2::geom_line(size = line_width) +
    ggplot2::labs(x = "Iteration", y = "Number of Items Generated (Cumulative)") +
    ggplot2::theme_bw()

}
