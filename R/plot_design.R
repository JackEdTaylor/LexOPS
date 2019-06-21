#' Plot the generated stimuli's design
#'
#' Takes the output from `generate()` or `long_format()`, and plots conditions' distributions on numeric variables used in the generate pipeline (i.e. indepdent variables, controls).
#'
#' @param df Output from `generate()` or `long_format()`
#' @param dodge_width The width to give to `ggplot2::position_dodge` (default is 0.2)
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
#' plot_design(stim)
#'
#' @export

plot_design <- function(df, dodge_width = 0.2) {
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_design()`")
  # ensure is in long format
  if (is.null(LexOPS_attrs$is.long_format)) df <- LexOPS::long_format(df)

  # get vector of splits (IVs)
  splits <- sapply(LexOPS_attrs$splits, dplyr::first)

  # get vector of control variables
  controls <- sapply(LexOPS_attrs$controls, dplyr::first)

  # factor vector of variables to plot
  plot_vars <- c(splits, controls)

  # remove non-numeric variables
  plot_vars <- plot_vars[sapply(df[plot_vars], is.numeric)]

  # function to recode a variable name as a heading
  plot_vars_headings <- sapply(plot_vars, function(v) {
    measure <- LexOPS::recode_measure(v, first_cite = FALSE, title_caps = TRUE, standard_eval = TRUE)
    source <- LexOPS::recode_corpus_apa(v, first_cite = FALSE, standard_eval = TRUE)
    sprintf("%s\n%s", measure, source)
  }, USE.NAMES = FALSE)

  # generate the point positions
  point_pos <- ggplot2::position_dodge(dodge_width)

  # plot the numeric variables
  df %>%
    dplyr::rename_at(plot_vars, ~ plot_vars_headings) %>%
    tidyr::gather("variable", "value", plot_vars_headings) %>%
    ggplot2::ggplot(ggplot2::aes(x = condition, y = value)) +
    ggplot2::geom_violin(colour = NA, fill = "grey", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(item_nr)), position = point_pos, alpha = 0.75) +
    ggplot2::geom_line(ggplot2::aes(group = as.factor(item_nr), colour = as.factor(item_nr)), position = point_pos, alpha = 0.25) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Condition", y = "Value")

}
