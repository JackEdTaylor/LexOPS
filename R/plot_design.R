#' Plot the generated stimuli's design
#'
#' Takes the output from `generate()` or `long_format()`, and plots conditions' distributions on numeric variables used in the generate pipeline (i.e. indepdent variables, controls).
#'
#' @param df Output from `generate()` or `long_format()`
#' @param include A string indicating which variables to include in the plot. This can be those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Alternatively, this can be a character vector of the variables that should be plotted, that were in the original dataframe. Default is `"design"`.
#' @param dodge_width The width to give to `ggplot2::position_dodge` (default is 0.2)
#' @param point_size Size of points (default = 0.75)
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
#' plot_design(stim)
#'
#' @export

plot_design <- function(df, include = "design", dodge_width = 0.2, point_size = 0.75, line_width = 1) {
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_design()`")
  # ensure is in long format
  if (is.null(LexOPS_attrs$is.long_format)) df <- LexOPS::long_format(df)

  # get vector of splits (IVs)
  splits <- sapply(LexOPS_attrs$splits, dplyr::first)

  # remove random splits
  if (!is.null(LexOPS_attrs$random_splits)) {
    splits <- splits[-LexOPS_attrs$random_splits]
  }

  # get vector of control variables
  controls <- c( sapply(LexOPS_attrs$controls, dplyr::first), sapply(LexOPS_attrs$control_functions, dplyr::first) )

  # get df which contains all the original variables for the generated stimuli
  meta_df <- LexOPS_attrs$meta_df

  # convert to numeric if appropriate
  meta_df <- suppressWarnings(dplyr::mutate_if(meta_df, function(x) all(!is.na(as.numeric(x))), as.numeric))

  # join this to df
  plot_df <- dplyr::select(df, c(item_nr, condition, string)) %>%
    dplyr::right_join(meta_df, by = "string")

  # factor vector of variables to plot
  plot_vars <- if (all(include == "design")) {
    c(splits, controls)
  } else if (all(include == "splits")) {
    splits
  } else if (all(include == "controls")) {
    controls
  } else {
    include
  }

  # flatten to vector
  plot_vars <- unlist(plot_vars)

  # remove non-numeric variables
  plot_vars <- plot_vars[sapply(meta_df[plot_vars], is.numeric)]

  # function to recode a variable name as a heading
  plot_vars_headings <- sapply(plot_vars, function(v) {
    measure <- LexOPS::var_to_measure(v, first_cite = FALSE, title_caps = TRUE, standard_eval = TRUE)
    source <- LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)
    out <- if (nchar(measure) == 0 & nchar(source) == 0) sprintf("%s\n", v) else sprintf("%s\n%s", measure, source)
    out
  }, USE.NAMES = FALSE)

  # generate the point positions
  point_pos <- ggplot2::position_dodge(dodge_width)

  # plot the numeric variables
  plot_df %>%
    dplyr::rename_at(plot_vars, ~ plot_vars_headings) %>%
    tidyr::gather("variable", "value", plot_vars_headings) %>%
    ggplot2::ggplot(ggplot2::aes(x = condition, y = value)) +
    ggplot2::geom_violin(colour = NA, fill = "grey", alpha = 0.5) +
    ggplot2::geom_point(ggplot2::aes(colour = as.factor(item_nr)), position = point_pos, alpha = 0.75, size = point_size) +
    ggplot2::geom_line(ggplot2::aes(group = as.factor(item_nr), colour = as.factor(item_nr)), position = point_pos, alpha = 0.25, size = line_width) +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Condition", y = "Value")

}
