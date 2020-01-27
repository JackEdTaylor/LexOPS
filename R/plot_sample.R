#' Plot how well the stimuli represent the underlying population of words given
#'
#' Takes the output from `generate()` or `long_format()`, and plots distributions on numeric variables used in the generate pipeline (i.e. indepdent variables, controls), collapsed across conditions, relative to the underlying distribution.Alternatively, distributions of any numeric variables in the original dataframe can be queried.
#'
#' @param df Output from `generate()` or `long_format()`
#' @param include A string indicating which variables to include in the plot. This can be those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Alternatively, this can be a character vector of the variables that should be plotted, that were in the original dataframe. Default is `"design"`.
#' @param force Logical, should the function be forced to try and work if attributes are missing (default is `TRUE`)? If `TRUE`, will expect the dataframe to have a structure similar to that produced by `long_format()`, where `condition` is character or factor, and `item_nr` is numeric or factor. Other variables will be plot-able if given to the `include` argument.
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
#' plot_sample(stim)
#'
#' @export

plot_sample <- function(df, include = "design", force = TRUE) {
  # get attributes
  if (is.null(attr(df, "LexOPS_attrs"))) {
    if (force) {
      warning("Attributes missing. Will try to add attributes")
      LexOPS_attrs <- list()
      LexOPS_attrs$generated <- TRUE
      LexOPS_attrs$is.long_format <- TRUE
      if (is.null(LexOPS_attrs$meta_df)) LexOPS_attrs$meta_df <- dplyr::select(df, -condition, -item_nr)
    } else {
      LexOPS_attrs <- NULL
    }
  } else {
    LexOPS_attrs <- attr(df, "LexOPS_attrs")
  }
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_sample_rep()` (try `force = TRUE`?).")
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

  # get the original df, recording whether each possible candidate was selected
  plot_df <- dplyr::select(meta_df, string, plot_vars) %>%
    dplyr::mutate(is_stim = dplyr::if_else(string %in% df$string, "Generated Stimuli", "Unused Candidates")) %>%
    dplyr::mutate(is_stim = factor(is_stim, levels = c("Unused Candidates", "Generated Stimuli")))

  # plot the result
  plot_df %>%
    dplyr::rename_at(plot_vars, ~ plot_vars_headings) %>%
    tidyr::gather("variable", "value", plot_vars_headings) %>%
    ggplot2::ggplot(ggplot2::aes(value, fill = is_stim, alpha = is_stim)) +
    ggplot2::geom_density() +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    ) +
    ggplot2::labs(x = "Condition", y = "Value") +
    ggplot2::scale_fill_manual(values = c("lightgrey", "blue")) +
    ggplot2::scale_alpha_manual(values = c(1, 0.25))
}
