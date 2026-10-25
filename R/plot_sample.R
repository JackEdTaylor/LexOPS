#' Plot how well the stimuli represent the underlying population of words given
#'
#' Takes the output from `generate()` or `long_format()`, and plots distributions on numeric variables used in the generate pipeline (i.e. indepdent variables, controls), collapsed across conditions, relative to the underlying distribution. Alternatively, distributions of any specific numeric variables in the original dataframe can be queried.
#'
#' @param df Output from `generate()` or `long_format()`
#' @param include A character vector indicating which variables to include in the plot. This can be those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Alternatively, this can be a character vector of the variables that should be plotted, that were in the original dataframe. Default is `"design"`.
#' @param force Logical, should the function be forced to try and work if attributes are missing (default is `TRUE`)? If `TRUE`, will expect the dataframe to have a structure similar to that produced by `long_format()`, where `condition` is character or factor, and `item_nr` is numeric or factor. Other variables will be plot-able if given to the `include` argument.
#' @param id_col A character vector specifying the column identifying unique observations (e.g. in `lexopsdata::lexops`, the `id_col` is `"string"`). Ignored unless `force=TRUE`, in which case it is required.
#' @importFrom stats ave
#' @importFrom utils head
#'
#' @return A ggplot object showing how conditions differ in independent variables, and are matched for in controls.
#'
#' @examplesIf rlang::is_installed("lexopsdata")
#' library(lexopsdata)
#'
#' stim <- lexops |>
#'   dplyr::filter(PK.Brysbaert >= .75) |>
#'   split_by(PoS.SUBTLEX_UK, "noun" ~ "adjective" ~ "verb") |>
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(Length, 0:0) |>
#'   generate(n = 50, match_null = "balanced")
#' plot_sample(stim)
#'
#' @export
#
plot_sample <- function(df, include = "design", force = TRUE, id_col = "string") {
  # get attributes
  if (is.null(attr(df, "LexOPS_info"))) {
    if (force) {
      warning("Attributes missing. Will try to add attributes")
      LexOPS_attrs <- list()
      LexOPS_attrs$generated <- TRUE
      LexOPS_attrs$is.long_format <- TRUE
      if (is.null(LexOPS_attrs$meta_df)) LexOPS_attrs$meta_df <- df[, setdiff(names(df), c("condition", "item_nr")), drop = FALSE]
    } else {
      LexOPS_attrs <- NULL
    }
  } else {
    LexOPS_attrs <- attr(df, "LexOPS_info")
    # get options from attributes
    if (!is.null(LexOPS_attrs$options)) {
      id_col <- LexOPS_attrs$options$id_col
    } else {
      id_col <- "string"
    }
  }
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_sample()` (or try `force = TRUE`?).")
  # ensure is in long format
  if (is.null(LexOPS_attrs$is.long_format)) df <- LexOPS::long_format(df)

  # get vector of splits (IVs)
  splits <- sapply(LexOPS_attrs$splits, function(x) x[[1]])

  # remove random splits
  if (!is.null(LexOPS_attrs$random_splits)) {
    splits <- splits[-LexOPS_attrs$random_splits]
  }

  # get vector of control variables
  controls <- c( sapply(LexOPS_attrs$controls, function(x) x[[1]]), sapply(LexOPS_attrs$control_functions, function(x) x[[1]]) )

  # get df which contains all the original variables for the generated stimuli
  meta_df <- LexOPS_attrs$meta_df

  # convert to numeric if appropriate
  num_cols_all <- colnames(meta_df)[vapply(meta_df, function(x) {
    !is.factor(x) && suppressWarnings(all(!is.na(as.numeric(x))))
  }, logical(1)) & colnames(meta_df) != id_col]
  if (length(num_cols_all) > 0) {
    for (nc in num_cols_all) meta_df[[nc]] <- suppressWarnings(as.numeric(meta_df[[nc]]))
  }

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

  # Keep variables that can be represented as numeric or categorical values
  plot_vars <- plot_vars[vapply(meta_df[plot_vars], function(x) {
    is.numeric(x) || is.factor(x) || is.character(x)
  }, logical(1))]

  # function to recode a variable name as a heading
  plot_vars_headings <- sapply(plot_vars, function(v) {
    measure <- LexOPS::var_to_measure(v, first_cite = FALSE, title_caps = TRUE, standard_eval = TRUE)
    source <- LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)
    out <- if (nchar(measure) == 0 & nchar(source) == 0) sprintf("%s\n", v) else sprintf("%s\n%s", measure, source)
    out
  }, USE.NAMES = FALSE)

  # get the original df, recording whether each possible candidate was selected
  plot_df_all <- meta_df[, c(id_col, plot_vars), drop = FALSE]
  plot_df_all$is_stim <- "All Candidates"

  plot_df_gen <- plot_df_all[plot_df_all[[id_col]] %in% df[[id_col]], , drop = FALSE]
  plot_df_gen$is_stim <- "Generated Items"

  plot_df <- rbind(plot_df_all, plot_df_gen)
  plot_df$is_stim <- factor(plot_df$is_stim, levels = c("All Candidates", "Generated Items"))

  # rename plot_vars in plot_df to headings
  for (i in seq_along(plot_vars)) {
    old <- plot_vars[i]
    new <- plot_vars_headings[i]
    if (old %in% names(plot_df)) names(plot_df)[names(plot_df) == old] <- new
  }

  categorical_values <- function(value, i) {
    if (is.factor(value)) {
      # keep only the observed levels in the candidates, but preserve the order
      observed_levels <- levels(value)[levels(value) %in% as.character(value)]
      category <- factor(as.character(value), levels = observed_levels)
    } else {
      # create new factor
      category <- factor(value, levels = unique(value[!is.na(value)]))
    }

    category_labels <- levels(category)
    # return list with metadata for plotting with
    list(
      category = category,
      category_labels = category_labels,
      # shift the locations by a large constant, and similarly add very large spacing, to make it more likely that categories get their own coordinates
      x_values = 1e9 + i * 1e6 + seq_along(category_labels),
      axis_values = rep(NA_real_, length(category_labels)),
      # add spacing between bars for categorical variables
      bar_width = rep(0.9, length(category_labels))
    )
  }

  # bin each variable and count observations (proportions) before faceting
  plot_data_list <- lapply(seq_along(plot_vars_headings), function(i) {
    h <- plot_vars_headings[i]
    value <- plot_df[[h]]

    # separate binning approaches for continuous numeric, integer numeric, and categorical numeric
    if (is.numeric(value)) {
      non_missing <- value[!is.na(value)]
      is_integer_numeric <- length(unique(non_missing)) <= 1L || all(non_missing == floor(non_missing))
      if (is_integer_numeric) {
        category <- factor(as.character(value), levels = as.character(sort(unique(non_missing))))
      } else {
        bin_breaks <- seq(min(non_missing), max(non_missing), length.out = 51)
        category <- cut(value, breaks = bin_breaks, include.lowest = TRUE)
      }

      category_labels <- if (is_integer_numeric) {
        levels(category)
      } else {
        rep(NA_character_, nlevels(category))
      }
      axis_values <- if (is_integer_numeric) {
        as.numeric(category_labels)
      } else {
        rep(NA_real_, nlevels(category))
      }
      x_values <- if (is_integer_numeric) {
        axis_values
      } else {
        head(bin_breaks, -1) + diff(bin_breaks) / 2
      }
      bar_width <- if (is_integer_numeric) {
        rep(0.9, length(category_labels))
      } else {
        # i.e., no gaps for continuous numeric
        rep(diff(bin_breaks)[1], nlevels(category))
      }
    } else {
      category_setup <- categorical_values(value, i)
      category <- category_setup$category
      category_labels <- category_setup$category_labels
      x_values <- category_setup$x_values
      axis_values <- category_setup$axis_values
      bar_width <- category_setup$bar_width
    }

    counts <- as.data.frame(table(category, plot_df$is_stim), stringsAsFactors = FALSE)
    names(counts) <- c("value", "is_stim", "n_obs")
    counts <- counts[counts$n_obs > 0, , drop = FALSE]
    category_index <- match(counts$value, levels(category))
    counts$x <- x_values[category_index]
    counts$bar_width <- bar_width[category_index]
    counts$axis_label <- category_labels[category_index]
    counts$axis_value <- axis_values[category_index]
    counts$n_obs <- counts$n_obs / ave(counts$n_obs, counts$is_stim, FUN = sum)
    counts$variable <- h
    counts
  })
  plot_data <- do.call(rbind, plot_data_list)
  categorical_data <- unique(plot_data[
    !is.na(plot_data$axis_label) & is.na(plot_data$axis_value),
    c("x", "axis_label")
  ])
  integer_data <- unique(plot_data[!is.na(plot_data$axis_value), c("x", "axis_value")])

  ggplot2::ggplot(plot_data, ggplot2::aes(
    xmin = .data$x - .data$bar_width / 2,
    xmax = .data$x + .data$bar_width / 2,
    ymin = 0,
    ymax = .data$n_obs,
    fill = .data$is_stim
  )) +
    # use geom_rect instead of geom_bar for control over individual bar widths (xmax-xmin)
    ggplot2::geom_rect(alpha = 0.75, colour = NA, position = "identity") +
    ggplot2::facet_wrap(ggplot2::vars(.data$variable), scales = "free") +
    ggplot2::scale_x_continuous(
      # separate breaks and labels approaches for categorical, integer, and continuous numeric variables
      breaks = function(limits) {
        cat_idx <- which(categorical_data$x >= min(limits) & categorical_data$x <= max(limits))
        if (length(cat_idx) > 0L) {
          selected <- if (length(cat_idx) <= 5L) cat_idx else cat_idx[seq(1, length(cat_idx), by = 10)]
          categorical_data$x[selected]
        } else if (any(integer_data$x >= min(limits) & integer_data$x <= max(limits))) {
          in_range <- integer_data[integer_data$x >= min(limits) & integer_data$x <= max(limits), ]
          if (nrow(in_range) <= 5L) {
            in_range$x
          } else {
            scales::breaks_pretty(n = 5)(range(in_range$axis_value))
          }
        } else {
          scales::breaks_pretty(n = 5)(limits)
        }
      },
      labels = function(breaks) {
        category_idx <- match(breaks, categorical_data$x)
        integer_idx <- match(breaks, integer_data$x)
        labels <- format(breaks, trim = TRUE, digits = 3)
        is_category <- !is.na(category_idx)
        is_integer <- !is.na(integer_idx)
        labels[is_category] <- categorical_data$axis_label[category_idx[is_category]]
        labels[is_integer] <- format(integer_data$axis_value[integer_idx[is_integer]], trim = TRUE)
        labels
      }
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    ) +
    ggplot2::labs(x = "Value", y = "Proportion of observations") +
    ggplot2::scale_fill_manual(values = c("black", "skyblue"))
}
