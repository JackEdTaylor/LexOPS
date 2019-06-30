#' Get a box type's colour
#'
#' Simple function for getting the visualisation colour (in hex) for different coloured boxes. Colours are only accurate for the LexOPS shiny app.
#'
#' @param box_type A character vector (one of: "primary", "warning", "Success", "danger", and "info"), specifying which kind of box to generate the colour for.
#'
#' @return The colour;s hex value.
#'
#' @examples
#'
#' #get_box_colour("primary")

get_box_colour <- function(box_type) {
  switch(box_type,
         "primary"="#3c8dbc",
         "warning"="#f39c12",
         "success"="#00a65a",
         "danger"="#dd4b39",
         "info"="#641e68")
}

#' Get sensible slider values
#'
#' Function for converting a numeric vector into recommended arguments for `shiny::sliderInput()`, for sliders in LexOPS.
#'
#' @param numeric_vec The numeric vector which the slider will be for.
#' @param n_levels How many sliders are to be generated for this variable?
#' @param is_tolerance Logical; is the slider for specifying tolerances (e.g. for matching)? Default is `FALSE`.
#'
#' @return A list object, containing the following reccommended argument values for `shiny::sliderInput()`: \itemize{
#'  \item min
#'  \item max
#'  \item value
#'  \item step
#' }
#'
#' @examples
#'
#' # for matching by frequency (e.g. control for...)
#' #sensible_slider_vals(LexOPS::lexops$Zipf.SUBTLEX_UK, 1, TRUE)
#'
#' # for 3 levels of arousal (e.g. split by...)
#' #sensible_slider_vals(LexOPS::lexops$AROU.Warriner, 3)
#'
#' # for filtering by word prevalence (e.g. control for...)
#' #sensible_slider_vals(LexOPS::lexops$PREV.Brysbaert, 1)
#'
#' @importFrom graphics hist
#' @importFrom stats quantile

sensible_slider_vals <- function(numeric_vec, n_levels, is_tolerance = FALSE) {
  # filter out NaN and NA values
  numeric_vec <- numeric_vec[!is.na(numeric_vec) & !is.nan(numeric_vec)]

  # where type can be "filter" or "tolerance"
  step <- if (is.integer(numeric_vec)) 1 else round(min(diff(hist(numeric_vec, 50, plot = FALSE)$breaks)), digits=4)
  # functions to calculate the floor to..., and ceiling to... (e.g. fl(0.15, 0.1) gives 0.1; the floor of 0.15 to the nearest 0.1)
  fl <- function(x, to=step) to*floor(x/to)
  ce <- function(x, to=step) to*ceiling(x/to)

  pct_quant <- quantile(numeric_vec, c(0, 0.025, 0.1, 0.45, 0.55, 0.9, 0.975, 1))

  value <- if (n_levels == 1) {
    if (!is_tolerance) {
      c(fl(pct_quant["2.5%"]), ce(pct_quant["97.5%"]))
    } else {
      if (is.integer(numeric_vec)) c(0, 0) else c(-step, step)
    }
  } else {
    if (is_tolerance) stop("n_levels cannot be >1 if is_tolerance is TRUE")
    if (n_levels == 2) {
      list(c(fl(pct_quant["0%"]), ce(pct_quant["10%"])), c(fl(pct_quant["90%"]), ce(pct_quant["100%"])))
    } else if (n_levels == 3) {
      list(c(fl(pct_quant["0%"]), ce(pct_quant["10%"])), c(fl(pct_quant["90%"]), ce(pct_quant["100%"])), c(fl(pct_quant["45%"]), ce(pct_quant["55%"])))
    } else if (n_levels > 3) {
      list(c(fl(pct_quant["0%"]), ce(pct_quant["10%"])), c(fl(pct_quant["90%"]), ce(pct_quant["100%"])), c(fl(pct_quant["45%"]), ce(pct_quant["55%"]))) %>%
        append(rep(list(c(-step, step)), n_levels-3))
    }
  }

  value <- if (is.list(value)) lapply(value, unname) else unname(value)

  list(
    min = ifelse(is_tolerance, -step*10, fl(min(numeric_vec))),
    max = ifelse(is_tolerance, step*10, ce(max(numeric_vec))),
    step = ifelse(is_tolerance, ifelse(is.integer(numeric_vec), step, step/2), step),
    value = value
  )
}

#' Generate a box's visualisation
#'
#' Function for generating a visualisation for a box's selected options
#'
#' @param var Character vector specifying which variable is selected
#' @param box_type Which type of box is it? (one of: "primary", "warning", "Success", "danger", and "info")
#' @param tol The area which should be highlighted. Can be a numeric vector of length two, a character vector of acceptable categories, or a list of such vectors. Each item in a `tol` list specifies one level.
#' @param match_string If the variable is being matched for, which string is it relative to. Leave as `NA` if not a matching box.
#' @param shade_label A character vector specifying what to label the levels. Order should match that of `tol`. Leave as `NA` if not doing a split.
#' @param shade_relative If `TRUE`, the value of tol is taken to be relative to the value for match_string. If `FALSE`, the raw values of tol are used. If `match_string` is not defined, `shade_relative` has no effect. Default is `TRUE`.
#' @param df A dataframe. Default is `LexOPS::lexops`.
#'
#' @return A ggplot/ggwordcloud object with the required visualisation.
#'
#' @examples
#'
#' #box_vis("CMU.PrN", "warning", c(-1, 1), "test")
#'
#' #box_vis("CMU.PrN", "warning", list(c(1, 1), c(3, 3)), shade_label = c("A1", "A2"))
#'
#' #box_vis("AROU.Glasgow_Norms", "info", c(-1, 0.5), match_string = "thicket")
#'
#' #box_vis("Rhyme.eSpeak.br", "danger", match_string = "thicket")
#'
#' #box_vis("PoS.SUBTLEX_UK", "warning", match_string = "laura")
#'
#' #box_vis("PoS.ELP", "warning")
#'
#' # ignore shade_label for categorical histogram?
#' #box_vis("PoS.SUBTLEX_UK", "warning", list("noun", "verb"), shade_label = c("B1", "B2"))

box_vis <- function(var, box_type = "primary", tol = NA, match_string = NA, shade_label = NA, shade_relative = TRUE, df = LexOPS::lexops) {

  match_string_val <- if (is.na(match_string)) NA else df[[var]][df$string==match_string]

  # filter out missing values to suppress warnings
  df <- dplyr::filter(df, !is.na(!!(dplyr::sym(var))))

  # ensure `shade` is a list
  shade <- if (!is.list(tol)) list(tol) else tol

  if (is.numeric(df[[var]])) {
    # if integer histogram, else density
    if (all(df[[var]] %% 1 == 0, na.rm = TRUE)) {
      pl <- box_vis.histogram(var, box_type, df)
      shade_padding <- 0.5
    } else {
      pl <- box_vis.density(var, box_type, df)
      shade_padding <- 0
    }
    # add red line for match_string_val if necessary
    if (!is.na(match_string_val)) {
      pl <- pl + ggplot2::geom_vline(xintercept=match_string_val, colour="red", size=1.25)
      # also adjust the values in `shade` to make them relative to `match_string_val`
      if (shade_relative) shade <- lapply(shade, function(i) match_string_val + i)
    }
    # shade in the selected tolerance
    if (!all(is.na(tol))) {
      shade_i_iter <- 0
      for (shade_i in shade) {
        shade_i_iter <- shade_i_iter + 1
        pl <- pl +
          ggplot2::annotate("rect",
                            xmin=shade_i[1]-shade_padding,
                            xmax=shade_i[2]+shade_padding,
                            ymin=-Inf, ymax=Inf,
                            alpha=0.4, colour=NA)
        if (all(!is.na(shade_label))) {
          pl <- pl +
            ggplot2::annotate("label",
                              x=shade_i[1]+((shade_i[2] - shade_i[1])/2), y=Inf,
                              label=shade_label[shade_i_iter],
                              vjust=1.5, fontface="bold", fill="black",
                              colour=get_box_colour(box_type))
        }
      }
    }
    pl <- pl + ggplot2::labs(y=NULL, x=NULL) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::theme_minimal()
  } else {
    # categorical visualisations here

    # get the type of measure
    var_measure <- var_to_measure(var, first_cite = FALSE, standard_eval = TRUE)

    # create the appropriate visualisation
    if (var_measure == "rhyme") {
      if (is.na(match_string)) {
        rhyme_word <- "rhyme"
        rhyme_word_val <- df[[var]][df$string==rhyme_word]
      } else {
        rhyme_word <- match_string
        rhyme_word_val <- match_string_val
      }
      pl <- box_vis.rhyme(var, box_type, rhyme_word, rhyme_word_val, df)
    } else {
      cat_to_highlight <- if (!is.na(match_string_val)) {
        match_string_val
      } else if (!all(is.na(tol))) {
        tol
      } else {
        c()
      }
      pl <- box_vis.categorical(var, box_type, cat_to_highlight, df)
    }
  }

  pl
}

box_vis.histogram <- function(var, box_type, df) {
  bin_vals <- sort(unique(df[[var]]))
  bin_interval <- min(abs(bin_vals - c(bin_vals[2:length(bin_vals)], 2)))
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = !!(dplyr::sym(var)))) +
    ggplot2::geom_histogram(binwidth=bin_interval, fill=get_box_colour(box_type), alpha=0.5, colour=NA)
}

box_vis.density <- function(var, box_type, df) {
  df %>%
    ggplot2::ggplot(ggplot2::aes(x = !!(dplyr::sym(var)))) +
    ggplot2::geom_density(fill=get_box_colour(box_type), alpha=0.5, colour=NA)
}

box_vis.rhyme <- function(var, box_type, rhyme_word, rhyme_word_val, df) {
  df %>%
    dplyr::select(string, !!(dplyr::sym(var)), PK.Brysbaert) %>%
    dplyr::filter(!!(dplyr::sym(var))==rhyme_word_val & PK.Brysbaert>=0.75 & string!=rhyme_word) %>%
    dplyr::sample_n(ifelse(nrow(.)<15, nrow(.), 15)) %>%
    dplyr::mutate(wordcloudsize=1, wordcloudalpha=0.9) %>%
    dplyr::add_row(string=rhyme_word, wordcloudsize=5, wordcloudalpha=1, .before=1) %>%
    ggplot2::ggplot(ggplot2::aes(label=string, size=wordcloudsize, alpha=wordcloudalpha)) +
    ggwordcloud::geom_text_wordcloud(colour=get_box_colour(box_type), rm_outside=T, shape="circle") +
    ggplot2::scale_size_area(max_size=20) +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme_minimal()
}

box_vis.categorical <- function(var, box_type, cat_to_highlight, df) {
  df %>%
    dplyr::filter(!is.na(!!(dplyr::sym(var)))) %>%
    dplyr::group_by(!!(dplyr::sym(var))) %>%
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::mutate(x = factor(!!(dplyr::sym(var)), levels = !!(dplyr::sym(var)))) %>%
    dplyr::mutate(alpha_level = ifelse(!!(dplyr::sym(var)) %in% cat_to_highlight, 0.75, 0.25)) %>%
    ggplot2::ggplot(ggplot2::aes(x = x, y = n, alpha = alpha_level)) +
    ggplot2::geom_bar(stat = "identity", fill=get_box_colour(box_type), colour=NA) +
    ggplot2::scale_alpha_identity() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(y=NULL, x=NULL)
}

#' Generate a question mark plot
#'
#' Function for generating a visualisation of question marks surrounding a message
#'
#' @param message A string; the message to display.
#' @param box_type The type of box the visualisation is presented in.
#'
#' @return A ggplot/ggwordcloud object with the required visualisation.
#'
#' @examples
#'
#' #box_vis.question_marks("Unknown!", "info")

box_vis.question_marks <- function(message, box_type) {
  dplyr::tibble(qm = c(message, rep("?", 99)),
         wordcloudsize = c(6, rep(1, 99)),
         wordcloudalpha = c(1, rep(0.6, 99))) %>%
    ggplot2::ggplot(aes(label=qm, size=wordcloudsize, alpha=wordcloudalpha)) +
    ggwordcloud::geom_text_wordcloud(colour=get_box_colour(box_type), rm_outside=T, shape='circle') +
    ggplot2::theme_minimal() +
    ggplot2::scale_size_area(max_size=15) +
    ggplot2::scale_alpha_identity()
}
