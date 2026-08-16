context("shiny functions")

# setup ----
set.seed(77)
eg_df <- data.frame(
  id = as.character(1:100),
  a = rnorm(100),
  b = factor(sample(c("a", "b", "c"), 100, replace=TRUE))
)

# get_box_colou()r ----
testthat::test_that("get_box_colour()", {
  # the 5 expected box colours are unique
  testthat::expect_length(
    c("primary", "warning", "success", "danger", "info") |>
      lapply(get_box_colour) |>
      unlist() |>
      unique(),
    5
  )
  # the 5 expected box colours are valid colours
  testthat::expect_all_true(
    lapply(c("primary", "warning", "success", "danger", "info"), function(box_type) {
      tryCatch(is.matrix(col2rgb(get_box_colour(box_type))), error = function(e) FALSE)
    }) |>
      unlist()
  )
})

# sensible_slider_values() ----
testthat::test_that("sensible_slider_values()", {
  # the returned list contains the expected fields
  testthat::expect_contains(
    names( sensible_slider_vals(rnorm(100), 1) ),
    c("min", "max", "step", "value")
  )
  # requesting just 1 level returns the min and max as a vector
  testthat::expect_length(
    sensible_slider_vals(rnorm(100), n_levels=1, is_tolerance=FALSE)$value,
    2
  )
  testthat::expect_true({
    vals <- sensible_slider_vals(rnorm(100), n_levels=1, is_tolerance=FALSE)$value
    vals[[1]] < vals[[2]]
  })
  # requesting 2 levels returns values for 3 levels
  testthat::expect_length(
    sensible_slider_vals(rnorm(100), n_levels=2, is_tolerance=FALSE)$value,
    2
  )
  # requesting 3 levels returns values for 3 levels
  testthat::expect_length(
    sensible_slider_vals(rnorm(100), n_levels=3, is_tolerance=FALSE)$value,
    3
  )
  # requesting 5 levels returns values for 5 levels
  testthat::expect_length(
    sensible_slider_vals(rnorm(100), n_levels=5, is_tolerance=FALSE)$value,
    5
  )
  # if requesting for a tolerance, the min and max should be +/- the step*2
  testthat::expect_true({
    out <- sensible_slider_vals(rnorm(100), n_levels=1, is_tolerance=TRUE)
    out$value[1] == -out$step*2 & out$value[2] == out$step*2
  })
  # if requesting something other than a tolerance, the min and max should be at least the actual min and max
  testthat::expect_true({
    x <- rnorm(100)
    out <- sensible_slider_vals(x, n_levels=1, is_tolerance=FALSE)
    out$min <= min(x) & out$max >= max(x)
  })
  # can't request more than 1 level if a tolerance
  testthat::expect_error(
    sensible_slider_vals(rnorm(100), n_levels=5, is_tolerance=TRUE),
    regexp = "n_levels cannot be >1 if is_tolerance is TRUE",
    fixed = TRUE
  )
})

# box_vis() ----
testthat::test_that("box_vis()", {
  # the various options run without error and return ggplot2 objects
  testthat::expect_s3_class(
    box_vis(var="a", df=eg_df),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis(var="b", df=eg_df),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis(var="b", df=eg_df, tol=c("a", "b"), cat_vis="tol"),
    "ggplot2::ggplot"
  )
  # these tests require the lexops dataset and dplyr
  testthat::skip_if_not_installed("lexopsdata")
  testthat::skip_if_not_installed("dplyr")
  testthat::expect_s3_class(
    box_vis(var="Zipf.SUBTLEX_UK", df=lexops, match_string="thicket", box_type="warning", tol=c(-0.2, 0.2), shade_relative=TRUE),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis("CMU.PrN", "warning", df=lexops, list(c(1, 1), c(3, 3)), shade_label = c("A1", "A2")),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis("Zipf.SUBTLEX_US", "primary", df=lexops, list(c(1, 2.5), c(4, 7)), shade_label = c("A1", "A2")),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis(var="PoS.SUBTLEX_UK", df=lexops, match_string="thicket", box_type="success", cat_vis="match_string_val"),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis(var="PoS.SUBTLEX_UK", df=lexops, box_type="info", cat_vis="tol", tol=c("noun", "verb")),
    "ggplot2::ggplot"
  )
  # these tests require ggwordcloud
  testthat::skip_if_not_installed("ggwordcloud")
  testthat::expect_s3_class(
    box_vis(var="Rhyme.eSpeak.br", df=lexops, match_string="flipper"),
    "ggplot2::ggplot"
  )
  testthat::expect_s3_class(
    box_vis.question_marks("Unknown!", "info"),
    "ggplot2::ggplot"
  )
})
