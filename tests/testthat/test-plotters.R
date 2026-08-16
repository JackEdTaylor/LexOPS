context("plotting functions")

# setup ----
set.seed(1)
eg_df <- data.frame(
  id = as.character(1:100),
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = factor(sample(c("a", "b", "c"), 100, replace=TRUE)),
  e = sample(1:5, 100, replace=TRUE),
  f = sample(c("zzzz", "zzza", "zzaa", "zaaa", "aaaa", "zzzzz"), 100, replace = TRUE),
  g = sample(c("yyyy", "yyya", "yyaa", "yaaa", "aaaa", "yyyyy"), 100, replace = TRUE)
)

stim1 <- eg_df |>
  set_options(id_col = "id") |>
  split_by(a, -5:-0.1 ~ 0.1:5) |>
  control_for(d) |>
  generate(5, silent=TRUE)

# plot_iterations() ----
testthat::test_that("plot_iterations()", {
  # doesn't fail
  testthat::expect_no_error(
    plot_iterations(stim1)
  )
  # is a ggplot2 object
  testthat::expect_s3_class(
    plot_iterations(stim1),
    "ggplot2::ggplot"
  )
  # error if not a LexOPS_pipeline object
  testthat::expect_error(
    plot_iterations(eg_df),
    regexp = "Must run `generate()` on `df` before using `plot_design()`",
    fixed = TRUE
  )
})

# plot_design() ----
testthat::test_that("plot_design()", {
  # doesn't fail
  testthat::expect_no_error(
    plot_design(stim1)
  )
  # is a ggplot2 object
  testthat::expect_s3_class(
    plot_design(stim1),
    "ggplot2::ggplot"
  )
  # warn about missing attributes
  testthat::expect_warning(
    {
      tryCatch(
        {
          stim2 <- stim1
          attr(stim2, "LexOPS_info") <- NULL
          plot_design(stim2)
        },
        error = function(e) {}
      )
    },
    "Attributes missing \\- will try to add attributes"
  )
  # can subset to splits, controls, or specific variables
  testthat::expect_no_error(
    plot_design(stim1, include="splits")
  )
  testthat::expect_no_error(
    plot_design(stim1, include="controls")
  )
  testthat::expect_no_error(
    plot_design(stim1, include=c("a", "b"))
  )
  # still works when random splits are included
  testthat::expect_no_error({
    stim2 <- eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      split_random(2, equal_size=TRUE) |>
      control_for(d) |>
      generate(5, silent=TRUE)

    plot_design(stim2)
  })
})

# plot_sample() ----
testthat::test_that("plot_sample()", {
  # doesn't fail
  testthat::expect_no_error(
    plot_sample(stim1)
  )
  # is a ggplot2 object
  testthat::expect_s3_class(
    plot_sample(stim1),
    "ggplot2::ggplot"
  )
  # warn about missing attributes
  testthat::expect_warning(
    {
      tryCatch(
        {
          stim2 <- stim1
          attr(stim2, "LexOPS_info") <- NULL
          plot_sample(stim2, id_col="id")
        },
        error = function(e) {}
      )
    },
    "Attributes missing. Will try to add attributes",
    fixed = TRUE
  )
  # can subset to splits, controls, or specific variables
  testthat::expect_no_error(
    plot_sample(stim1, include="splits")
  )
  testthat::expect_no_error(
    plot_sample(stim1, include="controls")
  )
  testthat::expect_no_error(
    plot_sample(stim1, include=c("a", "b"))
  )
})
