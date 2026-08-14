context("split_by() errors")

# setup ----
set.seed(99)
dat <- data.frame(
  a = runif(100, 1, 4),
  b = factor(sample(c("a", "b", "c"), 100, replace=TRUE))
)

# general errors ----
testthat::test_that("general errors", {
  # ensure that numeric levels are in order
  testthat::expect_error({
    # ignore expected warning about missing levels
    # (because there are no observations for 2 < x < 1)
    suppressWarnings(
      dat |>
        split_by(a, 2:1 ~ 2.1:2.5 ~ 3.2:4)
    )
  },
  "lower bounds must be lower than upper bounds",
  fixed = TRUE
  )

  # check that the expected warning was indeed what was produced
  testthat::expect_warning({
    # ignore expected warning about missing levels
    # (because there are no observations for 2 < x < 1)
    tryCatch(
      dat |>
        split_by(a, 2:1 ~ 2.1:2.5 ~ 3.2:4),
      error = function(e) {}
    )
  },
  "No entries could be found for some levels. Check all levels of a are possible.",
  fixed = TRUE
  )

  # ensure that numeric levels are exclusive
  testthat::expect_error({
    dat |>
      split_by(a, 1:2 ~ 2:2.5 ~ 3.2:4)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

  # test whether non-exclusive levels are still detected when out of order
  testthat::expect_error({
    dat |>
      split_by(a, 1:2 ~ 3.2:4 ~ 2:2.5)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

  # get error when missing categorical levels
  testthat::expect_error(
    # suppress expected additional warning about missing levels
    suppressWarnings(
      dat |>
        set_options(id_col = "id") |>
        split_by(b, "a" ~ "b" ~ "MISSING")
    ),
    regexp = "not all breaks are existing factor levels",
    fixed = TRUE
  )

  # get informative warning when missing categorical levels
  testthat::expect_warning(
    # suppress expected additional warning about missing levels
    tryCatch(
      dat |>
        set_options(id_col = "id") |>
        split_by(b, "a" ~ "b" ~ "MISSING"),
      error=function(e){}
    ),
    regexp = "No entries could be found for some levels. Check all levels of b are possible.",
    fixed = TRUE
  )

  # get informative warning when missing numeric levels
  testthat::expect_warning(
    dat |>
      set_options(id_col = "id") |>
      split_by(a, -2:-0.5 ~ 2:5),
    regexp = "No entries could be found for some levels. Check all levels of a are possible.",
    fixed = TRUE
  )

})
