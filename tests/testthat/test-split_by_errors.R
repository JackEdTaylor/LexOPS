context("split_by() errors")

# setup ----
dat <- data.frame(
  a = c(1.0, 1.1, 1.3, 1.9, 2, 2.1, 2.2, 2.9, 3, 3.1, 3.3)
)

# general errors ----
testthat::test_that("general errors", {
  testthat::expect_error({
    dat |>
      split_by(a, 2:1 ~ 2.1:2.5 ~ 3.2:4)
  },
  "lower bounds must be lower than upper bounds",
  fixed = TRUE
  )

  testthat::expect_error({
    dat |>
      split_by(a, 1:2 ~ 2:2.5 ~ 3.2:4)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

  # test whether still detected when out of order
  testthat::expect_error({
    dat |>
      split_by(a, 1:2 ~ 3.2:4 ~ 2:2.5)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

})
