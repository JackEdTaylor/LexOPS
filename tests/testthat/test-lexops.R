context("LexOPS::lexops dataset")

# lexops dataset ----
testthat::test_that("LexOPS::lexops", {
  # can call lexops and it is dataframe
  testthat::expect_true(
    if (!rlang::is_installed("lexopsdata")) {
      # suppress warning about lexopsdata not being installed
      # (should still be an (empty) dataframe if not installed)
      suppressWarnings(is.data.frame(lexops))
    } else {
      is.data.frame(lexops)
    }
  )
  # only run the remaining tests if the lexopsdata package is installed
  testthat::skip_if_not_installed("lexopsdata")
  # has the expected number of rows
  testthat::expect_true(nrow(lexops)==262532)
  # can run a generate pipeline on the dataframe
  testthat::expect_equal(
    lexops |>
      dplyr::filter(PK.Brysbaert >= .75) |>
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
      control_for(Length, 0:0) |>
      generate(n = 500, match_null = "balanced", silent = TRUE) |>
      nrow(),
    500
  )
})
