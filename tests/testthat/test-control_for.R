context("control_for")

testthat::test_that("errors", {
  testthat::expect_error(
    lexops %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2),
    "Unknown split conditions column! Make sure you run split_by() before control_for().",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>%
      split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2, cond_col = "splitID"),
    "No columns found for the manually defined cond_col 'splitID'",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>% control_for(example, -1:1),
    "'example' not in df?",
    fixed = TRUE
  )
})
