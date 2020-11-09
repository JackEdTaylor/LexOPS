context("LexOPS::lexops dataset")

# inbuilt dataset ----
testthat::test_that(
  # can call lexops and it is dataframe
  testthat::expect_true(is.data.frame(lexops)),
  # has the expected number of rows
  testthat::expect_true(nrow(lexops)==262532),
  # can run a generate pipelune on the dataframe
  testthat::expect_equal(
    lexops %>%
      dplyr::filter(PK.Brysbaert >= .75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      control_for(Length, 0:0) %>%
      generate(n = 500, match_null = "balanced", silent = TRUE) %>%
      nrow,
    500
  )
)
