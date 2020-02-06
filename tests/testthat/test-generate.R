context("generate")

testthat::test_that("warnings", {
  testthat::expect_warning(
    lexops %>%
      split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) %>%
      split_by(PoS.SUBTLEX_UK, "noun" ~ "verb") %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      generate(100000),
    "n is too large; requested n of 100000, but the condition with the fewest members has \\d+ entries. Ensure n < \\d+. Will generate as many stimuli as possible.",
    all = FALSE
  )

  testthat::expect_warning(
    lexops %>%
      dplyr::filter(PK.Brysbaert >= .75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      control_for(Length) %>%
      generate(n = 1000, match_null = "balanced"),
    "No tolerance given for numeric variable 'Length', will control for exactly.",
    fixed = TRUE
  )
})

testthat::test_that("errors", {
  testthat::expect_error(
    generate(20),
    "Expected df to be of class data frame, not numeric",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>%
      generate(20),
    "Could not identify split conditions column! Make sure you run split_by() before generate().",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>%
      split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      generate(n = "20"),
    "n must be numeric or a string of value \"all\"",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>%
      split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) %>%
      generate(n = 20, cond_col = "my_cond"),
    "No columns found for the manually defined cond_col 'my_cond'.",
    fixed = TRUE
  )

  testthat::expect_error(
    lexops %>%
      dplyr::filter(PK.Brysbaert >= .75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      control_for(Length, 0:0) %>%
      generate(n = 1000, match_null = "irreverent"),
    "Unknown match_null; expected \"inclusive\", \"random\", \"balanced\", \"first\", or a specific condition (e.g. \"A2_B1_C1\")",
    fixed = TRUE
  )
})

testthat::test_that("standard and non-standard evaluation equivalence, and seed reproducibility", {
  testthat::expect_identical(
    lexops %>%
      split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      generate(n = 20, seed = 42),
    lexops %>%
      split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) %>%
      generate(n = 20, seed = 42)
  )
})
