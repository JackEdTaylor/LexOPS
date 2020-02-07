context("generate")

testthat::test_that("general warnings", {
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
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      control_for(Length) %>%
      generate(n = 1000, match_null = "balanced"),
    "No tolerance given for numeric variable 'Length', will control for exactly.",
    fixed = TRUE
  )

  testthat::expect_warning(
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = "all", match_null = "balanced"),
    '`match_null="balanced"` may not work when `n="all"`. Check distributions of match_null in the output.',
    fixed = TRUE
  )
})

testthat::test_that("general errors", {
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
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
      control_for(Length, 0:0) %>%
      generate(n = 1000, match_null = "irreverent"),
    "Unknown match_null; expected \"inclusive\", \"random\", \"balanced\", \"first\", or a specific condition (e.g. \"A2_B1_C1\")",
    fixed = TRUE
  )
})

testthat::test_that("reproducibility", {
  # non-standard vs. non-standard (seed test)
  testthat::expect_identical(
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, seed = 42),
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, seed = 42)
  )
  # standard vs. standard (seed test)
  testthat::expect_identical(
    lexops %>%
      split_by("CNC.Brysbaert", list(c(1, 2), c(4, 5)), standard_eval = TRUE) %>%
      split_by("VAL.Warriner", list(c(1, 3), c(4.5, 5.5), c(7, 9)), standard_eval = TRUE) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.25, 0.25), standard_eval = TRUE) %>%
      control_for("Length", c(0, 0), standard_eval = TRUE) %>%
      control_for("PoS.BNC.Written", standard_eval = TRUE) %>%
      generate(n = 10, seed = 42),
    lexops %>%
      split_by("CNC.Brysbaert", list(c(1, 2), c(4, 5)), standard_eval = TRUE) %>%
      split_by("VAL.Warriner", list(c(1, 3), c(4.5, 5.5), c(7, 9)), standard_eval = TRUE) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.25, 0.25), standard_eval = TRUE) %>%
      control_for("Length", c(0, 0), standard_eval = TRUE) %>%
      control_for("PoS.BNC.Written", standard_eval = TRUE) %>%
      generate(n = 10, seed = 42)
  )
  # non-standard vs. standard (transferability test)
  testthat::expect_identical(
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, seed = 42),
    lexops %>%
      split_by("CNC.Brysbaert", list(c(1, 2), c(4, 5)), standard_eval = TRUE) %>%
      split_by("VAL.Warriner", list(c(1, 3), c(4.5, 5.5), c(7, 9)), standard_eval = TRUE) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.25, 0.25), standard_eval = TRUE) %>%
      control_for("Length", c(0, 0), standard_eval = TRUE) %>%
      control_for("PoS.BNC.Written", standard_eval = TRUE) %>%
      generate(n = 10, seed = 42)
  )
  # hybrid vs. hybrid (mixed transferability test)
  testthat::expect_identical(
    lexops %>%
      split_by("CNC.Brysbaert", list(c(1, 2), c(4, 5)), standard_eval = TRUE) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for("Zipf.SUBTLEX_UK", c(-0.25, 0.25), standard_eval = TRUE) %>%
      control_for(Length, 0:0) %>%
      control_for("PoS.BNC.Written", standard_eval = TRUE) %>%
      generate(n = 10, seed = 42),
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by("VAL.Warriner", list(c(1, 3), c(4.5, 5.5), c(7, 9)), standard_eval = TRUE) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for("Length", c(0, 0), standard_eval = TRUE) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, seed = 42)
  )
})

testthat::test_that("controls", {
  # test that categorical controls are applied correctly
  testthat::expect_equal(
    match_fail_count <- lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 1000, match_null = "balanced") %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "string") %>%
      dplyr::left_join(lexops, by = "string") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(length(unique(PoS.BNC.Written)) != 1) %>%
      nrow(),
    0
  )
  # test that exact numeric controls are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Length, 0:0) %>%
      generate(n = 1000, match_null = "balanced") %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "string") %>%
      dplyr::left_join(lexops, by = "string") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(length(unique(Length)) != 1) %>%
      nrow(),
    0
  )
  # test that inexact integer numeric controls are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Length, -1:1) %>%
      generate(n = 1000, match_null = "balanced") %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "string") %>%
      dplyr::left_join(lexops, by = "string") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::summarise(max_diff = abs(max(Length) - min(Length))) %>%
      dplyr::filter(max_diff > 1) %>%
      nrow(),
    0
  )
  # test that inexact float numeric controls are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.BNC.All, -0.2:0.2) %>%
      generate(n = 1000, match_null = "balanced") %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "string") %>%
      dplyr::left_join(lexops, by = "string") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::summarise(max_diff = abs(max(Zipf.BNC.All) - min(Zipf.BNC.All))) %>%
      dplyr::filter(max_diff > 0.2) %>%
      nrow(),
    0
  )
})
