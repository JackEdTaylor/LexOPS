context("generate")

# general warnings ----
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

# general errors ----
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

# reproducibility ----
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

# splits ----
testthat::test_that("splits", {
  # test that categorical splits are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.9) %>%
      split_by(PoS.BNC.All, "substantive" ~ "verb") %>%
      control_for(Zipf.BNC.All, -0.25:0.25) %>%
      generate(1000) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, PoS.BNC.All) %>%
          dplyr::rename(A1_PoS = PoS.BNC.All),
        by = c("A1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, PoS.BNC.All) %>%
          dplyr::rename(A2_PoS = PoS.BNC.All),
        by = c("A2" = "string")
      ) %>%
      dplyr::filter(
        A1_PoS == "substantive",
        A2_PoS == "verb"
      ) %>%
      nrow(),
    1000
  )

  # test that exact integer splits are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.9) %>%
      split_by(Length, 4:4 ~ 5:5) %>%
      control_for(Zipf.BNC.All, -0.25:0.25) %>%
      generate(1000) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Length) %>%
          dplyr::rename(A1_Length = Length),
        by = c("A1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Length) %>%
          dplyr::rename(A2_Length = Length),
        by = c("A2" = "string")
      ) %>%
      dplyr::filter(
        A1_Length == 4,
        A2_Length == 5
      ) %>%
      nrow(),
    1000
  )

  # test that inexact, assymmetric integer splits are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.9) %>%
      split_by(Length, 3:4 ~ 5:12) %>%
      control_for(Zipf.BNC.All, -0.25:0.25) %>%
      generate(1000) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Length) %>%
          dplyr::rename(A1_Length = Length),
        by = c("A1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Length) %>%
          dplyr::rename(A2_Length = Length),
        by = c("A2" = "string")
      ) %>%
      dplyr::filter(
        dplyr::between(A1_Length, 3, 4),
        dplyr::between(A2_Length, 5, 12)
      ) %>%
      nrow(),
    1000
  )

  # test that inexact, assymmetric float splits are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.9) %>%
      split_by(Zipf.BNC.All, 0.8:2.1 ~ 2.5:6) %>%
      control_for(Length, 0:0) %>%
      generate(1000) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All) %>%
          dplyr::rename(A1_Zipf = Zipf.BNC.All),
        by = c("A1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All) %>%
          dplyr::rename(A2_Zipf = Zipf.BNC.All),
        by = c("A2" = "string")
      ) %>%
      dplyr::filter(
        dplyr::between(A1_Zipf, 0.8, 2.1),
        dplyr::between(A2_Zipf, 2.5, 6)
      ) %>%
      nrow(),
    1000
  )

  # test that splits can be combined
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.9) %>%
      split_by(Zipf.BNC.All, 0.8:2.1 ~ 2.5:6) %>%
      split_by(PoS.BNC.All, "substantive" ~ "verb" ~ "adjective") %>%
      control_for(Length, 0:0) %>%
      generate(100) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A1_B1_Zipf = Zipf.BNC.All, A1_B1_PoS = PoS.BNC.All),
        by = c("A1_B1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A1_B2_Zipf = Zipf.BNC.All, A1_B2_PoS = PoS.BNC.All),
        by = c("A1_B2" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A1_B3_Zipf = Zipf.BNC.All, A1_B3_PoS = PoS.BNC.All),
        by = c("A1_B3" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A2_B1_Zipf = Zipf.BNC.All, A2_B1_PoS = PoS.BNC.All),
        by = c("A2_B1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A2_B2_Zipf = Zipf.BNC.All, A2_B2_PoS = PoS.BNC.All),
        by = c("A2_B2" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All, PoS.BNC.All) %>%
          dplyr::rename(A2_B3_Zipf = Zipf.BNC.All, A2_B3_PoS = PoS.BNC.All),
        by = c("A2_B3" = "string")
      ) %>%
      dplyr::filter(
        dplyr::between(A1_B1_Zipf, 0.8, 2.1),
        dplyr::between(A1_B2_Zipf, 0.8, 2.1),
        dplyr::between(A1_B3_Zipf, 0.8, 2.1),
        dplyr::between(A2_B1_Zipf, 2.5, 6),
        dplyr::between(A2_B2_Zipf, 2.5, 6),
        dplyr::between(A2_B3_Zipf, 2.5, 6),
        A1_B1_PoS == "substantive",
        A1_B2_PoS == "verb",
        A1_B3_PoS == "adjective",
        A2_B1_PoS == "substantive",
        A2_B2_PoS == "verb",
        A2_B3_PoS == "adjective"
      ) %>%
      nrow(),
    100
  )
})

# controls ----
testthat::test_that("controls", {
  # test that categorical controls are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 1000, match_null = "balanced") %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "string") %>%
      dplyr::left_join(lexops, by = "string") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(length(unique(PoS.BNC.Written)) == 1) %>%
      nrow(),
    2000  # as pivot_longer but no group_by(item_nr)
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
      dplyr::filter(length(unique(Length)) == 1) %>%
      nrow(),
    2000  # as pivot_longer but no group_by(item_nr)
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
      dplyr::filter(dplyr::between(max_diff, 0, 1)) %>%
      nrow(),
    1000
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
      dplyr::filter(max_diff <= 0.2) %>%
      nrow(),
    1000
  )
  # test that inexact, assymmetric float numeric controls are applied correctly
  testthat::expect_equal(
    lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.BNC.All, -0.1:0.25) %>%
      generate(n = 1000, match_null = "balanced") %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All) %>%
          dplyr::rename(A1_Zipf = Zipf.BNC.All),
        by = c("A1" = "string")
      ) %>%
      dplyr::left_join(
        lexops %>%
          dplyr::select(string, Zipf.BNC.All) %>%
          dplyr::rename(A2_Zipf = Zipf.BNC.All),
        by = c("A2" = "string")
      ) %>%
      dplyr::mutate(Zipf_diff = ifelse(match_null == "A1", A2_Zipf - A1_Zipf, A1_Zipf - A2_Zipf)) %>%
      dplyr::filter(dplyr::between(Zipf_diff, -0.1, 0.25)) %>%
      nrow(),
    1000
  )
})

# match_null options ----
testthat::test_that("match_null options", {
  # check balanced match nulls exist in equal frequencies for n divisible by number of conds
  testthat::expect_true({
    match_null_counts <- lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.BNC.All, -0.2:0.2) %>%
      generate(n = 1000, match_null = "balanced") %>%
      dplyr::group_by(match_null) %>%
      dplyr::count() %>%
      dplyr::pull(n)
    all(match_null_counts == 500)
  })
  # check balanced match nulls exist in almost equal frequencies for n not divisible by number of conds
  testthat::expect_true({
    match_null_counts <- lexops %>%
      subset(PK.Brysbaert >= 0.75) %>%
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
      control_for(Zipf.BNC.All, -0.2:0.2) %>%
      generate(n = 1003, match_null = "balanced") %>%
      dplyr::group_by(match_null) %>%
      dplyr::count() %>%
      dplyr::pull(n)
    (501 %in% match_null_counts) & (502 %in% match_null_counts)
  })
  # check "first" match nulls work
  testthat::expect_true({
    match_nulls <- lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, match_null = "first") %>%
      dplyr::pull(match_null)
    all(match_nulls == "A1_B1")
  })
  # check specific match nulls can be selected
  testthat::expect_true({
    match_nulls <- lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
      control_for(Length, 0:0) %>%
      control_for(PoS.BNC.Written) %>%
      generate(n = 10, match_null = "A2_B3") %>%
      dplyr::pull(match_null)
    all(match_nulls == "A2_B3")
  })
  # check inclusive match nulls work
  testthat::expect_equal(
    lexops %>%
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
      split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
      control_for(Zipf.SUBTLEX_UK, -0.5:0.5) %>%
      control_for(BG.SUBTLEX_UK, -0.05:0.9) %>%
      generate(n = 10, match_null = "inclusive") %>%
      long_format() %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(
        max(Zipf.SUBTLEX_UK) - min(Zipf.SUBTLEX_UK) <= 0.5,
        # note that assymmetric tolerances are cut short to become symmmetrical, with a maximum extent of the shortest distance from zero of the supplied positive and negative tolernces. As a result, -0.05:0.9 becomes -0.05:0.05
        max(BG.SUBTLEX_UK) - min(BG.SUBTLEX_UK) <= 0.05
      ) %>%
      nrow(),
    60
  )
})

# control_for_map ----
testthat::test_that("control_for_map", {
  # test with two conditions
  testthat::expect_equal({
    library(vwr)
    lexops %>%
      split_by(AROU.Warriner, 1:3 ~ 7:9) %>%
      control_for_map(levenshtein.distance, string, 0:2) %>%
      generate(10) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld = levenshtein.distance(A1, A2)) %>%
      dplyr::filter(ld <= 2) %>%
      nrow()
  }, 10)
  # test with three conditions
  testthat::expect_equal({
    library(vwr)
    lexops %>%
      split_by(PoS.BNC.All, "substantive" ~ "verb" ~ "adjective") %>%
      control_for_map(levenshtein.distance, string, 0:3) %>%
      generate(50) %>%
      dplyr::mutate(match_null_string = dplyr::case_when(
        match_null == "A1" ~ A1,
        match_null == "A2" ~ A2,
        match_null == "A3" ~ A3
      )) %>%
      tidyr::pivot_longer(c(A1, A2, A3), names_to = "condition", values_to = "string") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld = levenshtein.distance(match_null_string, string)) %>%
      dplyr::filter(ld <= 3) %>%
      nrow()
  }, 150)
})
