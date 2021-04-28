context("generate")

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

# general warnings ----
testthat::test_that("general warnings", {
  testthat::expect_warning(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(55, silent=TRUE),
    "n is too large; requested n of 55, but the condition with the fewest members has \\d+ entries. Ensure n < \\d+. Will generate as many stimuli as possible.",
    all = FALSE
  )

  testthat::expect_warning(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(d) %>%
      control_for(e) %>%
      generate(10, silent=TRUE),
    "No tolerance given for numeric variable 'e', will control for exactly.",
    fixed = TRUE
  )

  testthat::expect_warning(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate("all", match_null="balanced", silent=TRUE),
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
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate("10"),
    "n must be numeric or a string of value \"all\"",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10, match_null = "irreverent"),
    "Unknown match_null; expected \"inclusive\", \"random\", \"balanced\", \"first\", or a specific condition (e.g. \"A2_B1_C1\")",
    fixed = TRUE
  )
})

# reproducibility ----
testthat::test_that("reproducibility", {
  # check updates won't alter existing code's output
  testthat::expect_identical(
    {
      df <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate(10, seed=42, silent=TRUE)
      attributes(df) <- NULL
      df
    },
    {
      df <- data.frame(
        item_nr = 1:10,
        A1 = c("77", "81", "91", "75", "17", "100", "60", "72", "13", "88"),
        A2 = c("73", "94", "39", "95", "5", "51", "71", "68", "19", "89"),
        match_null = c("A1", "A1", "A2", "A2", "A2", "A2", "A2", "A1", "A1", "A1")
      )
      attributes(df) <- NULL
      df
    }
  )
  # non-standard vs. non-standard (seed test)
  testthat::expect_identical(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10, seed = 42, silent = TRUE),
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10, seed = 42, silent = TRUE)
  )
  # standard vs. standard (seed test)
  testthat::expect_identical(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by("a", list(c(-5, 0), c(0, 5)), standard_eval = TRUE) %>%
      control_for("b", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("c", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("d", standard_eval = TRUE) %>%
      generate(10, seed = 42, silent = TRUE),
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by("a", list(c(-5, 0), c(0, 5)), standard_eval = TRUE) %>%
      control_for("b", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("c", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("d", standard_eval = TRUE) %>%
      generate(10, seed = 42, silent = TRUE)
  )
  # non-standard vs. standard (transferability test)
  testthat::expect_identical(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10, seed = 42, silent = TRUE),
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by("a", list(c(-5, 0), c(0, 5)), standard_eval = TRUE) %>%
      control_for("b", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("c", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for("d", standard_eval = TRUE) %>%
      generate(10, seed = 42, silent = TRUE)
  )
  # hybrid vs. hybrid (mixed transferability test)
  testthat::expect_identical(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by("a", list(c(-5, 0), c(0, 5)), standard_eval = TRUE) %>%
      control_for(b, -2.5:2.5) %>%
      control_for("c", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for(d) %>%
      generate(10, seed = 42, silent = TRUE),
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for("b", c(-2.5, 2.5), standard_eval = TRUE) %>%
      control_for(c, -2.5:2.5) %>%
      control_for("d", standard_eval = TRUE) %>%
      generate(10, seed = 42, silent = TRUE)
  )
})

# no id col given ----
testthat::test_that("no id_col given", {
  # check that generate still works when there is no id col
  testthat::expect_equal(
    suppressWarnings(
      eg_df %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        generate(17, silent=TRUE) %>%
        nrow()
    ),
    17
  )
  # test that including no id_col gives a warning
  testthat::expect_warning(
    eg_df %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -2.5:2.5) %>%
      generate(17, silent=TRUE),
    "No id_col detected; will use row numbers."
  )
  # since eg_df's id column is just the row numbers anyway, these should be identical
  testthat::expect_identical(
    {
      df <- suppressWarnings(
        eg_df %>%
          set_options(id_col = "id") %>%
          split_by(a, -5:0 ~ 0:5) %>%
          control_for(b, -2.5:2.5) %>%
          generate(10, seed=42, silent=TRUE)
      )
      attributes(df) <- NULL
      df
    },
    {
      df <- suppressWarnings(
        eg_df %>%
          split_by(a, -5:0 ~ 0:5) %>%
          control_for(b, -2.5:2.5) %>%
          generate(10, seed=42, silent=TRUE)
      )
      attributes(df) <- NULL
      df
    }
  )
})

# silent option ----
testthat::test_that("silent_option", {
  # check that printing to console works as normal for n = x when silent = FALSE
  testthat::expect_gt(
    capture.output({
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate(10)
    }) %>%
      nchar() %>%
      sum(),
    0
  )
  # check that printing to console works as normal for n = "all" when silent = FALSE
  testthat::expect_gt(
    capture.output({
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate("all", match_null = "random")
    }) %>%
      nchar() %>%
      sum(),
    0
  )
  # check that the silent option works for n = x
  testthat::expect_equal(
    capture.output({
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate(10, silent = TRUE)
    }) %>%
      nchar() %>%
      sum(),
    0
  )
  # check that the silent option works for n = "all"
  testthat::expect_equal(
    capture.output({
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate("all", match_null = "random", silent = TRUE)
    }) %>%
      nchar() %>%
      sum(),
    0
  )
})

# splits ----
testthat::test_that("splits", {
  # test that categorical splits are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(d, "a" ~ "b") %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      generate(10, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, d) %>%
          dplyr::rename(A1_d = d),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, d) %>%
          dplyr::rename(A2_d = d),
        by = c("A2" = "id")
      ) %>%
      dplyr::filter(
        A1_d == "a",
        A2_d == "b"
      ) %>%
      nrow(),
    10
  )

  # test that exact integer splits are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(e, 1:1 ~ 3:3) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      generate(10, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, e) %>%
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, e) %>%
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) %>%
      dplyr::filter(
        A1_e == 1,
        A2_e == 3
      ) %>%
      nrow(),
    10
  )

  # test that inexact, assymmetric integer splits are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(e, 0:2 ~ 3:6) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      generate(40, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, e) %>%
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, e) %>%
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) %>%
      dplyr::filter(
        A1_e >= 0 & A1_e <= 2,
        A2_e >= 3 & A2_e <= 6
      ) %>%
      nrow(),
    40
  )

  # test that inexact, assymmetric float splits are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -1.56:0.1 ~ 0.743:3) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      generate(15, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A1_a = a),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A2_a = a),
        by = c("A2" = "id")
      ) %>%
      dplyr::filter(
        A1_a >= -1.56 & A1_a <= 0.1,
        A2_a >= 0.743 & A2_a <= 3
      ) %>%
      nrow(),
    15
  )

  # test that splits can be combined
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -3:0 ~ 0.25:3) %>%
      split_by(e, 0:2 ~ 3:4 ~ 5:6) %>%
      control_for(d) %>%
      generate(4, silent = TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A1_B1_a = a, A1_B1_e = e),
        by = c("A1_B1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A1_B2_a = a, A1_B2_e = e),
        by = c("A1_B2" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A1_B3_a = a, A1_B3_e = e),
        by = c("A1_B3" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A2_B1_a = a, A2_B1_e = e),
        by = c("A2_B1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A2_B2_a = a, A2_B2_e = e),
        by = c("A2_B2" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a, e) %>%
          dplyr::rename(A2_B3_a = a, A2_B3_e = e),
        by = c("A2_B3" = "id")
      ) %>%
      dplyr::filter(
        dplyr::between(A1_B1_a, -3, 0),
        dplyr::between(A1_B2_a, -3, 0),
        dplyr::between(A1_B3_a, -3, 0),
        dplyr::between(A2_B1_a, 0.25, 3),
        dplyr::between(A2_B2_a, 0.25, 3),
        dplyr::between(A2_B3_a, 0.25, 3),
        dplyr::between(A1_B1_e, 0, 2),
        dplyr::between(A1_B2_e, 3, 4),
        dplyr::between(A1_B3_e, 5, 6),
        dplyr::between(A2_B1_e, 0, 2),
        dplyr::between(A2_B2_e, 3, 4),
        dplyr::between(A2_B3_e, 5, 6)
      ) %>%
      nrow(),
    4
  )
})

# controls ----
testthat::test_that("controls", {
  # test that categorical controls are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(d) %>%
      generate(20, silent=TRUE) %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "id") %>%
      dplyr::left_join(eg_df, by = "id") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(length(unique(d)) == 1) %>%
      dplyr::ungroup() %>%
      nrow(),
    40  # as pivot_longer but no group_by(item_nr)
  )
  # test that exact numeric controls are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(e, 0:0) %>%
      generate(20, silent=TRUE) %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "id") %>%
      dplyr::left_join(eg_df, by = "id") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(length(unique(e)) == 1) %>%
      dplyr::ungroup() %>%
      nrow(),
    40  # as pivot_longer but no group_by(item_nr)
  )
  # test that inexact integer numeric controls are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(e, -1:1) %>%
      generate(20, silent=TRUE) %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "id") %>%
      dplyr::left_join(eg_df, by = "id") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::summarise(max_diff = max(e) - min(e)) %>%
      dplyr::filter(dplyr::between(max_diff, -1, 1)) %>%
      dplyr::ungroup() %>%
      nrow(),
    20
  )
  # test that inexact float numeric controls are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -0.25:0.25) %>%
      generate(20, silent=TRUE) %>%
      tidyr::pivot_longer(c(A1, A2), names_to = "condition", values_to = "id") %>%
      dplyr::left_join(eg_df, by = "id") %>%
      dplyr::group_by(item_nr) %>%
      dplyr::summarise(max_diff = max(b) - min(b)) %>%
      dplyr::filter(dplyr::between(max_diff, -0.25, 0.25)) %>%
      dplyr::ungroup() %>%
      nrow(),
    20
  )
  # test that inexact, assymmetric float numeric controls are applied correctly
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(b, -0.37:0.1) %>%
      generate(30, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b) %>%
          dplyr::rename(A1_b = b),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b) %>%
          dplyr::rename(A2_b = b),
        by = c("A2" = "id")
      ) %>%
      dplyr::mutate(b_diff = ifelse(match_null == "A1", A2_b - A1_b, A1_b - A2_b)) %>%
      dplyr::filter(dplyr::between(b_diff, -0.37, 0.1)) %>%
      nrow(),
    30
  )
})

# match_null options ----
testthat::test_that("match_null options", {
  # check balanced match nulls exist in equal frequencies for n divisible by number of conds
  testthat::expect_true({
    match_null_counts <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(d) %>%
      generate(32, match_null = "balanced", silent=TRUE) %>%
      dplyr::group_by(match_null) %>%
      dplyr::count() %>%
      dplyr::pull(n)
    all(match_null_counts == 16)
  })
  # check balanced match nulls exist in almost equal frequencies for n not divisible by number of conds
  testthat::expect_true({
    match_null_counts <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for(d) %>%
      generate(33, match_null = "balanced", silent=TRUE) %>%
      dplyr::group_by(match_null) %>%
      dplyr::count() %>%
      dplyr::pull(n)
    all(sort(match_null_counts) == c(16, 17))
  })
  # check "first" match nulls work
  testthat::expect_true({
    match_nulls <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      split_by(e, 0:3 ~ 4:6) %>%
      control_for(d) %>%
      generate(12, match_null = "first", silent=TRUE) %>%
      dplyr::pull(match_null)
    all(match_nulls == "A1_B1")
  })
  # check specific match nulls can be selected
  testthat::expect_true({
    match_nulls <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      split_by(e, 0:3 ~ 4:6) %>%
      control_for(d) %>%
      generate(12, match_null = "A1_B2", silent=TRUE) %>%
      dplyr::pull(match_null)
    all(match_nulls == "A1_B2")
  })
  # check inclusive match nulls work
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(d, "a" ~ "b" ~ "c") %>%
      control_for(b, -1.5:10) %>%
      generate(5, match_null = "inclusive", silent=TRUE) %>%
      long_format() %>%
      dplyr::group_by(item_nr) %>%
      dplyr::filter(
        # note that assymmetric tolerances are cut short to become symmmetrical, with a maximum extent of the shortest distance from zero of the supplied positive and negative tolernces. As a result, -1.5:10 becomes -1.5:1.5
        max(b) - min(b) <= 1.5
      ) %>%
      nrow(),
    15
  )
})

# control_for_map ----
testthat::test_that("control_for_map", {
  # test with two conditions
  testthat::expect_equal({
    library(vwr)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_map(levenshtein.distance, f, 0:2) %>%
      generate(36, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A1_f = f),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A2_f = f),
        by = c("A2" = "id")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld = levenshtein.distance(A1_f, A2_f)) %>%
      dplyr::filter(ld <= 2) %>%
      nrow()
  }, 36)
  # test with three conditions and not including 0 in tolerance
  testthat::expect_equal({
    library(vwr)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(d, "a" ~ "b" ~ "c") %>%
      control_for_map(levenshtein.distance, f, 1:4) %>%
      generate(23, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A1_f = f),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A2_f = f),
        by = c("A2" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A3_f = f),
        by = c("A3" = "id")
      ) %>%
      dplyr::mutate(match_null_f = dplyr::case_when(
        match_null == "A1" ~ A1_f,
        match_null == "A2" ~ A2_f,
        match_null == "A3" ~ A3_f
      )) %>%
      tidyr::pivot_longer(c(A1_f, A2_f, A3_f), names_to = "condition", values_to = "f") %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld = levenshtein.distance(match_null_f, f)) %>%
      dplyr::filter(ld >= 1 & ld <= 4) %>%
      nrow()
    },
  46  # (n_items * n_cells) - n_items; as each of the 2 conditions will be within the tolerance to each other, and the match null will be an exact match with itself
  )
  # test with multiple control functions
  testthat::expect_equal({
    library(vwr)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_map(levenshtein.distance, f, 2:Inf) %>%
      control_for_map(levenshtein.distance, g, 3:3) %>%
      generate(29, silent=TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A1_f = f),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, f) %>%
          dplyr::rename(A2_f = f),
        by = c("A2" = "id")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld_f = levenshtein.distance(A1_f, A2_f)) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, g) %>%
          dplyr::rename(A1_g = g),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, g) %>%
          dplyr::rename(A2_g = g),
        by = c("A2" = "id")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ld_g = levenshtein.distance(A1_g, A2_g)) %>%
      dplyr::filter(ld_g == 3, ld_f >= 2) %>%
      nrow()
  }, 29)
})

# control_for_euc ----
testthat::test_that("control_for_euc", {
  # test with two conditions, comparing manual euclidean distance calculation to that from control_for_euc()
  testthat::expect_equal({
    wide_res <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_euc(
        c(b, e),
        0:1.5,
        name = "gen_euc_dist"
      ) %>%
      generate(20, silent=TRUE)

    manual_euc_dist <- wide_res %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = scale(b), e = scale(e)) %>%
          dplyr::rename(A1_b = b, A1_e = e),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = scale(b), e = scale(e)) %>%
          dplyr::rename(A2_b = b, A2_e = e),
        by = c("A2" = "id")
      ) %>%
      dplyr::mutate(
        dist_b = A1_b - A2_b,
        dist_e = A1_e - A2_e,
        man_euc_dist = sqrt(dist_b**2 + dist_e**2)
      )

    wide_res %>%
      long_format() %>%
      dplyr::filter(condition != match_null) %>%
      dplyr::left_join(
        dplyr::select(manual_euc_dist, item_nr, man_euc_dist),
        by = "item_nr"
      ) %>%
      dplyr::filter(gen_euc_dist == man_euc_dist) %>%
      nrow()
  }, 20)
  # test weighted Euclidean distance is calculated as expected
  testthat::expect_equal({
    weights <- runif(2, 0.1, 100)
    weights_std <- weights / mean(weights)

    wide_res <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_euc(
        c(b, e),
        0:1.5,
        name = "gen_euc_dist",
        weights = weights
      ) %>%
      generate(20, silent=TRUE)

    manual_euc_dist <- wide_res %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = weights_std[1]*scale(b), e = weights_std[2]*scale(e)) %>%
          dplyr::rename(A1_b = b, A1_e = e),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = weights_std[1]*scale(b), e = weights_std[2]*scale(e)) %>%
          dplyr::rename(A2_b = b, A2_e = e),
        by = c("A2" = "id")
      ) %>%
      dplyr::mutate(
        dist_b = A1_b - A2_b,
        dist_e = A1_e - A2_e,
        man_euc_dist = sqrt(dist_b**2 + dist_e**2)
      )

    wide_res %>%
      long_format() %>%
      dplyr::filter(condition != match_null) %>%
      dplyr::left_join(
        dplyr::select(manual_euc_dist, item_nr, man_euc_dist),
        by = "item_nr"
      ) %>%
      dplyr::filter(gen_euc_dist == man_euc_dist) %>%
      nrow()
  }, 20)
  # test that weight standardisation can be disabled
  testthat::expect_equal({
    weights <- runif(2, 0.1, 100)

    wide_res <- eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_euc(
        c(b, e),
        0:10,
        name = "gen_euc_dist",
        weights = weights,
        standardise_weights = FALSE
      ) %>%
      generate(20, silent=TRUE)

    manual_euc_dist <- wide_res %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = weights[1]*scale(b), e = weights[2]*scale(e)) %>%
          dplyr::rename(A1_b = b, A1_e = e),
        by = c("A1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, b, e) %>%
          dplyr::mutate(b = weights[1]*scale(b), e = weights[2]*scale(e)) %>%
          dplyr::rename(A2_b = b, A2_e = e),
        by = c("A2" = "id")
      ) %>%
      dplyr::mutate(
        dist_b = A1_b - A2_b,
        dist_e = A1_e - A2_e,
        man_euc_dist = sqrt(dist_b**2 + dist_e**2)
      )

    wide_res %>%
      long_format() %>%
      dplyr::filter(condition != match_null) %>%
      dplyr::left_join(
        dplyr::select(manual_euc_dist, item_nr, man_euc_dist),
        by = "item_nr"
      ) %>%
      dplyr::filter(gen_euc_dist == man_euc_dist) %>%
      nrow()
  }, 20)
})
