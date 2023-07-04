context("generate() controls")

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
    library(stringdist)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_map(stringdist, f, 0:2, method="lv") %>%
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
      dplyr::mutate(ld = stringdist(A1_f, A2_f, method="lv")) %>%
      dplyr::filter(ld <= 2) %>%
      nrow()
  }, 36)
  # test with three conditions and not including 0 in tolerance
  testthat::expect_equal({
    library(stringdist)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(d, "a" ~ "b" ~ "c") %>%
      control_for_map(stringdist, f, 1:4, method="lv") %>%
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
      dplyr::mutate(ld = stringdist(match_null_f, f, method="lv")) %>%
      dplyr::filter(ld >= 1 & ld <= 4) %>%
      nrow()
    },
  46  # (n_items * n_cells) - n_items; as each of the 2 conditions will be within the tolerance to each other, and the match null will be an exact match with itself
  )
  # test with multiple control functions
  testthat::expect_equal({
    library(stringdist)
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_map(stringdist, f, 2:Inf, method="osa") %>%
      control_for_map(stringdist, g, 3:3, method="lv") %>%
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
      dplyr::mutate(ld_f = stringdist(A1_f, A2_f, method="osa")) %>%
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
      dplyr::mutate(ld_g = stringdist(A1_g, A2_g, method="lv")) %>%
      dplyr::filter(ld_g == 3, ld_f >= 2) %>%
      nrow()
  }, 29)
  # test passing of arguments via ellipses
  testthat::expect_equal({
    library(stringdist)

    plus_minus_strdist <- function(a, b, method="osa", minus=FALSE) {
      x <- stringdist(a, b, method=method)
      if (minus) x <- 0-x
      x
    }

    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      control_for_map(plus_minus_strdist, f, -3:0, minus=TRUE) %>%
      generate(27, silent=TRUE) %>%
      long_format() %>%
      dplyr::filter(as.numeric(control_map_1) <= 0 & as.numeric(control_map_1) >= -3) %>%
      nrow()
  }, 54)
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
        man_euc_dist = as.numeric(sqrt(dist_b**2 + dist_e**2))
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
