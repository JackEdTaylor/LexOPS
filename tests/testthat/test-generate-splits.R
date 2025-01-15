context("generate() splits")

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

  # test that exact integer splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(e, 3:3 ~ 1:1) %>%
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
        A1_e == 3,
        A2_e == 1
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

  # test that inexact, assymmetric integer splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(e, 3:6 ~ 0:2) %>%
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
        A1_e >= 3 & A1_e <= 6,
        A2_e >= 0 & A2_e <= 2
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

  # test that inexact, assymmetric float splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, 0.743:3 ~ -1.56:0.1) %>%
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
        A1_a >= 0.743 & A1_a <= 3,
        A2_a >= -1.56 & A2_a <= 0.1
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

  # test that splits can be combined when some levels are specified out of order
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -3:0 ~ 0.25:3) %>%
      split_by(e, 0:2 ~ 5:6 ~ 3:4) %>%
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
        dplyr::between(A1_B2_e, 5, 6),
        dplyr::between(A1_B3_e, 3, 4),
        dplyr::between(A2_B1_e, 0, 2),
        dplyr::between(A2_B2_e, 5, 6),
        dplyr::between(A2_B3_e, 3, 4)
      ) %>%
      nrow(),
    4
  )

  # test that split_random() works
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_random(3) %>%
      control_for(d) %>%
      generate(4, silent = TRUE) %>%
      nrow(),
    4
  )

  # test equal_size param of split_random()
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_random(10, equal_size=TRUE) %>%
      with(df) %>%
      dplyr::pull(LexOPS_splitCond_A) %>%
      table() %>%
      unique(),
    10
  )

  # test that split_random() can be combined with split_by()
  testthat::expect_equal(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -3:0 ~ 0.25:3) %>%
      split_random(3) %>%
      control_for(d) %>%
      generate(4, silent = TRUE) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A1_B1_a = a),
        by = c("A1_B1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A1_B2_a = a),
        by = c("A1_B2" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A1_B3_a = a),
        by = c("A1_B3" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A2_B1_a = a),
        by = c("A2_B1" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A2_B2_a = a),
        by = c("A2_B2" = "id")
      ) %>%
      dplyr::left_join(
        eg_df %>%
          dplyr::select(id, a) %>%
          dplyr::rename(A2_B3_a = a),
        by = c("A2_B3" = "id")
      ) %>%
      dplyr::filter(
        dplyr::between(A1_B1_a, -3, 0),
        dplyr::between(A1_B2_a, -3, 0),
        dplyr::between(A1_B3_a, -3, 0),
        dplyr::between(A2_B1_a, 0.25, 3),
        dplyr::between(A2_B2_a, 0.25, 3),
        dplyr::between(A2_B3_a, 0.25, 3)
      ) %>%
      nrow(),
    4
  )
})
