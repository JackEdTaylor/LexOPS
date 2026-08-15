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
    eg_df |>
      set_options(id_col = "id") |>
      split_by(d, "a" ~ "b") |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(10, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, d) |>
          dplyr::rename(A1_d = d),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, d) |>
          dplyr::rename(A2_d = d),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_d == "a",
        A2_d == "b"
      ) |>
      nrow(),
    10
  )

  # test that it is possible to have levels that cover multiple values in a categorical split
  testthat::expect_true({
    set.seed(1)  # want exact same stimulus set each time for this test
    stim <- eg_df |>
      set_options(id_col = "id") |>
      split_by(d, "a" ~ c("b", "c")) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(25, silent=TRUE)

  stim_lf <- long_format(stim)

  A1 <- subset(stim_lf, condition=="A1")
  A2 <- subset(stim_lf, condition=="A2")

  all(A1$d == "a") & all(c("b", "c") %in% A2$d)
  })

  # test that exact integer splits are applied correctly
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(e, 1:1 ~ 3:3) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(10, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_e == 1,
        A2_e == 3
      ) |>
      nrow(),
    10
  )

  # test that exact integer splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(e, 3:3 ~ 1:1) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(10, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_e == 3,
        A2_e == 1
      ) |>
      nrow(),
    10
  )

  # test that inexact, assymmetric integer splits are applied correctly
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(e, 0:2 ~ 3:6) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(40, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_e >= 0 & A1_e <= 2,
        A2_e >= 3 & A2_e <= 6
      ) |>
      nrow(),
    40
  )

  # test that inexact, assymmetric integer splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(e, 3:6 ~ 0:2) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(40, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A1_e = e),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, e) |>
          dplyr::rename(A2_e = e),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_e >= 3 & A1_e <= 6,
        A2_e >= 0 & A2_e <= 2
      ) |>
      nrow(),
    40
  )

  # test that inexact, assymmetric float splits are applied correctly
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -1.56:0.1 ~ 0.743:3) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(15, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A1_a = a),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A2_a = a),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_a >= -1.56 & A1_a <= 0.1,
        A2_a >= 0.743 & A2_a <= 3
      ) |>
      nrow(),
    15
  )

  # test that inexact, assymmetric float splits are applied correctly when levels are specified out of order
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, 0.743:3 ~ -1.56:0.1) |>
      control_for(b, -2.5:2.5) |>
      control_for(c, -2.5:2.5) |>
      generate(15, silent=TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A1_a = a),
        by = c("A1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A2_a = a),
        by = c("A2" = "id")
      ) |>
      dplyr::filter(
        A1_a >= 0.743 & A1_a <= 3,
        A2_a >= -1.56 & A2_a <= 0.1
      ) |>
      nrow(),
    15
  )

  # test that splits can be combined
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -3:0 ~ 0.25:3) |>
      split_by(e, 0:2 ~ 3:4 ~ 5:6) |>
      control_for(d) |>
      generate(4, silent = TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B1_a = a, A1_B1_e = e),
        by = c("A1_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B2_a = a, A1_B2_e = e),
        by = c("A1_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B3_a = a, A1_B3_e = e),
        by = c("A1_B3" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B1_a = a, A2_B1_e = e),
        by = c("A2_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B2_a = a, A2_B2_e = e),
        by = c("A2_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B3_a = a, A2_B3_e = e),
        by = c("A2_B3" = "id")
      ) |>
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
      ) |>
      nrow(),
    4
  )

  # test that splits can be combined when some levels are specified out of order
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -3:0 ~ 0.25:3) |>
      split_by(e, 0:2 ~ 5:6 ~ 3:4) |>
      control_for(d) |>
      generate(4, silent = TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B1_a = a, A1_B1_e = e),
        by = c("A1_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B2_a = a, A1_B2_e = e),
        by = c("A1_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A1_B3_a = a, A1_B3_e = e),
        by = c("A1_B3" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B1_a = a, A2_B1_e = e),
        by = c("A2_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B2_a = a, A2_B2_e = e),
        by = c("A2_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a, e) |>
          dplyr::rename(A2_B3_a = a, A2_B3_e = e),
        by = c("A2_B3" = "id")
      ) |>
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
      ) |>
      nrow(),
    4
  )
})

# random splits ----
testthat::test_that("random splits", {
  # test that split_random() works
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_random(3, equal_size=TRUE) |>
      control_for(d) |>
      generate(4, silent = TRUE) |>
      nrow(),
    4
  )

  # split_random() doesn't need the dataframe to already be a LexOPS pipeline
  testthat::expect_equal(
    # suppress warning about no id_col being set/detected
    suppressWarnings(
      eg_df |>
        split_random(3, equal_size=TRUE) |>
        control_for(d) |>
        generate(4, silent = TRUE) |>
        nrow()
    ),
    4
  )

  # can combine multiple split_random()
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_random(2, equal_size=TRUE) |>
      split_random(2, equal_size=TRUE) |>
      control_for(d) |>
      generate(4, silent = TRUE) |>
      nrow(),
    4
  )

  # test equal_size param of split_random()
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_random(10, equal_size=TRUE) |>
      with(df) |>
      dplyr::pull(LexOPS_splitCond_A) |>
      table() |>
      unique(),
    10
  )

  # test that split_random() can be combined with split_by()
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -3:0 ~ 0.25:3) |>
      split_random(3, equal_size=TRUE) |>
      control_for(d) |>
      generate(4, silent = TRUE) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A1_B1_a = a),
        by = c("A1_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A1_B2_a = a),
        by = c("A1_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A1_B3_a = a),
        by = c("A1_B3" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A2_B1_a = a),
        by = c("A2_B1" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A2_B2_a = a),
        by = c("A2_B2" = "id")
      ) |>
      dplyr::left_join(
        eg_df |>
          dplyr::select(id, a) |>
          dplyr::rename(A2_B3_a = a),
        by = c("A2_B3" = "id")
      ) |>
      dplyr::filter(
        dplyr::between(A1_B1_a, -3, 0),
        dplyr::between(A1_B2_a, -3, 0),
        dplyr::between(A1_B3_a, -3, 0),
        dplyr::between(A2_B1_a, 0.25, 3),
        dplyr::between(A2_B2_a, 0.25, 3),
        dplyr::between(A2_B3_a, 0.25, 3)
      ) |>
      nrow(),
    4
  )
})

# split_by() errors ----
testthat::test_that("split_by() errors", {
  # ensure that numeric levels are in order
  testthat::expect_error({
    # ignore expected warning about missing levels
    # (because there are no observations for 2 < x < 1)
    suppressWarnings(
      eg_df |>
        split_by(a, 2:1 ~ 2.1:2.5 ~ 3.2:4)
    )
  },
  "lower bounds must be lower than upper bounds",
  fixed = TRUE
  )

  # check that the expected warning was indeed what was produced
  testthat::expect_warning({
    # ignore expected warning about missing levels
    # (because there are no observations for 2 < x < 1)
    tryCatch(
      eg_df |>
        split_by(a, 2:1 ~ 2.1:2.5 ~ 3.2:4),
      error = function(e) {}
    )
  },
  "No entries could be found for some levels. Check all levels of a are possible.",
  fixed = TRUE
  )

  # ensure that numeric levels are exclusive
  testthat::expect_error({
    eg_df |>
      split_by(a, -5:0 ~ 0:1 ~ 1.1:5)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

  # test whether non-exclusive levels are still detected when out of order
  testthat::expect_error({
    eg_df |>
      split_by(a, -5:0 ~ 1.1:5 ~ 0:1)
  },
  "overlapping levels - ensure that no value could fall into multiple levels",
  fixed = TRUE
  )

  # get error when missing categorical levels
  testthat::expect_error(
    # suppress expected additional warning about missing levels
    suppressWarnings(
      eg_df |>
        set_options(id_col = "id") |>
        split_by(d, "a" ~ "b" ~ "MISSING")
    ),
    regexp = "not all breaks are existing factor levels",
    fixed = TRUE
  )

  # get informative warning when missing categorical levels
  testthat::expect_warning(
    # suppress expected additional warning about missing levels
    tryCatch(
      eg_df |>
        set_options(id_col = "id") |>
        split_by(d, "a" ~ "b" ~ "MISSING"),
      error=function(e){}
    ),
    regexp = "No entries could be found for some levels. Check all levels of d are possible.",
    fixed = TRUE
  )

  # get informative warning when missing numeric levels
  testthat::expect_warning(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -2:2 ~ 1000:1005),
    regexp = "No entries could be found for some levels. Check all levels of a are possible.",
    fixed = TRUE
  )

})

