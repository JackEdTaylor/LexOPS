context("generate() reproducibility")

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

# reproducibility ----
testthat::test_that("reproducibility", {
  # check updates won't alter existing code's output
  testthat::expect_identical(
    {
      set.seed(42)
      df <- eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        control_for(d) |>
        generate(10, silent=TRUE)
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
  # check updates won't alter existing code's output with inclusive matching
  testthat::expect_identical(
    {
      set.seed(42)
      df <- eg_df |>
        set_options(id_col = "id") |>
        split_by(d, "a" ~ "b" ~ "c") |>
        control_for(b, -0.5:0.5) |>
        control_for(c, -0.25:0.25) |>
        generate(10, match_null="inclusive", silent=TRUE)
      attributes(df) <- NULL
      df
    },
    {
      df <- data.frame(
        item_nr = 1:10,
        A1 = c("17", "35", "25", "70", "90", "56", "80", "52", "58", "75"),
        A2 = c("67", "9", "38", "31", "88", "98", "42", "89", "37", "30"),
        A3 = c("91", "51", "18", "73", "45", "39", "64", "12", "1", "72"),
        match_null = as.character(NA)
      )
      attributes(df) <- NULL
      df
    }
  )
  # check updates won't alter control_for_euc() results
  testthat::expect_identical(
    {
      set.seed(31415926)
      df <- eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for_euc(c(b, c), 0:0.2) |>
        generate(10, silent=TRUE)
      attributes(df) <- NULL
      df
    },
    {
      df <- data.frame(
        item_nr = 1:10,
        A1 = c("62", "29", "75", "1", "81", "34", "82", "27", "35", "99"),
        A2 = c("69", "93", "30", "89", "59", "55", "63", "48", "9", "51"),
        match_null = c("A1", "A1", "A2", "A1", "A1", "A2", "A2", "A2", "A1", "A2")
      )
      attributes(df) <- NULL
      df
    }
  )
  # check updates won't alter control_for_euc() results with weighting
  testthat::expect_identical(
    {
      set.seed(31415926)
      df <- eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for_euc(
          c(b, c),
          weights = c(4, 1),
          0:0.2
        ) |>
        generate(10, silent=TRUE)
      attributes(df) <- NULL
      df
    },
    {
      df <- data.frame(
        item_nr = 1:10,
        A1 = c("62", "29", "46", "37", "99", "88", "1", "67", "75", "45"),
        A2 = c("20", "11", "5", "30", "9", "39", "93", "69", "53", "56"),
        match_null = c("A1", "A1", "A2", "A1", "A1", "A2", "A2", "A2", "A1", "A2")
      )
      attributes(df) <- NULL
      df
    }
  )
  # check that using the same external seed twice can reproduce the older internal seeds' results when split_random() is used
  # (useful if users want to force newer versions of LexOPS to reproduce older results)
  testthat::expect_identical(
    {
      set.seed(42)
      df_a <- eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        split_random(2, equal_size=TRUE) |>
        control_for(c, -2.5:2.5) |>
        control_for(d)

      set.seed(42)
      df <- df_a |>
        generate(10, silent=TRUE)

      attributes(df) <- NULL
      df
    },
    {
      # internal seed code from v0.4.0
      # df <- eg_df |>
      #   set_options(id_col = "id") |>
      #   split_by(a, -5:-0.0001 ~ 0.0001:5) |>
      #   split_random(2, equal_size=TRUE, seed=42) |>
      #   control_for(c, -2.5:2.5) |>
      #   control_for(d) |>
      #   generate(10, seed=42)
      # result from internal seed method using v0.4.0
      df <- data.frame(
        item_nr = 1:10,
        A1_B1 = c("24", "100", "6", "37", "84", "14", "10", "99", "75", "81"),
        A1_B2 = c("42", "28", "91", "3", "60", "38", "97", "26", "35", "34"),
        A2_B1 = c("8", "85", "51", "53", "71", "15", "63", "93", "5", "76"),
        A2_B2 = c("86", "21", "64", "89", "50", "33", "95", "47", "20", "4"),
        match_null = c("A2_B2", "A1_B2", "A2_B2", "A1_B2", "A1_B1", "A1_B1", "A2_B2", "A1_B1", "A2_B1", "A2_B1")
      )
      attributes(df) <- NULL
      df
    }
  )
  # non-standard vs. non-standard (seed test)
  testthat::expect_identical(
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        control_for(d) |>
        generate(10, silent = TRUE)
    },
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        control_for(d) |>
        generate(10, silent = TRUE)
    }
  )
  # standard vs. standard (seed test)
  testthat::expect_identical(
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by("a", list(c(-5, -0.0001), c(0.0001, 5)), standard_eval = TRUE) |>
        control_for("b", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("c", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("d", standard_eval = TRUE) |>
        generate(10, silent = TRUE)
    },
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by("a", list(c(-5, -0.0001), c(0.0001, 5)), standard_eval = TRUE) |>
        control_for("b", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("c", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("d", standard_eval = TRUE) |>
        generate(10, silent = TRUE)
    }
  )
  # non-standard vs. standard (transferability test)
  testthat::expect_identical(
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        control_for(d) |>
        generate(10, silent = TRUE)
    },
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by("a", list(c(-5, -0.0001), c(0.0001, 5)), standard_eval = TRUE) |>
        control_for("b", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("c", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for("d", standard_eval = TRUE) |>
        generate(10, silent = TRUE)
    }
  )
  # hybrid vs. hybrid (mixed transferability test)
  testthat::expect_identical(
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by("a", list(c(-5, -0.0001), c(0.0001, 5)), standard_eval = TRUE) |>
        control_for(b, -2.5:2.5) |>
        control_for("c", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for(d) |>
        generate(10, silent = TRUE)
    },
    {
      set.seed(42)
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for("b", c(-2.5, 2.5), standard_eval = TRUE) |>
        control_for(c, -2.5:2.5) |>
        control_for("d", standard_eval = TRUE) |>
        generate(10, silent = TRUE)
    }
  )
  # check order doesn't matter when one split
  testthat::expect_identical(
    {
      set.seed(69)
      x <- eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        control_for(d) |>
        generate(10, silent = TRUE) |>
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    },
    {
      set.seed(69)
      x <- eg_df |>
        set_options(id_col = "id") |>
        control_for(d) |>
        control_for(b, -2.5:2.5) |>
        control_for(c, -2.5:2.5) |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        generate(10, silent = TRUE) |>
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    }
  )
  # check order doesn't matter when two splits, but same order of splits
  testthat::expect_identical(
    {
      set.seed(69)
      x <- eg_df |>
        set_options(id_col = "id") |>
        control_for(c, -2.5:2.5) |>
        split_by(e, 0:3 ~ 4:6) |>
        control_for(d) |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        generate(10, silent = TRUE) |>
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    },
    {
      set.seed(69)
      x <- eg_df |>
        set_options(id_col = "id") |>
        control_for(c, -2.5:2.5) |>
        split_by(e, 0:3 ~ 4:6) |>
        split_by(a, -5:-0.0001 ~ 0.0001:5) |>
        control_for(d) |>
        generate(10, silent = TRUE) |>
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    }
  )
  # check order does matter when two splits, with different order of splits
  testthat::expect_false({
    set.seed(69)
    x <- eg_df |>
      set_options(id_col = "id") |>
      control_for(c, -2.5:2.5) |>
      split_by(e, 0:3 ~ 4:6) |>
      control_for(d) |>
      split_by(a, -5:-0.0001 ~ 0.0001:5) |>
      generate(10, silent = TRUE) |>
      as.data.frame()
    attr(x, "LexOPS_info") <- NULL

    set.seed(69)
    y <- eg_df |>
      set_options(id_col = "id") |>
      control_for(c, -2.5:2.5) |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      split_by(e, 0:3 ~ 4:6) |>
      control_for(d) |>
      generate(10, silent = TRUE) |>
      as.data.frame()
    attr(y, "LexOPS_info") <- NULL

    identical(x, y)
  })
  # check that trying to provide an internal seed (now deprecated) produces an error
  testthat::expect_error(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.0001 ~ 0.0001:5) |>
      split_by(e, 0:3 ~ 4:6) |>
      control_for(c, -2.5:2.5) |>
      control_for(d) |>
      generate(10, silent = TRUE, seed = 42),
    regexp = "The seed argument is deprecated"
  )
  # check that trying to provide an internal seed to split_random() also produces an error
  testthat::expect_error(
    eg_df |>
      set_options(id_col = "id") |>
      split_random(2, equal_size=TRUE, seed=42) |>
      control_for(c, -2.5:2.5) |>
      control_for(d) |>
      generate(10, silent = TRUE),
    regexp = "The seed argument is deprecated"
  )
})
