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
  # check order doesn't matter when one split
  testthat::expect_identical(
    {
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        control_for(d) %>%
        generate(10, seed = 69, silent = TRUE) %>%
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    },
    {
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        control_for(d) %>%
        control_for(b, -2.5:2.5) %>%
        control_for(c, -2.5:2.5) %>%
        split_by(a, -5:0 ~ 0:5) %>%
        generate(10, seed = 69, silent = TRUE) %>%
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    }
  )
  # check order doesn't matter when two splits, but same order of splits
  testthat::expect_identical(
    {
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        control_for(c, -2.5:2.5) %>%
        split_by(e, 0:3 ~ 4:6) %>%
        control_for(d) %>%
        split_by(a, -5:0 ~ 0:5) %>%
        generate(10, seed = 69, silent = TRUE) %>%
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    },
    {
      x <- eg_df %>%
        set_options(id_col = "id") %>%
        control_for(c, -2.5:2.5) %>%
        split_by(e, 0:3 ~ 4:6) %>%
        split_by(a, -5:0 ~ 0:5) %>%
        control_for(d) %>%
        generate(10, seed = 69, silent = TRUE) %>%
        as.data.frame()
      attr(x, "LexOPS_info") <- NULL
      x
    }
  )
  # check order does matter when two splits, with different order of splits
  testthat::expect_false({
    x <- eg_df %>%
      set_options(id_col = "id") %>%
      control_for(c, -2.5:2.5) %>%
      split_by(e, 0:3 ~ 4:6) %>%
      control_for(d) %>%
      split_by(a, -5:0 ~ 0:5) %>%
      generate(10, seed = 69, silent = TRUE) %>%
      as.data.frame()
    attr(x, "LexOPS_info") <- NULL
    y <- eg_df %>%
      set_options(id_col = "id") %>%
      control_for(c, -2.5:2.5) %>%
      split_by(a, -5:0 ~ 0:5) %>%
      split_by(e, 0:3 ~ 4:6) %>%
      control_for(d) %>%
      generate(10, seed = 69, silent = TRUE) %>%
      as.data.frame()
    attr(y, "LexOPS_info") <- NULL

    identical(x, y)
  })
})
