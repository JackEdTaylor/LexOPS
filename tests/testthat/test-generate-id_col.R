context("generate() id_col")

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

# no id col given ----
testthat::test_that("no id_col given", {
  # check that generate still works when there is no id col
  testthat::expect_equal(
    suppressWarnings(
      eg_df %>%
        split_by(a, -5:-0.1 ~ 0.1:5) %>%
        control_for(b, -2.5:2.5) %>%
        generate(17, silent=TRUE) %>%
        nrow()
    ),
    17
  )
  # test that including no id_col gives a warning
  testthat::expect_warning(
    eg_df %>%
      split_by(a, -5:-0.1 ~ 0.1:5) %>%
      control_for(b, -2.5:2.5) %>%
      generate(17, silent=TRUE),
    "No id_col detected; will use current row numbers (after any subsetting).",
    fixed = TRUE
  )
  # since eg_df's id column is just the row numbers anyway, these should be identical if no filtering is done by split_by
  testthat::expect_identical(
    {
      df <- suppressWarnings(
        eg_df %>%
          set_options(id_col = "id") %>%
          split_by(a, -5:-0.00001 ~ 0.00001:5, filter=FALSE) %>%
          control_for(b, -2.5:2.5) %>%
          generate(10, seed=42, silent=TRUE)
      )
      attributes(df) <- NULL
      df
    },
    {
      df <- suppressWarnings(
        eg_df %>%
          split_by(a, -5:-0.00001 ~ 0.00001:5, filter=FALSE) %>%
          control_for(b, -2.5:2.5) %>%
          generate(10, seed=42, silent=TRUE)
      )
      attributes(df) <- NULL
      df
    }
  )
})
