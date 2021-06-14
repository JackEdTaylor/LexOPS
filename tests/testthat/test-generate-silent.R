context("generate() silent")

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
