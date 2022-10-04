context("generate() warnings")

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
    "n is too large; requested n of 55, but the condition with the fewest members has \\d+ entries. Ensure n <= \\d+. Will generate as many stimuli as possible."
  )

  testthat::expect_warning(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:0 ~ 0:5) %>%
      split_random(5, seed=1) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(55, silent=TRUE),
    "n is too large; requested n of 55, but the condition with the fewest members has \\d+ entries. Ensure n <= \\d+. You may also increase the number of candidates by setting equal_size=TRUE in split_random\\(\\). Will generate as many stimuli as possible."
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
