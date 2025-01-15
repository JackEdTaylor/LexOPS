context("generate() errors")

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

# general errors ----
testthat::test_that("general errors", {
  testthat::expect_error({
    # randomly repeat between 1 and 5 rows
    n_reps <- sample(1:5, size=1)
    rep_rows <- sample.int(nrow(eg_df), n_reps)
    eg_df_reps <- rbind(eg_df, eg_df[rep_rows, ])

    eg_df_reps %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:-0.1 ~ 0.1:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10)
    },
    "LexOPS assumes that id_col uniquely identifies the rows",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:-0.1 ~ 0.1:5) %>%
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
      split_by(a, -5:-0.1 ~ 0.1:5) %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10, match_null = "irreverent"),
    "Unknown match_null; expected \"inclusive\", \"random\", \"balanced\", \"first\", or a specific condition (e.g. \"A2_B1_C1\")",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_by(a, -5:-0.1 ~ 0.1:5) %>%
      generate(10),
    "No controls defined - see ?LexOPS::generate for example usage of generate()",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      split_random(2) %>%
      generate(10),
    "No controls defined - see ?LexOPS::generate for example usage of generate()",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      control_for(b, -2.5:2.5) %>%
      control_for(c, -2.5:2.5) %>%
      control_for(d) %>%
      generate(10),
    "No splits defined - see ?LexOPS::generate for example usage of generate()",
    fixed = TRUE
  )

  testthat::expect_error(
    eg_df %>%
      set_options(id_col = "id") %>%
      control_for_euc(c(b, c), -2.5:2.5) %>%
      generate(10),
    "No splits defined - see ?LexOPS::generate for example usage of generate()",
    fixed = TRUE
  )
})
