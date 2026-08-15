context("long_format()")

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

n_items <- 5

stim1 <- eg_df |>
  set_options(id_col = "id") |>
  split_by(a, -5:-0.1 ~ 0.1:5) |>
  control_for(d) |>
  generate(n_items, silent=TRUE)

# long_format() ----
testthat::test_that("long_format()", {
  # using long_format() provides a dataframe
  testthat::expect_s3_class(
    long_format(stim1),
    "data.frame"
  )
  # each row is distinct and the dataframe has the expected number of items
  testthat::expect_equal(
    nrow( dplyr::distinct(long_format(stim1)) ),
    n_items * 2  # n_items is per condition
  )
  # by default, returns the variables in the design (also checks order)
  testthat::expect_equal(
    colnames(long_format(stim1)),
    c("item_nr", "condition", "match_null", "id", "a", "d")
  )
  # requesting the design variables explicitly also provides all variables in the design (also checks order)
  testthat::expect_equal(
    colnames(long_format(stim1, include="design")),
    c("item_nr", "condition", "match_null", "id", "a", "d")
  )
  # can request all splits (also check order is as expected)
  testthat::expect_equal(
    colnames(long_format(stim1, include="splits")),
    c("item_nr", "condition", "match_null", "id", "a")
  )
  # can request all controls (also check order is as expected)
  testthat::expect_equal(
    colnames(long_format(stim1, include="controls")),
    c("item_nr", "condition", "match_null", "id", "d")
  )
  # can request all variables from the original df (also check order is as expected)
  testthat::expect_equal(
    colnames(long_format(stim1, include="all")),
    c("item_nr", "condition", "match_null", "id", "a", "b", "c",
      "d", "e", "f", "g", "LexOPS_splitCond_A", "LexOPS_splitCond")
  )
})

# long_format() candidates ----
# tests can request all candidates from the original dataframe
testthat::test_that("long_format() candidates", {
  # should be at least as many rows as in the generated items
  testthat::expect_gte(
    nrow( long_format(stim1, include_candids = TRUE) ),
    nrow(stim1)
  )
  # should be at most as many rows as in the original dataframe
  testthat::expect_lte(
    nrow( long_format(stim1, include_candids = TRUE) ),
    nrow(eg_df)
  )
  # if including the candidates, all the used candidates should also be present
  testthat::expect_contains(
    long_format(stim1, include_candids = TRUE)$id,
    long_format(stim1, include_candids = FALSE)$id
  )
})

# long_format() errors ----
testthat::test_that("long_format() errors", {
  # get informative error if run on a dataframe that has not gone through a LexOPS pipeline
  testthat::expect_error(
    long_format(eg_df),
    regexp = "`long_format()` should only be run on a dataframe generated through the LexOPS `generate()` function",
    fixed = TRUE
  )
  # get informative error if something other than a dataframe is passed
  testthat::expect_error(
    long_format("uh oh!"),
    regexp = "Expected df to be of class data frame, not character",
    fixed = TRUE
  )
  # get an informative error if request an unexpected `include` argument
  testthat::expect_error(
    long_format(stim1, include="everything"),
    regexp = "`include` must be one of \"all\", \"design\", \"splits\", \"controls\", or NA",
    fixed = TRUE
  )
})
