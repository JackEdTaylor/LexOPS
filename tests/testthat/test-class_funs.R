context("class functions")

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

# identity and coercion ----
testthat::test_that("identity and coercion", {
  # the class of a pipeline is LexOPS_pipeline
  testthat::expect_s3_class(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      control_for(d),
    "LexOPS_pipeline"
  )
  # is.LexOPS_pipeline() detects the pipeline
  testthat::expect_true(
    is.LexOPS_pipeline(
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.1 ~ 0.1:5) |>
        control_for(d)
    )
  )
  # is.LexOPS_pipeline() doesn't detect non-pipelines as pipelines
  testthat::expect_false(
    is.LexOPS_pipeline(eg_df)
  )
  # can coerce dataframes to LexOPS_pipeline
  testthat::expect_s3_class(as.LexOPS_pipeline(eg_df), "LexOPS_pipeline")
  # coercing something that is already a LexOPS_pipeline to a LexOPS_pipeline preserves the class
  testthat::expect_s3_class(as.LexOPS_pipeline(as.LexOPS_pipeline(eg_df)), "LexOPS_pipeline")
  # can not coerce non-dataframe objects to LexOPS_pipeline
  testthat::expect_error(
    as.LexOPS_pipeline("uh oh!"),
    regexp = "Expected data.frame object",
    fixed = TRUE
  )
})

# formatting ----
testthat::test_that("formatting", {
  # one numeric split and one numeric control
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      control_for(b, -1:1) |>
      format(),
    "2 level factorial design\n\n1 Splits:\n  a, -5:-0.1 ~ 0.1:5\n\n1 Controls:\n  b, -1:1"
  )
  # two numeric splits and two numeric controls
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      split_by(b, -3:-0.2 ~ 0.2:3) |>
      control_for(c, -1:1) |>
      control_for(e, -2.3:4.989) |>
      format(),
    "2 x 2 level factorial design\n\n2 Splits:\n  a, -5:-0.1 ~ 0.1:5\n  b, -3:-0.2 ~ 0.2:3\n\n2 Controls:\n  c, -1:1\n  e, -2.3:4.989"
  )
  # numeric and non-numeric splits
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      split_by(d, "a" ~ "b" ~ "c") |>
      control_for(c, -1:1) |>
      format(),
    "2 x 3 level factorial design\n\n2 Splits:\n  a, -5:-0.1 ~ 0.1:5\n  d, a ~ b ~ c\n\n1 Controls:\n  c, -1:1"
  )
  # numeric and non-numeric controls
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      control_for(c, -1:1) |>
      control_for(d) |>
      format(),
    "2 level factorial design\n\n1 Splits:\n  a, -5:-0.1 ~ 0.1:5\n\n2 Controls:\n  c, -1:1\n  d"
  )
  # control functions
  testthat::expect_equal(
    {
      library(stringdist)
      eg_df |>
        set_options(id_col = "id") |>
        split_by(a, -5:-0.1 ~ 0.1:5) |>
        control_for_map(stringdist, f, 0:2, method="lv") |>
        format()
    },
    "2 level factorial design\n\n1 Splits:\n  a, -5:-0.1 ~ 0.1:5\n\n1 Control functions:\n  control_map_1, f, 0:2"
  )
  # printing a pipeline prints its formatted character vector
  testthat::expect_output(
    eg_df |>
      set_options(id_col = "id") |>
      split_by(a, -5:-0.1 ~ 0.1:5) |>
      control_for(b, -1:1) |>
      print(),
    regexp = "2 level factorial design\n\n1 Splits:\n  a, -5:-0.1 ~ 0.1:5\n\n1 Controls:\n  b, -1:1",
    fixed = TRUE
  )
})
