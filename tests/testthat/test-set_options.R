context("set_options()")

# setup ----
set.seed(1)
eg_df <- data.frame(
  id_int = 1:10,
  id_char = as.character(1:10),
  id_float = (1:10)/2,
  id_fct = factor((1:10)/2),
  a = factor(rep(c("y", "z"), 5)),
  b = rnorm(10)
)

# set_options ----
testthat::test_that("set_options()", {
  # set_options() has to be the first function in a pipeline
  testthat::expect_error(
    eg_df |>
      split_by(a, "y" ~ "z") |>
      set_options(id_col = "id_char"),
    regexp = "`set_options()` must be the first function run in a generate pipeline",
    fixed = TRUE
  )
  # when passing an id_col, the value must be a (standard evaluation) character string
  testthat::expect_error(
    eg_df |>
      set_options(id_col = 1),
    regexp = "`id_col` must be a character vector of length 1",
    fixed = TRUE
  )
  # when passing a cond_col, the value must be a (standard evaluation) character string
  testthat::expect_error(
    eg_df |>
      set_options(cond_col = 99),
    regexp = "`cond_col` must be a character vector of length 1",
    fixed = TRUE
  )
  # the id column can be integers
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id_int") |>
      split_by(a, "y" ~ "z") |>
      control_for(b, -2:2) |>
      generate(2, silent=TRUE) |>
      nrow(),
    2
  )
  # the id column can be floats
  testthat::expect_equal(
    eg_df |>
      set_options(id_col = "id_float") |>
      split_by(a, "y" ~ "z") |>
      control_for(b, -2:2) |>
      generate(2, silent=TRUE) |>
      nrow(),
    2
  )
  # the id column can be factors
  testthat::expect_equal(
    suppressWarnings(
      eg_df |>
        set_options(id_col = "id_fct") |>
        split_by(a, "y" ~ "z") |>
        control_for(b, -2:2) |>
        generate(2, silent=TRUE) |>
        nrow()
    ),
    2
  )
  # if a factor, should be warned about coercion
  testthat::expect_warning(
    eg_df |>
      set_options(id_col = "id_fct") |>
      split_by(a, "y" ~ "z") |>
      control_for(b, -2:2) |>
      generate(2, silent=TRUE),
    regexp = "id_col 'id_fct' is a factor; will be coerced to character vector representation of factor levels",
    fixed = TRUE
  )
})
