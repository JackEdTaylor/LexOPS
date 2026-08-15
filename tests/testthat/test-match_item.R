context("match_item()")

# setup ----
eg_df <- data.frame(
  id = 1:10,
  a = c(21, 3, 32.4, -12, 8787, 9, 24, 43.1112, -193, -1.3),
  b = c(-2, -2, -1, -1, 0, 0, 1, 1, 2, 2),
  c = rep(c("a", "b"), 5)
)

# match_item() ----
testthat::test_that("match_item()", {
  # returns a dataframe
  testthat::expect_s3_class(
    match_item(eg_df, 3, a=-20:20, c, id_col="id"),
    "data.frame"
  )
  # returns all items except the target if tolerances are inf
  testthat::expect_equal(
    nrow( match_item(eg_df, 3, a=-Inf:Inf, b=-Inf:Inf, id_col="id") ),
    nrow( eg_df ) - 1
  )
  # all columns from the original dataframe are present
  testthat::expect_contains(
    colnames( match_item(eg_df, 3, a=-Inf:Inf, b=-Inf:Inf, id_col="id") ),
    colnames( eg_df )
  )
  # returns items ordered by Euclidean distance if tolerances are inf
  testthat::expect_equal(
    match_item(eg_df, 3, a=-Inf:Inf, b=-Inf:Inf, id_col="id")$id,
    order( euc_dists(eg_df, 3, c(a, b), id_col="id") )[2:nrow(eg_df)]
  )
  # tolerances restrict the returned items to only those that fit
  testthat::expect_equal(
    match_item(eg_df, 3, a=-20:20, c, id_col="id")$id,
    c(7, 1)
  )
  # can use tolerances to just match by a non-numeric variable
  testthat::expect_equal(
    match_item(eg_df, 3, c, id_col="id")$id,
    eg_df$id[eg_df$c == eg_df$c[eg_df$id==3] & eg_df$id!=3]
  )
  # missing tolerances for numeric variables are interpreted as exact matching
  testthat::expect_equal(
    match_item(eg_df, 1, b, id_col="id")$id,
    2
  )
  # if only matching by a non-numeric variable, the Euclidean distances should all be NA
  testthat::expect_all_true(
    is.na(match_item(eg_df, 1, c, id_col="id")$euclidean_distance)
  )
  # tolerances do not restrict the returned items if filter==FALSE
  testthat::expect_contains(
    match_item(eg_df, 3, a=-20:20, c, id_col="id", filter=FALSE)$id,
    eg_df$id[eg_df$id!=3]
  )
  # can use standard evaluation
  testthat::expect_equal(
    match_item(eg_df, 8, a=-20:20, c, id_col="id"),
    match_item(eg_df, 8, list(c("a", -20, 20), "c"), id_col="id", standard_eval=TRUE)
  )
})

# match_item() errors ----
testthat::test_that("match_item()", {
  # df is not a dataframe
  testthat::expect_error(
    match_item("uh oh!", 3, a=-Inf:Inf, b=-20:20, c, id_col="id"),
    regexp = "Expected df to be of class data frame, not character",
    fixed = TRUE
  )
  # any existing column called "euclidean_distance" will be removed
  testthat::expect_warning(
    {
      eg_df2 <- eg_df
      eg_df2$euclidean_distance <- NA
      match_item(eg_df2, 3, a=-Inf:Inf, b=-20:20, c, id_col="id")
    },
    regexp = "\"euclidean_distance\" column will be ignored, as this is overwritten by `match_item()`",
    fixed = TRUE
  )
  # id_col should be passed as a string
  testthat::expect_error(
    match_item(eg_df, 3, a=-Inf:Inf, b=-20:20, c, id_col=1),
    regexp = "Expected id_col to be of class character, not numeric",
    fixed = TRUE
  )
  # any missing variables should elicit a warning
  testthat::expect_error(
    match_item(eg_df, 3, a=-50:50, missing1=-10:12, missing2, id_col="id"),
    regexp = "^Missing 2 variables in df\\:.*missing1.*missing2$"
  )
  # wrong number of tolerances
  testthat::expect_error(
    match_item(eg_df, 1, a=-1:0:1, id_col="id"),
    regexp = "^1 variables misspecified\\:.*a - expected list object to be of length 1 \\(no tolerances\\) or 3 \\(with tolerances\\), not 4$"
  )
  # numeric tolerances for a non-numeric variable
  testthat::expect_error(
    match_item(eg_df, 1, c=-20:20, id_col="id"),
    regexp = "^1 variables misspecified\\:.*c - did not expect tolerances for non-numeric variable$"
  )
  # id_col is missing
  testthat::expect_error(
    match_item(eg_df, 6, c, id_col="missing1"),
    regexp = "id_col 'missing1' not found in df",
    fixed = TRUE
  )
  # value is missing
  testthat::expect_error(
    match_item(eg_df, 999, c, id_col="id"),
    regexp = "'999' not found in 'id' column of df",
    fixed = TRUE
  )
})

# match_word() ----
testthat::test_that("match_word()", {
  # deprecated match_word() function uses match_item()
  testthat::expect_equal(
    suppressWarnings(
      match_word(eg_df, 3, a=-Inf:Inf, b=-20:20, c, id_col="id")
    ),
    match_item(eg_df, 3, a=-Inf:Inf, b=-20:20, c, id_col="id")
  )
  # warning about deprecation
  testthat::expect_warning(
    match_word(eg_df, 3, a=-Inf:Inf, b=-20:20, c, id_col="id"),
    regexp = "`match_word() is now outdated. Please use `match_item()`",
    fixed = TRUE
  )
})
