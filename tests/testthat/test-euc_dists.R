context("euc_dists()")

# setup ----
eg_df <- data.frame(
  id = as.character(1:100),
  a = rnorm(100, 100, 10),
  b = rnorm(100),
  c = rnorm(100),
  d = sample(c("y", "z"), 100, replace=TRUE)
)

# euc_dists() ----
testthat::test_that("euc_dists()", {
  # returns the correct number of outputs
  testthat::expect_equal(
    eg_df |>
      euc_dists(1, c(a, b, c), id_col="id") |>
      length(),
    nrow(eg_df)
  )
  # should be zero distance from the selected item
  testthat::expect_equal(
    euc_dists(eg_df, 5, c(a, b, c), id_col="id")[[5]],
    0
  )
  # check that the distances are correct
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=FALSE, center=FALSE),
    {
      targ <- eg_df[eg_df$id==50,]

      adist <- eg_df$a - targ$a
      bdist <- eg_df$b - targ$b
      cdist <- eg_df$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the distances are the same with non-standard evaluation
  testthat::expect_equal(
    euc_dists(eg_df, 50, c("a", "b", "c"), id_col="id", scale=FALSE, center=FALSE, standard_eval=TRUE),
    {
      targ <- eg_df[eg_df$id==50,]

      adist <- eg_df$a - targ$a
      bdist <- eg_df$b - targ$b
      cdist <- eg_df$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the scaling works as expected
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=TRUE, center=FALSE),
    {
      eg_df_sc <- eg_df
      eg_df_sc$a <- as.numeric(scale(eg_df_sc$a, scale=TRUE, center=FALSE))
      eg_df_sc$b <- as.numeric(scale(eg_df_sc$b, scale=TRUE, center=FALSE))
      eg_df_sc$c <- as.numeric(scale(eg_df_sc$c, scale=TRUE, center=FALSE))

      targ <- eg_df_sc[eg_df_sc$id==50,]

      adist <- eg_df_sc$a - targ$a
      bdist <- eg_df_sc$b - targ$b
      cdist <- eg_df_sc$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the centring works as expected
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=FALSE, center=TRUE),
    {
      eg_df_sc <- eg_df
      eg_df_sc$a <- as.numeric(scale(eg_df_sc$a, scale=FALSE, center=TRUE))
      eg_df_sc$b <- as.numeric(scale(eg_df_sc$b, scale=FALSE, center=TRUE))
      eg_df_sc$c <- as.numeric(scale(eg_df_sc$c, scale=FALSE, center=TRUE))

      targ <- eg_df_sc[eg_df_sc$id==50,]

      adist <- eg_df_sc$a - targ$a
      bdist <- eg_df_sc$b - targ$b
      cdist <- eg_df_sc$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the scaling and centring can be combined
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=TRUE, center=TRUE),
    {
      eg_df_sc <- eg_df
      eg_df_sc$a <- as.numeric(scale(eg_df_sc$a, scale=TRUE, center=TRUE))
      eg_df_sc$b <- as.numeric(scale(eg_df_sc$b, scale=TRUE, center=TRUE))
      eg_df_sc$c <- as.numeric(scale(eg_df_sc$c, scale=TRUE, center=TRUE))

      targ <- eg_df_sc[eg_df_sc$id==50,]

      adist <- eg_df_sc$a - targ$a
      bdist <- eg_df_sc$b - targ$b
      cdist <- eg_df_sc$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the weighting works as expected
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=TRUE, center=TRUE, weights=c(1.1, 5, 2.76), standardise_weights=FALSE),
    {
      eg_df_sc <- eg_df
      eg_df_sc$a <- as.numeric(scale(eg_df_sc$a, scale=TRUE, center=TRUE))
      eg_df_sc$b <- as.numeric(scale(eg_df_sc$b, scale=TRUE, center=TRUE))
      eg_df_sc$c <- as.numeric(scale(eg_df_sc$c, scale=TRUE, center=TRUE))

      targ <- eg_df_sc[eg_df_sc$id==50,]

      adist <- 1.1 * (eg_df_sc$a - targ$a)
      bdist <- 5 * (eg_df_sc$b - targ$b)
      cdist <- 2.76 * (eg_df_sc$c - targ$c)

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # check that the standardised weights work as expected
  testthat::expect_equal(
    euc_dists(eg_df, 50, c(a, b, c), id_col="id", scale=TRUE, center=TRUE, weights=c(1.1, 5, 2.76), standardise_weights=TRUE),
    {
      w <- c(1.1, 5, 2.76)
      w <- w / mean(w)

      eg_df_sc <- eg_df
      eg_df_sc$a <- as.numeric(scale(eg_df_sc$a, scale=TRUE, center=TRUE))
      eg_df_sc$b <- as.numeric(scale(eg_df_sc$b, scale=TRUE, center=TRUE))
      eg_df_sc$c <- as.numeric(scale(eg_df_sc$c, scale=TRUE, center=TRUE))

      targ <- eg_df_sc[eg_df_sc$id==50,]

      adist <- w[1] * (eg_df_sc$a - targ$a)
      bdist <- w[2] * (eg_df_sc$b - targ$b)
      cdist <- w[3] * (eg_df_sc$c - targ$c)

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # uses all numeric columns if requested
  testthat::expect_equal(
    euc_dists(eg_df, 50, "all", id_col="id", scale=FALSE, center=FALSE),
    {
      targ <- eg_df[eg_df$id==50,]

      adist <- eg_df$a - targ$a
      bdist <- eg_df$b - targ$b
      cdist <- eg_df$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # uses all numeric columns if no specific columns are provided
  testthat::expect_equal(
    euc_dists(eg_df, 50, id_col="id", scale=FALSE, center=FALSE),
    {
      targ <- eg_df[eg_df$id==50,]

      adist <- eg_df$a - targ$a
      bdist <- eg_df$b - targ$b
      cdist <- eg_df$c - targ$c

      sqrt(adist**2 + bdist**2 + cdist**2)
    }
  )
  # informative warning if no numeric variables
  testthat::expect_warning(
    euc_dists(eg_df[, c("id", "d")], 1, id_col="id"),
    regexp = "No numeric columns detected in `df`",
    fixed = TRUE
  )
  # all values are NA if no numeric variables
  testthat::expect_all_true(
    is.na(suppressWarnings(
      euc_dists(eg_df[, c("id", "d")], 1, id_col="id")
    ))
  )
  # informative warning if no numeric variables provided
  testthat::expect_warning(
    euc_dists(eg_df, 1, c(), id_col="id", standard_eval=TRUE),
    regexp = "No numeric columns specified in `vars`",
    fixed = TRUE
  )
  # all values are NA if no numeric variables provided
  testthat::expect_all_true(
    is.na(suppressWarnings(
      euc_dists(eg_df, 1, c(), id_col="id", standard_eval=TRUE)
    ))
  )
})

# euc_dists() errors ----
testthat::test_that("euc_dists() errors", {
  # informative error if not a dataframe
  testthat::expect_error(
    euc_dists("uh oh!", 1, c(a, b, c), id_col="id"),
    regexp = "Expected df to be of class data frame, not character",
    fixed = TRUE
  )
  # informative error if only non-numeric variables provided
  testthat::expect_error(
    euc_dists(eg_df, 1, d, id_col="id"),
    regexp = "0 non-numeric columns specified in `vars`",
    fixed = TRUE
  )
  # informative error if unknown variables provided
  testthat::expect_error(
    euc_dists(eg_df, 1, c(missing1, d, missing2), id_col="id"),
    regexp = "2 unknown columns in `df`: missing1, missing2",
    fixed = TRUE
  )
  # informative error if non-character vector passed as id_col
  testthat::expect_error(
    euc_dists(eg_df, 1, c(a, b, c), id_col=1),
    regexp = "Expected id_col to be of class string, not numeric",
    fixed = TRUE
  )
})
