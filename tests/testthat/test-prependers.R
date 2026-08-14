context("prependers")

# prepend() ----
testthat::test_that("prepend()", {
  # can prepend with integers
  testthat::expect_equal(
    LexOPS:::prepend(c(10, 4.2, 50, "hello"), 9),
    c(9, 10, 4.2, 50, "hello")
  )
  # can prepend with character vectors
  testthat::expect_equal(
    LexOPS:::prepend(c("world", 9, 10, 4, 50), "hello"),
    c("hello", "world", 9, 10, 4, 50)
  )
  # can insert to non-first locations
  # (note this functionality is not actually used in the package)
  testthat::expect_equal(
    LexOPS:::prepend(c(10, 4, 50, "hello"), 9, before=3),
    c(10, 4, 9, 50, "hello")
  )
  # can prepend with a vector
  testthat::expect_equal(
    LexOPS:::prepend(c(10, 4.2, 50, "hello"), c(9, 3.2, "z")),
    c(9, 3.2, "z", 10, 4.2, 50, "hello")
  )
  # can insert a vector
  testthat::expect_equal(
    LexOPS:::prepend(c(10, 4.2, 50, "hello"), c(9, 3.2, "z"), before=2),
    c(10, 9, 3.2, "z", 4.2, 50, "hello")
  )
})

# prepend_list() ----
testthat::test_that("prepend_list()", {
  # can prepend a list with nesting
  testthat::expect_equal(
    LexOPS:::prepend_list(list(10, 4, list(50, "hello")), 9),
    list(9, 10, 4, list(50, "hello"))
  )
  # can prepend a list with a vector
  testthat::expect_equal(
    LexOPS:::prepend_list(list(10, 4, list(50, "hello")), c("hello", "world")),
    list(c("hello", "world"), 10, 4, list(50, "hello"))
  )
  # prepending a list with a list will nest
  testthat::expect_equal(
    LexOPS:::prepend_list(list(3, 4, list(5, 6)), list("hello", "world")),
    list(list("hello", "world"), 3, 4, list(5, 6))
  )
  # if input is not a list, don't nest (keep original lack of nesting)
  # (note this functionality is not actually used in the package)
  testthat::expect_equal(
    LexOPS:::prepend_list(c(3, 4, 5), list("hello", "world")),
    list("hello", "world", 3, 4, 5)
  )
})
