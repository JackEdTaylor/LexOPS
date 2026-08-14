context("parsers")

# parse_levels() ----
testthat::test_that("parse_levels()", {
  # check numeric variables
  # (substituted because of non-standard eval)
  testthat::expect_equal(
    parse_levels(substitute(numeric_var), substitute(1:2 ~ 3.75:4 ~ 5:6.999)),
    list("numeric_var", c(1, 2), c(3.75, 4), c(5, 6.999))
  )
  # check character variables
  testthat::expect_equal(
    parse_levels(substitute(character_var), substitute("L1" ~ "L2" ~ "L3" ~ "L4")),
    list("character_var", "L1", "L2", "L3", "L4")
  )
  # check character variables can be grouped
  testthat::expect_equal(
    parse_levels(substitute(character_var), substitute("L1" ~ c("L2a", "L2b") ~ "L3" ~ "L4")),
    list("character_var", "L1", c("L2a", "L2b"), "L3", "L4")
  )
  # variables can be grouped
  # (no current use case?)
  testthat::expect_equal(
    parse_levels(substitute(c(character_var, numeric_var)), substitute("L1" ~ "L2" ~ "L3")),
    list(c("character_var", "numeric_var"), "L1", "L2", "L3")
  )
  # support for missing levels, with NULL coerced to NA
  testthat::expect_equal(
    parse_levels(substitute(numeric_var), substitute(NA ~ 3.75:4 ~ NULL ~ 5:6.999)),
    list("numeric_var", NA, c(3.75, 4), NA, c(5, 6.999))
  )
  # can provide just a variable
  testthat::expect_equal(
    parse_levels(substitute(my_var)),
    list("my_var")
  )
  # can provide just grouped variables
  testthat::expect_equal(
    parse_levels(substitute(c(variable1, variable2))),
    list(c("variable1", "variable2"))
  )
})

# parse_ellipsis() ----
testthat::test_that("parse_ellipsis()", {
  # check combining multiple numeric and character variables
  testthat::expect_equal(
    parse_ellipsis(substitute(c(numeric1 = 0:0, character1, numeric2 = -0.1:0.1, character2))),
    list(
      c("numeric1", 0, 0),
      "character1",
      c("numeric2", -0.1, 0.1),
      "character2"
    )
  )
})

# parse_unvectorise() ----
testthat::test_that("parse_unvectorise()", {
  testthat::expect_equal(
    LexOPS:::parse_unvectorise("c(hello, world)"),
    c("hello", "world")
  )
})
