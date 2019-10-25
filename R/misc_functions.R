#' Prepend a Vector
#'
#' Copied from old versions of rlang as function now deprecated.
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended.
#'
#' @return A merged vector.

prepend <- function (x, values, before = 1) {
  n <- length(x)
  stopifnot(before > 0 && before <= n)
  if (before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}

#' Prepend a List
#'
#' A prepend function that plays nicely with lists.
#'
#' @param x the vector to be modified.
#' @param ... the objects to be included in the modified vector.
#'
#' @return A merged vector.
#'
#' @examples
#' # prepend_list(list(c("test", "two"), c("test", "three")), c("test", "one"))

prepend_list <- function (x, ...) {
  if (is.list(x)) {
    c(list(...), x)
  } else {
    c(..., x)
  }
}
