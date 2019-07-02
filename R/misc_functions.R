#' Prepend a Vector
#'
#' Copied from old versions of rlang as function now deprecated.
#'
#' @param x the vector to be modified.
#' @param values to be included in the modified vector.
#' @param before a subscript, before which the values are to be appended.
#'
#' @return A merged vector.
#'
#' @examples
#'
#' #get_box_colour("primary")

function (x, values, before = 1) {
  n <- length(x)
  stopifnot(before > 0 && before <= n)
  if (before == 1) {
    c(values, x)
  } else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}
