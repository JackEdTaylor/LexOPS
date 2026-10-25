#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#'
#' @examples
#' c(1, 4, 2, 7) %>%
#'   sum()
#'
#' @usage lhs \%>\% rhs
#'
#' @returns No return value (syntactic alias for `rhs(lhs())`).
NULL

#' Walrus operator
#'
#' See \code{rlang::\link[rlang]{quasiquotation}} for details.
#'
#' @examples
#' # example from rlang::quasiquotation
#' name <- "foo"
#'
#' rbind2 <- function(...) {
#'   do.call("rbind", rlang::list2(...))
#' }
#'
#' rbind2("{name}" := 1:2, bar = 3:4)
#'
#' @name :=
#' @rdname walrus
#' @keywords internal
#' @export
#' @importFrom rlang :=
#'
#' @returns No return value (syntactic alias for `=`).
NULL
