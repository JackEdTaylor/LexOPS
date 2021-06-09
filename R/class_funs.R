#' Print a summary about an ungenerated LexOPS pipeline
#'
#' Prints a summary about a LexOPS pipeline object (returned from functions like `split_by()`, `control_for()`, etc.), listing the splits and controls in the pipeline so far.
#'
#' @param x A LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param ... Other arguments passed to or from other methods.
#'
#' @export

print.LexOPS_pipeline <- function(x, ...) {
  cat(format(x, ...), "\n")
}

format.LexOPS_pipeline <- function(x, ...) {
  lp_info <- x$info

  Ns <- lapply(lp_info, length)

  factorial_Ns <- sapply(lp_info$splits, function(x) length(x)-1)
  factorial_summ <- sprintf("%s level factorial design", paste(factorial_Ns, collapse=" x "))

  splits_txt <- sapply(lp_info$splits, function(x) {
    name <- x[[1]]
    levels <- x[2:length(x)] %>%
      sapply(paste, collapse=":") %>%
      paste(collapse = " ~ ")
    paste(name, levels, sep = ", ")
  }) %>%
    sprintf("  %s", .) %>%
    paste(collapse = "\n") %>%
    sprintf("%g Splits:\n%s", Ns$splits, .)

  controls_txt <- sapply(lp_info$controls, function(x) {
    name <- x[[1]]
    if (length(x)>1) {
      tol <- paste(x[[2]], collapse=":")
      paste(name, tol, sep = ", ")
    } else {
      name
    }
  }) %>%
    sprintf("  %s", .) %>%
    paste(collapse = "\n") %>%
    sprintf("%g Controls:\n%s", Ns$controls, .)

  control_funs_txt <- sapply(lp_info$control_functions, function(x) {
    name <- x[[1]]
    fun_var <- x[[3]]
    fun_tol <- paste(x[[4]], collapse=":")
    paste(c(name, fun_var, fun_tol), collapse=", ")
  }) %>%
    sprintf("  %s", .) %>%
    paste(collapse = "\n") %>%
    sprintf("%g Control functions:\n%s", Ns$control_functions, .)

  c(factorial_summ, splits_txt, controls_txt, control_funs_txt) %>%
    paste(collapse = "\n\n")
}

is.LexOPS_pipeline <- function(x) {
  "LexOPS_pipeline" %in% class(x)
}

as.LexOPS_pipeline <- function(x) {
  if (is.LexOPS_pipeline(x)) {
    x
  } else if (is.data.frame(x)) {
    lp <- list(df = x)
    class(lp) <- "LexOPS_pipeline"
    lp
  } else {
    stop("Expected data.frame object")
  }
}
