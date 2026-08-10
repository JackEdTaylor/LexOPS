#' Print a summary about an ungenerated LexOPS pipeline
#'
#' Prints a summary about a LexOPS pipeline object (returned from functions like `split_by()`, `control_for()`, etc.), listing the splits and controls in the pipeline so far.
#'
#' @param x A LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param ... Other arguments passed to or from other methods (unused).
#'
#' @export

print.LexOPS_pipeline <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Get a text summary about a LexOPS pipeline
#'
#' Generates a text summary of a LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#'
#' @param x A LexOPS_pipeline object resulting from one of `split_by()`, `control_for()`, etc..
#' @param ... Other arguments passed to or from other methods (unused).
#'
#' @export

format.LexOPS_pipeline <- function(x, ...) {
  lp_info <- x$info

  Ns <- lapply(lp_info, length)

  factorial_Ns <- if (length(lp_info$splits)==0) "?" else sapply(lp_info$splits, function(x) length(x)-1)
  factorial_summ <- sprintf("%s level factorial design", paste(factorial_Ns, collapse=" x "))

  splits_txt <- sapply(lp_info$splits, function(x) {
    name <- x[[1]]
    levels <- sapply(x[2:length(x)], paste, collapse = ":")
    levels <- paste(levels, collapse = " ~ ")
    paste(name, levels, sep = ", ")
  })
  splits_txt <- sprintf("  %s", splits_txt)
  splits_txt <- paste(splits_txt, collapse = "\n")
  splits_txt <- sprintf("%g Splits:\n%s", Ns$splits, splits_txt)

  controls_txt <- sapply(lp_info$controls, function(x) {
    name <- x[[1]]
    if (length(x) > 1) {
      tol <- paste(x[[2]], collapse = ":")
      paste(name, tol, sep = ", ")
    } else {
      name
    }
  })
  controls_txt <- sprintf("  %s", controls_txt)
  controls_txt <- paste(controls_txt, collapse = "\n")
  controls_txt <- sprintf("%g Controls:\n%s", Ns$controls, controls_txt)

  control_funs_txt <- sapply(lp_info$control_functions, function(x) {
    name <- x[[1]]
    fun_var <- x[[3]]
    fun_tol <- paste(x[[4]], collapse = ":")
    paste(c(name, fun_var, fun_tol), collapse = ", ")
  })
  control_funs_txt <- sprintf("  %s", control_funs_txt)
  control_funs_txt <- paste(control_funs_txt, collapse = "\n")
  control_funs_txt <- sprintf("%g Control functions:\n%s", Ns$control_functions, control_funs_txt)

  out <- c(factorial_summ, splits_txt, controls_txt, control_funs_txt)
  paste(out, collapse = "\n\n")
}

#' Check whether an object is of class LexOPS_pipeline
#'
#' @param x Object resulting from one of `split_by()`, `control_for()`, etc..
#'
#' @export

is.LexOPS_pipeline <- function(x) {
  "LexOPS_pipeline" %in% class(x)
}

#' Set an object's class to LexOPS_pipeline
#'
#' @param x Object resulting from one of `split_by()`, `control_for()`, etc..
#'
#' @export

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
