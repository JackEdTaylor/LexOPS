#' Print a summary about an ungenerated LexOPS pipeline
#'
#' Prints a summary about a LexOPS pipeline object (returned from functions like \code{\link{split_by}}, \code{\link{control_for}}, etc.), listing the splits and controls in the pipeline so far.
#'
#' @param x A `LexOPS_pipeline` object resulting from one of \code{\link{split_by}}, \code{\link{control_for}}, etc., which has not yet been passed through \code{\link{generate}}.
#' @param ... Other arguments passed to or from other methods (unused).
#'
#' @return No return value, called for side effects (printing).
#'
#' @examples
#' pipeline <- lexops |>
#'   subset(PK.Brysbaert >= .75) |>
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(Length, 0:0)
#'
#' pipeline
#'
#'
#' @export

print.LexOPS_pipeline <- function(x, ...) {
  cat(format(x, ...), "\n")
}

#' Get a text summary about a LexOPS pipeline
#'
#' Generates a text summary of a `LexOPS_pipeline` object resulting from one of \code{\link{split_by}}, \code{\link{control_for}}, etc..
#'
#' @param x A `LexOPS_pipeline` object resulting from one of \code{\link{split_by}}, \code{\link{control_for}}, etc., which has not yet been passed through \code{\link{generate}}.
#' @param ... Other arguments passed to or from other methods (unused).
#'
#' @return A character vector containing a formatted summary of the LexOPS pipeline.
#'
#' @examples
#' pipeline <- lexops |>
#'   subset(PK.Brysbaert >= .75) |>
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(Length, 0:0)
#'
#' format(pipeline)
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

#' Check whether an object is of class `LexOPS_pipeline`
#'
#' @param x Object, possibly resulting from one of \code{\link{split_by}}, \code{\link{control_for}}, etc..
#'
#' @return A logical value: `TRUE` if `x` is of class `LexOPS_pipeline`, or `FALSE` otherwise.
#'
#' @examples
# the lexops dataset is not a LexOPS_pipeline before going through split_by() or control_for()
#' is.LexOPS_pipeline(lexops)  # FALSE
#'
#' # a pipeline pregeneration is a LexOPS_pipeline
#' pregen <- lexops |>
#'   subset(PK.Brysbaert >= .75) |>
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
#'   control_for(Length, 0:0)
#'
#' is.LexOPS_pipeline(pregen)  # TRUE
#'
#' # a pipeline's output is still a LexOPS_pipeline after going through generate()
#' postgen <- generate(pregen, 10)
#' is.LexOPS_pipeline(postgen)  # TRUE
#'
#' @export

is.LexOPS_pipeline <- function(x) {
  "LexOPS_pipeline" %in% class(x)
}

#' Set an object's class to `LexOPS_pipeline`
#'
#' @param x Object, ultimately of class `data.frame`, possibly resulting from one of \code{\link{split_by}}, \code{\link{control_for}}, etc..
#'
#' @return `x`, coerced into class `LexOPS_pipeline`
#'
#' @examples
#' as.LexOPS_pipeline(lexops)
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
