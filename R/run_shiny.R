#' Run the LeXOPS Shiny App
#'
#' Runs the LexOPS Shiny App, with optional additional arguments.
#'
#' @param ... optional arguments to `shiny::runApp()`
#'
#' @export

run_shiny <- function(...) {
  appDir <- system.file("shiny-app", package = "LexOPS")
  if (appDir == "") {
    stop("Could not find the directory for the LexOPS Shiny App. Try re-installing `LexOPS`.")
  }
  shiny::runApp(appDir, ...)
}

