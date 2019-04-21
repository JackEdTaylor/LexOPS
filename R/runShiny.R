#' runShiny
#'
#' Runs the LexOPS Shiny App.
#'
#' @export

runShiny <- function() {
  appDir <- system.file("shiny-app", package = "LexOPS")
  if (appDir == "") {
    stop("Could not find the shiny app's directory. Try re-installing `LexOPS`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
