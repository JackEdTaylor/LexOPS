#' Run the LeXOPS Shiny App
#'
#' Runs the LexOPS Shiny App, with optional additional arguments.
#'
#' The LexOPS shiny app requires additional packages. If any dependencies are missing, the user will be given an error and installation instructions. The additional packages required are:
#' * `shiny`
#' * `shinydashboard`
#' * `plotly`
#' * `ggwordcloud`
#' * `colourpicker`
#' * `shinycssloaders`
#' * `shinyjs`
#' * `viridis`
#' * `DT`
#' * `readr`
#' * `forcats`
#' * `stringdist`
#' * `lexopsdata` (http://github.com/JackEdTaylor/lexopsdata)
#'
#' @param ... optional arguments to `shiny::runApp()`
#'
#' @export

run_shiny <- function(...) {
  app_dir <- system.file("shiny-app", package = "LexOPS")
  # check app is installed
  if (app_dir == "") {
    stop("Could not find the directory for the LexOPS Shiny App. Try re-installing `LexOPS`.")
  }
  # check dependencies
  rlang::check_installed(
    c("lexopsdata", "shiny", "shinydashboard", "plotly", "ggwordcloud", "colourpicker", "shinycssloaders", "shinyjs", "viridis", "DT", "readr", "forcats", "stringdist", "dplyr", "tidyr", "tibble", "purrr"),
    reason = "to run the LexOPS shiny app",
    action = function(pkg, ...) {
      # use github repository for lexopsdata
      pkg[pkg=="lexopsdata"] <- "JackEdTaylor/lexopsdata@*release"
      pak::pkg_install(pkg)
    }
  )
  # run app
  shiny::runApp(app_dir, ...)
}
