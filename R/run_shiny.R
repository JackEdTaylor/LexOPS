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
  # check CRAN dependencies
  shiny_deps <- c("shiny", "shinydashboard", "plotly", "ggwordcloud", "colourpicker", "shinycssloaders", "shinyjs", "viridis", "DT", "readr", "forcats", "stringdist", "dplyr", "tidyr", "tibble", "purrr")
  shiny_deps_missing <- lapply(shiny_deps, function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) TRUE else FALSE
  })
  # check github dependencies
  github_deps <- c("lexopsdata" = "http://github.com/JackEdTaylor/lexopsdata")
  github_deps_missing <- lapply(names(github_deps), function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) TRUE else FALSE
  })
  # if missing any dependencies, stop and tell the user how to install
  if (any(unlist(shiny_deps_missing))) {
    deps_vec <- shiny_deps[shiny_deps_missing == TRUE]
    deps_vec_gh <- github_deps[github_deps_missing == TRUE]

    deps_inst_cran <- if (length(deps_vec)) {
      sprintf("install.packages(c(%s))", paste(sprintf("\"%s\"", deps_vec), collapse=", "))
    } else {
      NULL
    }

    deps_inst_gh <- if (length(deps_vec_gh)) {
      sprintf("remotes::install_github(c(%s))", paste(sprintf("\"%s\"", deps_vec_gh), collapse=", "))
    } else {
      NULL
    }

    deps_inst_str <- paste(c(deps_inst_cran, deps_inst_gh), collapse="; ")

    stop(sprintf("You are missing %s packages required by the LexOPS shiny app. Install them with:\n%s", length(deps_vec) + length(deps_vec_gh), deps_inst_str))
  }
  # run app
  shiny::runApp(app_dir, ...)
}
