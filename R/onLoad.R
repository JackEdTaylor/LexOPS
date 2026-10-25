# function to return the lexops dataset if installed
.lexops_data <- function() {
  if (rlang::is_interactive()) {
    # if running interactively, prompt to install if missing
    rlang::check_installed(
      "lexopsdata",
      reason = "to use the lexops dataset; https://github.com/JackEdTaylor/lexopsdata",
      action = function(pkg, ...) {
        # use github repository for lexopsdata
        pak::pkg_install("JackEdTaylor/lexopsdata@*release", ask=TRUE)
      }
    )
  } else {
    # if not running interactively, give a warning and return an empty dataframe if missing
    if (!rlang::is_installed("lexopsdata")) {
      checking_package <- identical(
        Sys.getenv("_R_CHECK_PACKAGE_NAME_"),
        "LexOPS"
      )

      if (!checking_package) {
        warning(
          "To use the lexops dataset, please install the lexopsdata package:\npak::pkg_install(\"JackEdTaylor/lexopsdata@*release\")"
        )
      }
      # return empty dataframe if unavailable and not in interactive session
      return(data.frame())
    }
  }

  lexopsdata::lexops
}

.onLoad <- function(libname, pkgname) {
  # add .data as global variable to prevent global variables note for non-standard evaluation in tidyverse
  utils::globalVariables(".data")

  # bind lexops to the .lexops_data() function
  ns <- asNamespace(pkgname)
  makeActiveBinding("lexops", .lexops_data, ns)

  invisible()
}
