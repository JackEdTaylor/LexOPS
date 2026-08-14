# function to return the lexops dataset if installed
.lexops_data <- function() {
  if (rlang::is_interactive()) {
    # if running interactively, prompt to install if missing
    rlang::check_installed(
      "lexopsdata",
      reason = "to use the lexops dataset",
      action = function(pkg, ...) {
        # use github repository for lexopsdata
        pkg[pkg=="lexopsdata"] <- "JackEdTaylor/lexopsdata@*release"
        pak::pkg_install(pkg)
      }
    )
  } else {
    # if not running interactively, give a warning and return an empty dataframe if missing
    if (!rlang::is_installed("lexopsdata")) {
      warning(
        "To use the lexops dataset, please install the lexopsdata package:\npak::pkg_install(\"JackEdTaylor/lexopsdata@*release\")"
      )
      # return empty dataframe if unavailable and not in interactive session
      return(data.frame())
    }
  }

  lexopsdata::lexops
}

.onLoad <- function(libname, pkgname) {

  # prevent note about global variables in devtools::check()
  utils::globalVariables("x")
  utils::globalVariables("n")
  utils::globalVariables("alpha_level")
  utils::globalVariables("qm")
  utils::globalVariables("wordcloudsize")
  utils::globalVariables("wordcloudalpha")
  utils::globalVariables("string")
  utils::globalVariables("PK.Brysbaert")
  utils::globalVariables("item_nr")
  utils::globalVariables("condition")
  utils::globalVariables("value")
  utils::globalVariables("iteration")
  utils::globalVariables("stim_generated")
  utils::globalVariables("is_stim")

  # instead of lazyload, bind `lexops` to the .lexops_data() function
  ns <- asNamespace(pkgname)
  if (!exists("lexops", envir = ns, inherits = FALSE) || !bindingIsActive("lexops", ns)) {
    if (exists("lexops", envir = ns, inherits = FALSE) && !bindingIsActive("lexops", ns)) {
      unlockBinding("lexops", ns)
    }
    makeActiveBinding("lexops", .lexops_data, ns)
  }

  invisible()
}
