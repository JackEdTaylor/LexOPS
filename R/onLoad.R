# function to load the lexops dataset if installed, or else give an error with installation instructions
.lexops_data <- function() {
  if (!requireNamespace("lexopsdata", quietly = TRUE)) {
    warning(
      "To use the lexops dataset, please install the lexopsdata package:\npak::pkg_install(\"JackEdTaylor/lexopsdata\")"
    )
    # return empty dataframe if unavailable
    return(data.frame())
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
