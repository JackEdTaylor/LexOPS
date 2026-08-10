# function to load the lexops dataset if installed, or else give an error with installation instructions
.lexops_data <- function() {
  if (!requireNamespace("lexopsdata", quietly = TRUE)) {
    stop(
      "To use the lexops dataset, please install the lexopsdata package: remotes::install_github(\"JackEdTaylor/lexopsdata\")",
      call. = FALSE
    )
  }

  lexopsdata::lexops
}

.onLoad <- function(libname, pkgname) {
  # prevent note about global variables in devtools::check()

  utils::globalVariables(".")

  utils::globalVariables("x")
  utils::globalVariables("n")
  utils::globalVariables("alpha_level")
  utils::globalVariables("qm")
  utils::globalVariables("wordcloudsize")
  utils::globalVariables("wordcloudalpha")
  utils::globalVariables("x")
  utils::globalVariables("string")
  utils::globalVariables("PK.Brysbaert")
  utils::globalVariables("LexOPS_cond")
  utils::globalVariables("item_nr")
  utils::globalVariables("condition")
  utils::globalVariables("match_null")
  utils::globalVariables("value")
  utils::globalVariables("iteration")
  utils::globalVariables("was_successful")
  utils::globalVariables("stim_generated")
  utils::globalVariables("tmp")
  utils::globalVariables("euclidean_distance")
  utils::globalVariables("var")
  utils::globalVariables("matchFilter")
  utils::globalVariables("is_stim")
  utils::globalVariables("control_for_euc_val")

# instead of lazyload, bind the `lexops` to the .lexops_data() function
  ns <- asNamespace(pkgname)
  if (!exists("lexops", envir = ns, inherits = FALSE) || !bindingIsActive("lexops", ns)) {
    if (exists("lexops", envir = ns, inherits = FALSE) && !bindingIsActive("lexops", ns)) {
      unlockBinding("lexops", ns)
    }
    makeActiveBinding("lexops", .lexops_data, ns)
  }

  invisible()
}
