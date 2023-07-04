#' An outdated wrapper for match_item()
#'
#' The `match_word()` function has been replaced by `match_item()`. This wrapper for the function has been kept to support backwards compatibility to code written in older versions of LexOPS. This wrapper may be removed in the future, so please update your code to use `match_item()`.
#'
#' @param df A data frame to reorder, containing the target string (default = LexOPS::lexops).
#' @param target The target string
#' @param ... Should specify the variables and tolerances in the form `Length = 0:0, Zipf.SUBTLEX_UK = -0.1:0.1, PoS.SUBTLEX_UK`. Numeric variables can include tolerances (as elements 2:3 of a vector). Numeric variables with no tolerances will be matched exactly.
#' @param id_col A character vector specifying the column identifying unique observations (e.g. in `LexOPS::lexops`, the `id_col` is `"string"`).
#' @param filter Logical. If TRUE, matches outside the tolerances specified in vars are removed. If FALSE, a new column, matchFilter is calculated indicating whether or not the string is within all variables' tolerances. (Default = TRUE.)
#' @param standard_eval Logical; bypasses non-standard evaluation, and allows more standard R object of list. If `TRUE`, `...` should be a single list specifying the variables to match by and their tolerances, in the form `list("numericVariable1Name", c("numericVariable2Name", -1.5, 3), "characterVariableName")`. Default = `FALSE`.
#'
#' @return Returns data frame based on `df`. If `filter` == TRUE, will only contain matches. If `filter` == FALSE, will be the original `df` object, with a new column, "matchFilter".
#' @examples
#'
#' # Match by number of syllables exactly
#' lexops |>
#'   match_word("thicket", Syllables.CMU)
#'
#' # Match by number of syllables exactly, but keep all entries in the original dataframe
#' lexops |>
#'   match_word("thicket", Syllables.CMU, filter = FALSE)
#'
#' # Match by number of syllables exactly, and rhyme
#' lexops |>
#'   match_word("thicket", Syllables.CMU, Rhyme.CMU)
#'
#' # Match by length exactly, and closely by frequency (within 0.2 Zipf either way)
#' lexops |>
#'   match_word("thicket", Length, Zipf.SUBTLEX_UK = -0.2:0.2)
#'
#' # The syntax makes matching by multiple variables easiy and readable
#' lexops |>
#'   match_word(
#'     "elephant",
#'     BG.SUBTLEX_UK = -0.005:0.005,
#'     Length = 0:0,
#'     Zipf.SUBTLEX_UK = -0.1:0.1,
#'     PoS.SUBTLEX_UK,
#'     RT.ELP = -10:10
#'   )
#'
#' # Match using standard evaluation
#' lexops |>
#'   match_word("thicket", list("Length", c("Zipf.SUBTLEX_UK", -0.2, 0.2)), standard_eval = TRUE)
#'
#' # Find matches within an orthographic levenshtein distance of 5 from "thicket":
#' library(dplyr)
#' library(stringdist)
#' targ_word <- "thicket"
#' lexops |>
#'   mutate(old = stringdist(targ_word, string, method="lv")) |>
#'   match_word(targ_word, old = 0:5)
#'
#' # Find matches within a phonological levenshtein distance of 2 from "thicket":
#' # (note that this method requires 1-letter phonological transcriptions)
#' library(dplyr)
#' library(stringdist)
#' targ_word <- "thicket"
#' targ_word_pronun <- lexops |>
#'   filter(string == "thicket") |>
#'   pull(eSpeak.br_1letter)
#' lexops |>
#'   mutate(pld = stringdist(targ_word_pronun, eSpeak.br_1letter, method="lv")) |>
#'   match_word(targ_word, pld = 0:2)
#'
#' @seealso \code{\link{lexops}} for the default data frame and associated variables.
#'
#' @export

match_word <- function(df = LexOPS::lexops, target, ..., id_col = "string", filter = TRUE, standard_eval = FALSE) {
  warning("`match_word() is now outdated. Please use `match_item()`")
  match_item(df = df, target = target, ...=..., id_col = id_col, filter = filter, standard_eval = standard_eval)
}
