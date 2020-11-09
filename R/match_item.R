#' Get suitable matches for a single item on one or several dimensions.
#'
#' Suggests items that are suitable matches for a target item, based on selected variables of a data frame. Note that unlike functions in the generate pipeline (e.g. `control_for()`), multiple variables' tolerances can be defined in one function.
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
#' lexops %>%
#'   match_item("thicket", Syllables.CMU)
#'
#' # Match by number of syllables exactly, but keep all entries in the original dataframe
#' lexops %>%
#'   match_item("thicket", Syllables.CMU, filter = FALSE)
#'
#' # Match by number of syllables exactly, and rhyme
#' lexops %>%
#'   match_item("thicket", Syllables.CMU, Rhyme.CMU)
#'
#' # Match by length exactly, and closely by frequency (within 0.2 Zipf either way)
#' lexops %>%
#'   match_item("thicket", Length, Zipf.SUBTLEX_UK = -0.2:0.2)
#'
#' # The syntax makes matching by multiple variables easiy and readable
#' lexops %>%
#'   match_item(
#'     "elephant",
#'     BG.SUBTLEX_UK = -0.005:0.005,
#'     Length = 0:0,
#'     Zipf.SUBTLEX_UK = -0.1:0.1,
#'     PoS.SUBTLEX_UK,
#'     RT.ELP = -10:10
#'   )
#'
#' # Match using standard evaluation
#' lexops %>%
#'   match_item("thicket", list("Length", c("Zipf.SUBTLEX_UK", -0.2, 0.2)), standard_eval = TRUE)
#'
#' # Find matches within an orthographic levenshtein distance of 5 from "thicket":
#' library(dplyr)
#' library(vwr)
#' targ_word <- "thicket"
#' lexops %>%
#'   mutate(old = levenshtein.distance(targ_word, string)) %>%
#'   match_item(targ_word, old = 0:5)
#'
#' # Find matches within a phonological levenshtein distance of 2 from "thicket":
#' # (note that this method requires 1-letter phonological transcriptions)
#' library(dplyr)
#' library(vwr)
#' targ_word <- "thicket"
#' targ_word_pronun <- lexops %>%
#'   filter(string == "thicket") %>%
#'   pull(eSpeak.br_1letter)
#' lexops %>%
#'   mutate(pld = levenshtein.distance(targ_word_pronun, eSpeak.br_1letter)) %>%
#'   match_item(targ_word, pld = 0:2)
#'
#' @seealso \code{\link{lexops}} for the default data frame and associated variables.
#'
#' @export

match_item <- function(df = LexOPS::lexops, target, ..., id_col = "string", filter = TRUE, standard_eval = FALSE) {
  if (standard_eval) {
    vars <- (...)
  } else {
    vars <- substitute(c(...)) %>%
      parse_ellipsis()
  }
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check this dataframe doesn't include a column called euclidean_distance; if it does, remove it and throw a warning
  if ("euclidean_distance" %in% colnames(df)) {
    warning('"euclidean_distance" column will be ignored, as this is overwritten by `match_item()`')
    df$euclidean_distance <- NULL
  }
  # check id_col is a string
  if (!is.character(id_col)) stop(sprintf("Expected id_col to be of class string, not %s", class(id_col)))
  # check target is a string
  if (!is.character(target)) stop(sprintf("Expected target to be of class string, not %s", class(target)))
  # check all variables in vars are in the dataframe
  varsPresent <- sapply(vars, function(listobj) listobj[1] %in% colnames(df))
  if (!all(varsPresent)) {
    stop(
      sprintf(
        "Missing %i variables in df:\n\t%s",
        length(vars[!varsPresent]),
        paste(sapply(vars[!varsPresent], function(listobj) listobj[1]), collapse="\n\t")
      )
    )
  }
  # check numeric and non-numeric variables are correctly specified
  varsIncorrectlySpecified <- sapply(vars, function(listObj) {
    (!is.numeric(df[[listObj[1]]]) & length(listObj)==3) | !(length(listObj) %in% c(1, 3))
  })
  if (any(varsIncorrectlySpecified)) {
    stop(
      sprintf(
        "%i variables misspecified:\n\t%s",
        length(vars[varsIncorrectlySpecified]),
        paste(sapply(vars[varsIncorrectlySpecified], function(listObj) {
          recodedError <- if (!length(listObj) %in% c(1, 3)) {
            sprintf("expected list object to be of length 1 (no tolerances) or 3 (with tolerances), not %i", length(listObj))
          } else if (!is.numeric(df[[listObj[[1]][1]]])) {
            "did not expect tolerances for non-numeric variable"
          }
          sprintf("%s - %s", listObj[1], recodedError)
        }), collapse="\n\t")
      )
    )
  }
  # check id_col is a column in df
  if (!id_col %in% colnames(df)) stop(sprintf("'%s' column not found in df", id_col))
  # check target word in id_col
  if (!target %in% df[[id_col]]) stop(sprintf("'%s' not found in '%s' column of df", target, id_col))

  # get the euclidean distance, and add as a new column, 2nd after the id_col column; all NA if no numeric variables
  vars_sans_tols <- sapply(vars, dplyr::first, USE.NAMES = FALSE)
  numeric_vars <- vars_sans_tols[sapply(df[, vars_sans_tols], is.numeric)]
  df <- if (length(numeric_vars)>0) {
    dplyr::mutate(df, euclidean_distance = LexOPS::euc_dists(df, target = target, vars = numeric_vars, id_col = id_col, standard_eval = TRUE))
  } else {
    dplyr::mutate(df, euclidean_distance = NA)
  }

  df <- df %>%
    dplyr::arrange(euclidean_distance) %>%
    dplyr::select(!!(dplyr::sym(id_col)), euclidean_distance, dplyr::everything())

  # get the numeric and character tolerances relative to the target word
  numFilt <- lapply(vars, function(listObj) {
    if (is.numeric(df[[listObj[1]]])) {
      out <- listObj
      match_string_val <- dplyr::filter(df, !!dplyr::sym(id_col) == target) %>%
        dplyr::pull(!!dplyr::sym(listObj[[1]]))
      if (length(listObj) == 3) {
        out[2:3] <- as.numeric(out[2:3]) + match_string_val
      } else if (length(listObj) == 1) {
        out[2:3] <- match_string_val
      }
      return(out)
    }
  })
  numFilt[sapply(numFilt, is.null)] <- NULL  # remove NULL values (probably character filters)

  charFilt <- lapply(vars, function(listObj) {
    if (!is.numeric(df[[listObj[1]]])) {
      out <- listObj
      out[2] <- dplyr::filter(df, !!dplyr::sym(id_col) == target) %>%
        dplyr::pull(!!dplyr::sym(listObj[[1]])) %>%
        as.character()
      return(out)
    }
  })
  charFilt[sapply(charFilt, is.null)] <- NULL  # remove NULL values (probably numeric filters)

  # filter out words that don't fit the filters
  if (length(numFilt) > 0) {

    numFilt_string <- numFilt %>%
      lapply(function(filt) sprintf("dplyr::between(%s, %f, %f)", filt[1], as.numeric(filt[2]), as.numeric(filt[3]))) %>%
      paste0(collapse = " & ")

    numOut <- dplyr::filter(df, !!rlang::parse_expr(numFilt_string))
  }
  if (length(charFilt) > 0) {

    charFilt_string <- charFilt %>%
      lapply(function(filt) sprintf("%s == \"%s\"", as.character(filt[1]), as.character(filt[2]))) %>%
      paste0(collapse = " & ")

    charOut <- dplyr::filter(df, !!rlang::parse_expr(charFilt_string))
  }

  # return the result
  if (length(numFilt) > 0 & length(charFilt) > 0) {
    out <- dplyr::inner_join(charOut, numOut, by = colnames(charOut))
  } else if (length(numFilt) > 0 & length(charFilt) == 0) {
    out <- numOut
  } else {
    out <- charOut
  }

  # if the filter argument is FALSE, return the original df, but with new column matchFilter
  if (!filter) {
    out <- dplyr::mutate(df, matchFilter = !!(dplyr::sym(id_col)) %in% out[[id_col]]) %>%
      dplyr::select(!!dplyr::sym(id_col), matchFilter, dplyr::everything())
  }

  # remove the target word
  out <- dplyr::filter(out, !!(dplyr::sym(id_col)) != target)

  # return the result
  out
}

# these should throw each of the possible errors

# LexOPS::lexops %>% match_item("thicket", list(c("Length", -1, 1), "Rhyme", c("unreal", 1)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", -1, 1), c("Rhyme.CMU", 1, 2), "Zipf.SUBTLEX_UK"))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5)))
#
# LexOPS::lexops %>% match_item("111", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5, 1.5)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5, 1.5)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5, 1.5)), df="hi")
#
# LexOPS::lexops %>% match_item(2, list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5, 1.5)), id_col = "string")
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -1.5, 1.5)), id_col = 3*1)
#
# # these should work
#
# LexOPS::lexops %>% match_item("thicket", list(c("Zipf.SUBTLEX_UK", -0.2, 0.2)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -0.2, 0.2)))
#
# LexOPS::lexops %>% match_item("thicket", list(c("Length", 0, 0), c("Zipf.SUBTLEX_UK", -0.2, 0.2), "Rhyme.eSpeak.br"))
