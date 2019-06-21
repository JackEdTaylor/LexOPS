#' Convert a variable name to its measure
#'
#' Converts the name of a variable from `LexOPS::lexops` (e.g. "fpmw.SUBTLEX_US") into the name of its measure (e.g. "frequency per million words").
#'
#' @param var The variable name (non-standard evaluation)
#' @param first_cite Logical; if `TRUE`, gives full name (e.g. frequency per million words), if `FALSE`, gives abbreviated name (e.g. FPMW)
#' @param default The character string that should be returned if the variable does not have a known cietable source
#' @param standard_eval Logical; bypasses non-standard evaluation. If `TRUE`, `var` should be a column name in quotation marks. If `FALSE`, the quotation marks are not necessary. Default = `FALSE`.
#'
#' @return A citation of a variable's source in APA format. Returns `default` if not recognised as a citeabile source.
#'
#' @examples
#'
#' recode_measure(Zipf.SUBTLEX_UK)
#'
#' recode_measure("AROU.Glasgow_Norms", standard_eval = TRUE)
#'
#' recode_measure(AoA.Kuperman, first_cite = FALSE)
#'
#' @export

recode_measure <- function(var, first_cite = TRUE, default = "", standard_eval = FALSE) {
  if (!standard_eval) var <- substitute(var)
  var_name <- corpus_recode_name_measure(var)
  if (!is.null(var_name) & !is.na(var_name)) {
    if (first_cite) {
      dplyr::recode(
        var_name,
        ".1letter" = "1-letter (ARPABET) representations",
        ".PrN" = "number of pronunciations",
        ".br_IPA" = "International Phonetic Alphabet (IPA) representations of words' British pronunciations",
        "Zipf." = "Frequency in Zipf (Zipf=log10(frequency per million)+3)",
        "fpmw." = "Frequency per Million words (FPMW)",
        "PoS." = "Part of Speech",
        "Length" = "Length (number of characters)",
        "BG." = "bigram probability",
        "ON.OLD20" = "Orthographic Levenshtein Distance 20 (OLD20)",
        "ON.Colthearts_N" = "Coltheart's N",
        "ON.Log_OLD20" = "Log Orthographic Levenshtein Distance 20 (OLD20)",
        "ON.Log_Colthearts_N" = "Log Coltheart's N",
        "Syllables." = "number of syllables",
        "Phonemes." = "number of phonemes",
        "Rhyme." = "rhyme",
        "FAM." = "familiarity ratings",
        "AoA." = "age of acquisition (AoA)",
        "CNC." = "concreteness ratings",
        "IMAG." = "imageability ratings",
        "AROU." = "arousal ratings",
        "VAL." = "valence ratings",
        "DOM." = "dominance ratings",
        "SIZE." = "semantic size ratings",
        "GEND." = "gender ratings",
        "HUM." = "humour ratings",
        "RT." = "lexical decision response time (RT)",
        "Accuracy." = "lexical decision accuracy",
        "PREV." = "word prevalence",
        "PK." = "proportion known",
        .default = default
      )
    } else {
      dplyr::recode(
        var_name,
        ".1letter" = "1-letter (ARPABET) representations",
        ".PrN" = "number of pronunciations",
        ".br_IPA" = "IPA representations of words' British pronunciations",
        "Zipf." = "Frequency in Zipf",
        "fpmw." = "FPMW",
        "PoS." = "Part of Speech",
        "Length" = "Length",
        "BG." = "bigram probability",
        "ON.OLD20" = "OLD20",
        "ON.Colthearts_N" = "Coltheart's N",
        "ON.Log_OLD20" = "Log OLD20",
        "ON.Log_Colthearts_N" = "Log Coltheart's N",
        "Syllables." = "number of syllables",
        "Phonemes." = "number of phonemes",
        "Rhyme." = "rhyme",
        "FAM." = "familiarity ratings",
        "AoA." = "AoA",
        "CNC." = "concreteness ratings",
        "IMAG." = "imageability ratings",
        "AROU." = "arousal ratings",
        "VAL." = "valence ratings",
        "DOM." = "dominance ratings",
        "SIZE." = "semantic size ratings",
        "GEND." = "gender ratings",
        "HUM." = "humour ratings",
        "RT." = "lexical decision RT",
        "Accuracy." = "lexical decision accuracy",
        "PREV." = "word prevalence",
        "PK." = "proportion known",
        .default = default
      )
    }
  } else {
    default
  }
}

corpus_recode_name_measure <- function(var) {
  # where var should be a character vector
  var_names <- c(".1letter", ".PrN", ".br_IPA", "Zipf.", "fpmw.", "PoS.", "Length", "BG.", "ON.OLD20", "ON.Colthearts_N", "ON.Log_OLD20", "ON.Log_Colthearts_N", "Syllables.", "Phonemes.", "PN.PLD20", "PN.Colthearts_N", "PN.Log_PLD20", "PN.Log_Colthearts_N", "Rhyme.", "FAM.", "AoA.", "CNC.", "IMAG.", "AROU.", "VAL.", "DOM.", "SIZE.", "GEND.", "HUM.", "RT.", "Accuracy.", "PREV.", "PK.")
  matches <- sapply(var_names, grepl, var, USE.NAMES = FALSE)
  if (length(matches[matches])==0) return(NA)
  if (length(matches[matches])>1) warning(sprintf("Multiple (%i) sources found. Will return all matches.", length(matches[matches])))
  var_names[matches]
}
