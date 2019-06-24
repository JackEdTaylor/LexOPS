#' Convert a variable name to its measure
#'
#' Converts the name of a variable from `LexOPS::lexops` (e.g. "fpmw.SUBTLEX_US") into the name of its measure (e.g. "frequency per million words").
#'
#' @param var The variable name (non-standard evaluation)
#' @param first_cite Logical; if `TRUE`, gives full name (e.g. frequency per million words), if `FALSE`, gives abbreviated name (e.g. FPMW)
#' @param default The character string that should be returned if the variable does not have a known cietable source
#' @param title_caps Logical; use title capitalisation (e.g. "Familiarity Ratings" rather than "famliarity ratings"). Default is `FALSE`.
#' @param standard_eval Logical; bypasses non-standard evaluation. If `TRUE`, `var` should be a column name in quotation marks. If `FALSE`, the quotation marks are not necessary. Default = `FALSE`.
#'
#' @return A citation of a variable's source in APA format. Returns `default` if not recognised as a citeabile source.
#'
#' @examples
#'
#' var_to_measure(Zipf.SUBTLEX_UK)
#'
#' var_to_measure("AROU.Glasgow_Norms", standard_eval = TRUE)
#'
#' var_to_measure(AoA.Kuperman, first_cite = FALSE)
#'
#' @export

var_to_measure <- function(var, first_cite = TRUE, default = "", title_caps = FALSE, standard_eval = FALSE) {
  if (!standard_eval) var <- substitute(var)
  var_name <- corpus_recode_name_measure(var)
  if (!is.null(var_name) & !is.na(var_name)) {
    if (first_cite) {
      out <- dplyr::recode(
        var_name,
        ".1letter" = "1-letter (ARPABET) Representations",
        ".PrN" = "Number of Pronunciations",
        ".br_IPA" = "International Phonetic Alphabet (IPA) Representations of Words' British Pronunciations",
        "Zipf." = "Frequency in Zipf (Zipf=log10(frequency per million)+3)",
        "fpmw." = "Frequency per Million words",
        "PoS." = "Part of Speech",
        "Length" = "Length (Number of Characters)",
        "BG." = "Bigram Probability",
        "ON.OLD20" = "Orthographic Levenshtein Distance 20 (OLD20)",
        "ON.Colthearts_N" = "Coltheart's N",
        "ON.Log_OLD20" = "Log Orthographic Levenshtein Distance 20 (OLD20)",
        "ON.Log_Colthearts_N" = "Log Coltheart's N",
        "Syllables." = "Number of Syllables",
        "Phonemes." = "Number of Phonemes",
        "Rhyme." = "Rhyme",
        "FAM." = "Familiarity Ratings",
        "AoA." = "Age of Acquisition",
        "CNC." = "Concreteness Ratings",
        "IMAG." = "Imageability Ratings",
        "AROU." = "Arousal Ratings",
        "VAL." = "Valence Ratings",
        "DOM." = "Dominance Ratings",
        "SIZE." = "Semantic Size Ratings",
        "GEND." = "Gender Ratings",
        "HUM." = "Humour Ratings",
        "RT." = "Lexical Decision Response Time (RT)",
        "Accuracy." = "Lexical Decision Accuracy",
        "PREV." = "Word Prevalence",
        "PK." = "Proportion Known",
        .default = default
      )
    } else {
      out <- dplyr::recode(
        var_name,
        ".1letter" = "1-letter (ARPABET) Representations",
        ".PrN" = "Number of Pronunciations",
        ".br_IPA" = "IPA Representations of Words' British Pronunciations",
        "Zipf." = "Frequency in Zipf",
        "fpmw." = "Frequency per Million words",
        "PoS." = "Part of Speech",
        "Length" = "Length",
        "BG." = "Bigram Probability",
        "ON.OLD20" = "OLD20",
        "ON.Colthearts_N" = "Coltheart's N",
        "ON.Log_OLD20" = "Log OLD20",
        "ON.Log_Colthearts_N" = "Log Coltheart's N",
        "Syllables." = "Number of Syllables",
        "Phonemes." = "Number of Phonemes",
        "Rhyme." = "Rhyme",
        "FAM." = "Familiarity Ratings",
        "AoA." = "Age of Acquisition",
        "CNC." = "Concreteness Ratings",
        "IMAG." = "Imageability Ratings",
        "AROU." = "Arousal Ratings",
        "VAL." = "Valence Ratings",
        "DOM." = "Dominance Ratings",
        "SIZE." = "Semantic Size Ratings",
        "GEND." = "Gender Ratings",
        "HUM." = "Humour Ratings",
        "RT." = "Lexical Decision RT",
        "Accuracy." = "Lexical Decision Accuracy",
        "PREV." = "Word Prevalence",
        "PK." = "Proportion Known",
        .default = default
      )
    }
  } else {
    return(default)
  }
  if (!title_caps) {
    # regex to match sections of strings which should be converted to lower
    lower_targs_regex <- "\\b(?!RT|OLD20|ARPABET|FPMW|Zipf|Coltheart|N$|IPA)\\S+"
    # get the replacement lower case strings
    rep_string <- regmatches(out, gregexpr(lower_targs_regex, out, perl = TRUE)) %>%
      lapply(tolower)
    # replace words that should be lower case with lower case versions
    regmatches(out, gregexpr(lower_targs_regex, out, perl = TRUE)) <- rep_string
  }
  out
}

corpus_recode_name_measure <- function(var) {
  # where var should be a character vector
  var_names <- c(".1letter", ".PrN", ".br_IPA", "Zipf.", "fpmw.", "PoS.", "Length", "BG.", "ON.OLD20", "ON.Colthearts_N", "ON.Log_OLD20", "ON.Log_Colthearts_N", "Syllables.", "Phonemes.", "PN.PLD20", "PN.Colthearts_N", "PN.Log_PLD20", "PN.Log_Colthearts_N", "Rhyme.", "FAM.", "AoA.", "CNC.", "IMAG.", "AROU.", "VAL.", "DOM.", "SIZE.", "GEND.", "HUM.", "RT.", "Accuracy.", "PREV.", "PK.")
  matches <- sapply(var_names, grepl, var, USE.NAMES = FALSE)
  if (length(matches[matches])==0) return(NA)
  if (length(matches[matches])>1) warning(sprintf("Multiple (%i) sources found. Will return all matches.", length(matches[matches])))
  var_names[matches]
}

#' Convert a variable name to APA citation
#'
#' Converts the name of a variable from `LexOPS::lexops` (e.g. "Zipf.SUBTLEX_US") into an APA-style citation (e.g. "SUBTLEX-US (Brysbaert & New, 2009)").
#'
#' @param var The variable name (non-standard evaluation)
#' @param first_cite Logical; if `TRUE`, gives full citation, if `FALSE`, gives abbreviated (i.e. "et al.") citation
#' @param default The character string that should be returned if the variable does not have a known cietable source
#' @param standard_eval Logical; bypasses non-standard evaluation. If `TRUE`, `var` should be a column name in quotation marks. If `FALSE`, the quotation marks are not necessary. Default = `FALSE`.
#'
#' @return A citation of a variable's source in APA format. Returns `default` if not recognised as a citeabile source.
#'
#' @examples
#'
#' var_to_source(Zipf.SUBTLEX_UK)
#'
#' var_to_source("AROU.Glasgow_Norms", standard_eval = TRUE)
#'
#' var_to_source(AoA.Kuperman, first_cite = FALSE)
#'
#' @export

var_to_source <- function(var, first_cite = TRUE, default = "", standard_eval = FALSE) {
  if (!standard_eval) var <- substitute(var)
  var_name <- corpus_recode_name_source(var)
  if (!is.null(var_name) & !is.na(var_name)) {
    if (first_cite) {
      dplyr::recode(
        var_name,
        "BNC.Written" = 'written sources of the British National Corpus (BNC; "The British National Corpus, version 3 (BNC XML Edition)," 2007)',
        "BNC.Spoken" = 'spoken sources of the British National Corpus (BNC; "The British National Corpus, version 3 (BNC XML Edition)," 2007)',
        "SUBTLEX_UK" = "SUBTLEX-UK (van Heuven, Mandera, Keuleers, & Brysbaert, 2014)",
        "SUBTLEX_US" = "SUBTLEX-US (Brysbaert & New, 2009)",
        "CMU" = "the CMU Pronouncing Dictionary (Weide, 2014)",
        "Glasgow_Norms" = "the Glasgow Norms (Scott, Keitel, Becirspahic, Yao, & Sereno, 2018)",
        "Clark_and_Paivio" = "Clark and Paivio (2004)",
        "AoA.Kuperman" = "Kuperman, Stadthagen-Gonzalez and Brysbaert (2012)",
        "AoA.BrysbaertBiemiller" = "Brysbaert and Biemiller's (2017)",
        "CNC.Brysbaert" = "Brysbaert, Warriner and Kuperman (2014)",
        "Warriner" = "Warriner, Kuperman and Brysbaert (2013)",
        "EngelthalerHills" = "Engelthaler and Hills (2018)",
        "PREV.Brysbaert" = "Brysbaert, Mandera, and Keuleers (2018)",
        "PK.Brysbaert" = "Brysbaert, Mandera, and Keuleers (2018)",
        "ELP" = "the English Lexicon Project (ELP; Balota et al., 2007)",
        "BLP" = "the British Lexicon Project (BLP; Keuleers, Lacey, Rastle, & Brysbaert, 2012)",
        .default = default
      )
    } else {
      dplyr::recode(
        var_name,
        "BNC.Written" = "written sources of the BNC",
        "BNC.Spoken" = "spoken sources of the BNC",
        "SUBTLEX_UK" = "SUBTLEX-UK (van Heuven et al., 2014)",
        "SUBTLEX_US" = "SUBTLEX-US (Brysbaert & New, 2009)",
        "CMU" = "the CMU Pronouncing Dictionary (Weide, 2014)",
        "Glasgow_Norms" = "the Glasgow Norms",
        "Clark_and_Paivio" = "Clark and Paivio (2004)",
        "AoA.Kuperman" = "Kuperman et al. (2012)",
        "AoA.BrysbaertBiemiller" = "Brysbaert and Biemiller (2017)",
        "CNC.Brysbaert" = "Brysbaert et al. (2014)",
        "Warriner" = "Warriner et al. (2013)",
        "EngelthalerHills" = "Engelthaler and Hills (2018)",
        "PREV.Brysbaert" = "Brysbaert et al. (2018)",
        "PK.Brysbaert" = "Brysbaert et al. (2018)",
        "ELP" = "the ELP",
        "BLP" = "the BLP",
        .default = default
      )
    }
  } else {
    default
  }
}

corpus_recode_name_source <- function(var) {
  # where var should be a character vector
  var_names <- c("CMU", "eSpeak", "SUBTLEX_UK", "SUBTLEX_US", "BNC.Written", "BNC.Spoken", "Glasgow_Norms", "Clark_and_Paivio", "AoA.Kuperman", "AoA.BrysbaertBiemiller", "CNC.Brysbaert", "Warriner", "EngelthalerHills", "PREV.Brysbaert", "PK.Brysbaert", "ELP", "BLP", "Length")
  matches <- sapply(var_names, grepl, var, USE.NAMES = FALSE)
  if (length(matches[matches])==0) return(NA)
  if (length(matches[matches])>1) warning(sprintf("Multiple (%i) sources found. Will return all matches.", length(matches[matches])))
  var_names[matches]
}
