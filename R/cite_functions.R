#' Generate a citation
#'
#' Takes the output from `generate()` or `long_format()`, and generates a tibble that lists known references that should be cited.
#'
#' @param df Output from `generate()` or `long_format()`
#' @param include A string indicating which variables to include in the citation. This can be those specified by `split_by()` and `control_for()` (`"design"`), only those specified in `split_by()` (`"splits"`), or only those specified by `control_for()` (`"controls"`). Alternatively, this can be a character vector of the variables that should be cited, that were in the original dataframe. Default is `"design"`.
#'
#' @return A tibble that lists measures and sources of data that should be cited (if known).
#'
#' @examples
#'
#' stim <- lexops %>%
#'   dplyr::filter(PK.Brysbaert >= .75) %>%
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   control_for(Length) %>%
#'   generate(n = 50, match_null = "balanced")
#' cite_design(stim)
#'
#' @export

cite_design <- function(df, include = "design") {
  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")
  # check is generated stimuli
  if (is.null(LexOPS_attrs$generated)) stop("Must run `generate()` on `df` before using `plot_design()`")
  # ensure is in long format
  if (is.null(LexOPS_attrs$is.long_format)) df <- LexOPS::long_format(df)

  # Tell the user to also cite LexOPS
  cat(sprintf("Please also cite LexOPS: Taylor, Beith and Sereno (2019), doi:10.31234/osf.io/7sudw\n"))

  # get vector of splits (IVs)
  splits <- sapply(LexOPS_attrs$splits, dplyr::first)

  # remove random splits
  if (!is.null(LexOPS_attrs$random_splits)) {
    splits <- splits[-LexOPS_attrs$random_splits]
  }

  # get vector of control variables
  controls <- c( sapply(LexOPS_attrs$controls, dplyr::first), sapply(LexOPS_attrs$control_functions, dplyr::first) )

  # factor vector of variables to plot
  cite_vars <- if (all(include == "design")) {
    c(splits, controls)
  } else if (all(include == "splits")) {
    splits
  } else if (all(include == "controls")) {
    controls
  } else {
    include
  }

  # flatten to vector
  cite_vars <- unlist(cite_vars)

  citation_table(cite_vars)

}

#' Generate a citation
#'
#' Get the measure name, source, and url for any vector of column names from `LexOPS::lexops`.
#'
#' @param cite_vars A character vector of column names
#'
#' @return A tibble that lists measures and sources of data that should be cited (if known).
#'
#' @examples
#'
#' citation_table(c("PK.Brysbaert", "HUM.Engelthaler"))
#'
#' @export

citation_table <- function(cite_vars) {
  # remove string from cite_vars if present
  cite_vars <- cite_vars[cite_vars != "string"]
  # create tibble containing citation info
  dplyr::tibble(
    var = cite_vars,
    measure = sapply(cite_vars, LexOPS::var_to_measure, first_cite = TRUE, title_caps = TRUE, default = "Custom Measure", standard_eval = TRUE),
    source = sapply(cite_vars, LexOPS::var_to_source, first_cite = TRUE, default = "Custom Source", standard_eval = TRUE),
    url = sapply(cite_vars, LexOPS::var_to_url, default = "Unknown", standard_eval = TRUE)
  ) %>%
    dplyr::mutate(
      source = ifelse(var=="Length", NA, source),
      url = ifelse(var=="Length", NA, url)
    )
}

#' Convert a variable name to its measure
#'
#' Converts the name of a variable from `LexOPS::lexops` (e.g. "fpmw.SUBTLEX_US") into the name of its measure (e.g. "frequency per million words").
#'
#' @param var The variable name (non-standard evaluation)
#' @param first_cite Logical; if `TRUE`, gives full name (e.g. frequency per million words), if `FALSE`, gives abbreviated name (e.g. FPMW)
#' @param default The character string that should be returned if the variable does not have a known cietable source
#' @param title_caps Logical; use title capitalisation (e.g. "Familiarity Ratings" rather than "famliarity ratings"). Default is `FALSE`.
#' @param include_pronunciations Logical; allow pronunciation transpositions as measures (e.g. CMU.1letter). Default is `TRUE`.
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

var_to_measure <- function(var, first_cite = TRUE, default = "", title_caps = FALSE, include_pronunciations = TRUE, standard_eval = FALSE) {
  if (!standard_eval) var <- substitute(var)
  var_name <- var_to_measure_name(var, include_pronunciations)
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
        "BG." = "Character Bigram Probability",
        "ON.OLD20" = "Orthographic Levenshtein Distance 20 (OLD20)",
        "ON.Colthearts_N" = "Orthographic Coltheart's N",
        "ON.Log_OLD20" = "Log Orthographic Levenshtein Distance 20 (Log OLD20)",
        "ON.Log_Colthearts_N" = "Log Orthographic Coltheart's N",
        "Syllables." = "Number of Syllables",
        "Phonemes." = "Number of Phonemes",
        "PN.PLD20" = "Phonological Levenshtein Distance 20 (PLD20)",
        "PN.Colthearts_N" = "Phonological Coltheart's N",
        "PN.Log_PLD20" = "Log Phonological Levenshtein Distance 20 (Log PLD20)",
        "PN.Log_Colthearts_N" = "Log Phonological Coltheart's N",
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
        "BG." = "Character Bigram Probability",
        "ON.OLD20" = "OLD20",
        "ON.Colthearts_N" = "Orthographic Coltheart's N",
        "ON.Log_OLD20" = "Log OLD20",
        "ON.Log_Colthearts_N" = "Log Orthographic Coltheart's N",
        "Syllables." = "Number of Syllables",
        "Phonemes." = "Number of Phonemes",
        "PN.PLD20" = "PLD20",
        "PN.Colthearts_N" = "Phonological Coltheart's N",
        "PN.Log_PLD20" = "Log PLD20",
        "PN.Log_Colthearts_N" = "Log Phonological Coltheart's N",
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

var_to_measure_name <- function(var, include_pronunciations=TRUE) {
  # where var should be a character vector
  var_names <- c(".1letter", ".PrN", ".br_IPA", "Zipf.", "fpmw.", "PoS.", "Length", "BG.", "ON.OLD20", "ON.Colthearts_N", "ON.Log_OLD20", "ON.Log_Colthearts_N", "Syllables.", "Phonemes.", "PN.PLD20", "PN.Colthearts_N", "PN.Log_PLD20", "PN.Log_Colthearts_N", "Rhyme.", "FAM.", "AoA.", "CNC.", "IMAG.", "AROU.", "VAL.", "DOM.", "SIZE.", "GEND.", "HUM.", "RT.", "Accuracy.", "PREV.", "PK.")
  if (!include_pronunciations) var_names <- var_names[!var_names %in% c(".1letter", ".br_IPA")]
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
#' @param default The character string that should be returned if the variable does not have a known citeable source
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
        "BNC.All" = 'all sources of the British National Corpus (BNC; "The British National Corpus, version 3 (BNC XML Edition)," 2007)',
        "BNC.Written" = 'written sources of the British National Corpus (BNC; "The British National Corpus, version 3 (BNC XML Edition)," 2007)',
        "BNC.Spoken" = 'spoken sources of the British National Corpus (BNC; "The British National Corpus, version 3 (BNC XML Edition)," 2007)',
        "SUBTLEX_UK" = "SUBTLEX-UK (van Heuven, Mandera, Keuleers, & Brysbaert, 2014)",
        "SUBTLEX_US" = "SUBTLEX-US (Brysbaert & New, 2009)",
        "CMU" = "the CMU Pronouncing Dictionary (Weide, 2014)",
        "eSpeak" = "eSpeak version 1.48.15. (2015)",
        "Glasgow_Norms" = "the Glasgow Norms (Scott, Keitel, Becirspahic, Yao, & Sereno, 2018)",
        "Clark_and_Paivio" = "Clark and Paivio (2004)",
        "AoA.Kuperman" = "Kuperman, Stadthagen-Gonzalez and Brysbaert (2012)",
        "AoA.BrysbaertBiemiller" = "Brysbaert and Biemiller (2017)",
        "CNC.Brysbaert" = "Brysbaert, Warriner and Kuperman (2014)",
        "Warriner" = "Warriner, Kuperman and Brysbaert (2013)",
        "EngelthalerHills" = "Engelthaler and Hills (2018)",
        "PREV.Brysbaert" = "Brysbaert, Mandera, McCormick, and Keuleers (2019)",
        "PK.Brysbaert" = "Brysbaert, Mandera, McCormick, and Keuleers (2019)",
        "ELP" = "the English Lexicon Project (ELP; Balota et al., 2007)",
        "BLP" = "the British Lexicon Project (BLP; Keuleers, Lacey, Rastle, & Brysbaert, 2012)",
        .default = default
      )
    } else {
      dplyr::recode(
        var_name,
        "BNC.All" = "all BNC texts",
        "BNC.Written" = "written sources of the BNC",
        "BNC.Spoken" = "spoken sources of the BNC",
        "SUBTLEX_UK" = "SUBTLEX-UK (van Heuven et al., 2014)",
        "SUBTLEX_US" = "SUBTLEX-US (Brysbaert & New, 2009)",
        "CMU" = "the CMU Pronouncing Dictionary (Weide, 2014)",
        "eSpeak" = "eSpeak (2015)",
        "Glasgow_Norms" = "the Glasgow Norms (Scott et al., 2018)",
        "Clark_and_Paivio" = "Clark and Paivio (2004)",
        "AoA.Kuperman" = "Kuperman et al. (2012)",
        "AoA.BrysbaertBiemiller" = "Brysbaert and Biemiller (2017)",
        "CNC.Brysbaert" = "Brysbaert et al. (2014)",
        "Warriner" = "Warriner et al. (2013)",
        "EngelthalerHills" = "Engelthaler and Hills (2018)",
        "PREV.Brysbaert" = "Brysbaert et al. (2018)",
        "PK.Brysbaert" = "Brysbaert et al. (2018)",
        "ELP" = "the ELP (Balota et al., 2007)",
        "BLP" = "the BLP (Keuleers et al., 2012)",
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

#' Convert a variable name to a url reference.
#'
#' Converts the name of a variable from `LexOPS::lexops` (e.g. "Zipf.SUBTLEX_US") into a url (usually doi).
#'
#' @param var The variable name (non-standard evaluation)
#' @param default The character string that should be returned if the variable does not have a known citeable source
#' @param standard_eval Logical; bypasses non-standard evaluation. If `TRUE`, `var` should be a column name in quotation marks. If `FALSE`, the quotation marks are not necessary. Default = `FALSE`.
#'
#' @return A citation of a variable's source in APA format. Returns `default` if not recognised as a citeabile source.
#'
#' @examples
#'
#' var_to_url(Zipf.SUBTLEX_UK)
#'
#' var_to_url("AROU.Glasgow_Norms", standard_eval = TRUE)
#'
#' var_to_url(AoA.Kuperman)
#'
#' @export

var_to_url <- function(var, default = "", standard_eval = FALSE) {
  if (!standard_eval) var <- substitute(var)
  var_name <- corpus_recode_name_source(var)
  if (!is.null(var_name) & !is.na(var_name)) {
    dplyr::recode(
      var_name,
      "BNC.Written" = 'http://www.natcorp.ox.ac.uk/',
      "BNC.Spoken" = 'http://www.natcorp.ox.ac.uk/',
      "SUBTLEX_UK" = "https://doi.org/10.1080/17470218.2013.850521",
      "SUBTLEX_US" = "https://doi.org/10.3758/BRM.41.4.977",
      "CMU" = "http://www.speech.cs.cmu.edu/cgi-bin/cmudict",
      "eSpeak" = "http://espeak.sourceforge.net/",
      "Glasgow_Norms" = "https://doi.org/10.3758/s13428-018-1099-3",
      "Clark_and_Paivio" = "https://doi.org/10.3758/BF03195584",
      "AoA.Kuperman" = "https://doi.org/10.3758/s13428-012-0210-4",
      "AoA.BrysbaertBiemiller" = "https://doi.org/10.3758/s13428-016-0811-4",
      "CNC.Brysbaert" = "https://doi.org/10.3758/s13428-013-0403-5",
      "Warriner" = "https://doi.org/10.3758/s13428-012-0314-x",
      "EngelthalerHills" = "https://doi.org/10.3758/s13428-017-0930-6",
      "PREV.Brysbaert" = "https://doi.org/10.3758/s13428-018-1077-9",
      "PK.Brysbaert" = "https://doi.org/10.3758/s13428-018-1077-9",
      "ELP" = "https://doi.org/10.3758/BF03193014",
      "BLP" = "https://doi.org/10.3758/s13428-011-0118-4",
      .default = default
    )
  } else {
    default
  }
}
