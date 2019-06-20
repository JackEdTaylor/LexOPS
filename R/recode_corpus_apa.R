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
#' recode_corpus_apa(Zipf.SUBTLEX_UK)
#'
#' recode_corpus_apa("AROU.Glasgow_Norms", standard_eval = TRUE)
#'
#' recode_corpus_apa(AoA.Kuperman, first_cite = FALSE)
#'
#' @export

recode_corpus_apa <- function(var, first_cite = TRUE, default = "", standard_eval = FALSE) {
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
