corpus_recode_apa <- function(inputopts) {
  if (!is.null(inputopts)) {
    recode(inputopts,
           "bnc_w" = "the British National Corpus (written)",
           "bnc.wbg" = "the British National Corpus (written)",
           "bnc_s" = "the British National Corpus (spoken)",
           "bnc.sbg" = "the British National Corpus (spoken)",
           "suk" = "SUBTLEX-UK",
           "subtlex_uk.bg" = "SUBTLEX-UK",
           "sus" = "SUBTLEX-US",
           "subtlex_us.bg" = "SUBTLEX-US",
           "elp" = "the English Lexicon Project",
           "blp" = "the British Lexicon Project",
           "mp" = "the Moby Project",
           "cmu" = "the CMU Pronouncing Dictionary",
           "gn" = "the Glasgow Norms",
           "cp" = "Clark and Paivio (2004)",
           "kuperman" = "Kuperman et al. (2012)",
           "bb" = "Brysbaert & Biemiller (2017)",
           "brysbaert" = "Brysbaert et al. (2014)",
           "warriner" = "Warriner et al. (2013)",
           "eh" = "Engelthaler & Hills (2018)"
    )
  } else {
    NA
  }
}

corpus_recode <- function(inputopts = c("bnc_w", "bnc_s"), prefix=NA) {
  if (!is.null(inputopts)) {
    prefix_dot <- if(is.na(prefix)) {""} else {sprintf("%s.", prefix)}
    recoded <- recode(inputopts,
                      "bnc_w" = "BNC.Written",
                      "bnc.wbg" = "BNC.Written",
                      "bnc_s" = "BNC.Spoken",
                      "bnc.sbg" = "BNC.Spoken",
                      "suk" = "SUBTLEX_UK",
                      "subtlex_uk.bg" = "SUBTLEX_UK",
                      "sus" = "SUBTLEX_US",
                      "subtlex_us.bg" = "SUBTLEX_US",
                      "elp" = "ELP",
                      "blp" = "BLP",
                      "mp" = "Moby",
                      "cmu" = "CMU.pr1",
                      "gn" = "Glasgow_Norms",
                      "cp" = "Clark_and_Paivio",
                      "kuperman" = "Kuperman",
                      "bb" = "BrysbaertBiemiller",
                      "brysbaert" = "Brysbaert",
                      "warriner" = "Warriner",
                      "eh" = "EngelthalerHills"
    )
    sprintf("%s%s", prefix_dot, recoded)
  } else {
    NA
  }
}

