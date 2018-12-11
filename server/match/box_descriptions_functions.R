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

box_descr_numeric <- function(variablename="Familiarity", variabletype="ratings", opts, slider, selected, avg_zscored=T, exact_recommended=F, abbreviated_variabletype=NA) {
  variabletype <- if (is.na(variabletype)) {"values"} else {variabletype}
  # what is being matched by
  if (!selected | length(opts)==0) {
    t <- sprintf("Will not match by %s.", variablename)
  } else if (length(opts)==1) {
    t <- sprintf("Will match by %s %s, according to %s.", variablename, variabletype, corpus_recode_apa(opts))
  } else {
    zscored <- if (avg_zscored) {" z-scored"} else {""}
    t <- sprintf("Will match by mean%s %s %s, averaged across %i sources.", zscored, variablename, variabletype, length(opts))
  }
  # tolerance info
  if (selected & length(opts)!=0) {
    abbreviated_variabletype <- if (is.na(abbreviated_variabletype)) {variabletype} else {abbreviated_variabletype}
    # e.g. Might want to refer "to Response Times" as "RTs" the second time they are mentioned. In this case, set abbreviated_variabletype="RTs"
    if(slider[1]==0 & slider[2]==0){
      notrecommended <- if (exact_recommended) {""} else {" (not recommended)"}
      t <- sprintf('%s Will match by %s exactly%s.', t, variabletype, notrecommended)
    } else if(slider[1]==0 & slider[2]!=0){
      t <- sprintf('%s Will allow %s %.2f greater than the target word.', t, variabletype, slider[2])
    } else if(slider[1]!=0 & slider[2]==0){
      t <- sprintf('%s Will allow %s %.2f smaller than the target word.', t, variabletype, abs(slider[1]))
    } else if(slider[1]!=0 & slider[2]!=0){
      t <- sprintf('%s Will allow %s %.2f smaller or %.2f greater than the target word.', t, variabletype, abs(slider[1]), slider[2])
    }
  }
  t
}

