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

