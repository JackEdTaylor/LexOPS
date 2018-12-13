matchresults_undistanced <- reactive({
  
  # add similarity information to the lexops df
  if (input$check.os) {
    lexops$OS <- as.integer(switch(input$os.opt,
                                   'ld'=vwr::levenshtein.distance(input$string, lexops$string),
                                   'ldd'=vwr::levenshtein.damerau.distance(input$string, lexops$string)))
  }
  
  if (input$check.ps) {
    lexops$PS <- as.integer(switch(input$ps.opt,
                                   'ld'=vwr::levenshtein.distance(lexops[[sprintf("CMU.pr%i_1letter", get_pron_nr(input$manual.pron.ps, input$string))]][lexops$string==input$string], lexops$CMU.pr1_1letter),
                                   'ldd'=vwr::levenshtein.damerau.distance(lexops[[sprintf("CMU.pr%i_1letter", get_pron_nr(input$manual.pron.ps, input$string))]][lexops$string==input$string], lexops$CMU.pr1_1letter)))
  }
  
  
  matched <- select(lexops, string) %>%
    # Lexical
    matcher(lexops, corpus_recode(input$frequency.opt, if(input$frequency.log){"Zipf"}else{"fpmw"}), input$frequency.sl, input$check.frequency, input$string, colmeans_name=sprintf("Avg.%s", if(input$frequency.log){"Zipf"}else{"fpmw"}), do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$pos.opt, "PoS"), NA, input$check.partofspeech, input$string, manual_str_in_x=if(input$check.manual.pos){input$manual.pos}else{NA}, do.filter=!input$check.match.ignorefilters) %>%
    # Orthographic
    matcher(lexops, "Length", input$length.sl, input$check.length, input$string, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$bgfreq.opt, "BG"), input$bgfreq.sl, input$check.bgfreq, input$string, colmeans_name=("Avg.BG"), do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, sprintf("ON.%s%s", if(input$on.log){"Log_"}else{""}, switch(input$on.opt, "old20"="OLD20", "cn"="Colthearts_N")), input$on.sl, input$check.on, input$string, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, "OS", input$os.sl, input$check.os, input$string) %>%
    # Phonological
    matcher(lexops, corpus_recode(input$syllables.opt, "Syllables"), input$syllables.sl, input$check.syllables, input$string, pron_nr=if(input$syllables.opt=='cmu'){get_pron_nr(input$manual.pron.phonemes, input$string)}else{NA}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, "Phonemes.CMU.pr1", input$phonemes.sl, input$check.phonemes, input$string, pron_nr=get_pron_nr(input$manual.pron.phonemes, input$string), do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, "Rhyme.CMU.pr1", NA, input$check.rhyme, input$string, pron_nr=get_pron_nr(input$manual.pron.rhyme, input$string), do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, sprintf("PN.%s%s.CMU.pr1", if(input$pn.log){"Log_"}else{""}, switch(input$pn.opt, "pld20"="PLD20", "cn"="Colthearts_N")), input$pn.sl, input$check.pn, input$string, pron_nr=get_pron_nr(input$manual.pron.pn, input$string), do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, "PS", input$ps.sl, input$check.ps, input$string, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, "CMU.PrN", input$prn.sl, input$check.prn, input$string, do.filter=!input$check.match.ignorefilters) %>%
    # Semantic
    matcher(lexops, corpus_recode(input$fam.opt, "FAM"), input$fam.sl, input$check.fam, input$string, colmeans_name="Avg.FAM", scale.cols=if(length(input$fam.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$aoa.opt, "AoA"), input$aoa.sl, input$check.aoa, input$string, colmeans_name="Avg.AoA", scale.cols=if(length(input$aoa.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$cnc.opt, "CNC"), input$cnc.sl, input$check.cnc, input$string, colmeans_name="Avg.CNC", scale.cols=if(length(input$cnc.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$imag.opt, "IMAG"), input$imag.sl, input$check.imag, input$string, colmeans_name="Avg.IMAG", scale.cols=if(length(input$imag.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$arou.opt, "AROU"), input$arou.sl, input$check.arou, input$string, colmeans_name="Avg.AROU", scale.cols=if(length(input$arou.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$val.opt, "VAL"), input$val.sl, input$check.val, input$string, colmeans_name="Avg.VAL", scale.cols=if(length(input$val.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$dom.opt, "DOM"), input$dom.sl, input$check.dom, input$string, colmeans_name="Avg.DOM", scale.cols=if(length(input$dom.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$size.opt, "SIZE"), input$size.sl, input$check.size, input$string, colmeans_name="Avg.SIZE", scale.cols=if(length(input$size.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$gen.opt, "GEND"), input$gen.sl, input$check.gen, input$string, colmeans_name="Avg.GEND", scale.cols=if(length(input$gen.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$hum.opt, "HUM"), input$hum.sl, input$check.hum, input$string, colmeans_name="Avg.HUM", scale.cols=if(length(input$hum.opt)>1) {T} else {F}, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$rt.opt, if(input$rt.zscore){"RT_zscore"}else{"RT"}), input$rt.sl, input$check.rt, input$string, do.filter=!input$check.match.ignorefilters) %>%
    matcher(lexops, corpus_recode(input$acc.opt, if(input$acc.zscore){"Accuracy_zscore"}else{"Accuracy"}), input$acc.sl, input$check.acc, input$string, do.filter=!input$check.match.ignorefilters)
  
  
  # Get differences and distances
  matched_differences <- get_differences(matched, str_in=input$string)
  matched_distances <- get_distances(matched_differences)
  
  # Select results format
  if (input$results.format=='rv') {
    res <- matched
  } else if (input$results.format=='diff') {
    res <- matched_differences
  } else if (input$results.format=='dist') {
    res <- matched_distances
  }
  
  res
  
})


# add the distance measures if selected

matchresults_unsorted <- reactive ({

  res <- matchresults_undistanced()
  
  # match by Euclidean Distance, using all columns is default, so assume this is true if the input is.null
  if (input$check.matchdist.ed) {
    target_cols <-
      if (!is.null(input$match_results_ed_all)) {
        if (input$match_results_ed_all=="manual") {
          input$match_results_ed_opts
        } else {
          colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
        }
      } else {
        colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
      }
    
    res <- res %>%
      mutate(Euclidean.Distance = get_euclidean_distance(res, input$string, columns=target_cols)) %>%
      select(Euclidean.Distance, everything()) %>%
      select(string, everything())
  }
  
  if (input$check.matchdist.cb) {
    target_cols <-
      if (!is.null(input$match_results_cb_all)) {
        if (input$match_results_cb_all=="manual") {
          input$match_results_cb_opts
        } else {
          colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
        }
      } else {
        colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
      }
    
    res <- res %>%
      mutate(CityBlock.Distance = get_cityblock_distance(res, input$string, columns=target_cols)) %>%
      select(CityBlock.Distance, everything()) %>%
      select(string, everything())
  }
  
  res
  
})



# sorting, and removing the target word

matchresults <- reactive ({
  
  out <- matchresults_unsorted()
  
  for (sortnr in 1:5) {
    if (sprintf("match_results_sort_%i", sortnr) %in% names(input)) {
      sortnr_string <- as.character(input[[sprintf("match_results_sort_%i", sortnr)]])
      sortnr_order <- as.character(input[[sprintf("match_results_sort_%i_order", sortnr)]])
      if (sortnr_string != "(None)") {
        if (sortnr_order == "Ascending") {
          out <- out %>%
            arrange(!!! sym(sortnr_string))
        } else {
          out <- out %>%
            arrange(desc(!!! sym(sortnr_string)))
        }
      }
    }
  }
  
  # sort by euclidean distance as default if the UIs haven't rendered
  if (is.null(input$match_results_sort_1_order)) {
    if (input$check.matchdist.ed & "Euclidean.Distance" %in% colnames(out)) {
      out <- arrange(out, Euclidean.Distance)
    }
  }
  
  out %>%
    filter(string != input$string)
  
})