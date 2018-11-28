matchresults_unsorted <- reactive({
  
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
    matcher(lexops, corpus_recode(input$frequency.opt, if(input$frequency.log){"Zipf"}else{"fpmw"}), input$frequency.sl, input$check.frequency, input$string, colmeans_name=sprintf("Avg.%s", if(input$frequency.log){"Zipf"}else{"fpmw"})) %>%
    matcher(lexops, corpus_recode(input$pos.opt, "PoS"), NA, input$check.partofspeech, input$string, manual_str_in_x=if(input$check.manual.pos){input$manual.pos}else{NA}) %>%
    # Orthographic
    matcher(lexops, "Length", input$length.sl, input$check.length, input$string) %>%
    matcher(lexops, corpus_recode(input$bgfreq.opt, "BG"), input$bgfreq.sl, input$check.bgfreq, input$string, colmeans_name=("Avg.BG")) %>%
    matcher(lexops, sprintf("ON.%s%s", if(input$on.log){"Log_"}else{""}, switch(input$on.opt, "old20"="OLD20", "cn"="Colthearts_N")), input$on.sl, input$check.on, input$string) %>%
    matcher(lexops, "OS", input$os.sl, input$check.os, input$string) %>%
    # Phonological
    matcher(lexops, corpus_recode(input$syllables.opt, "Syllables"), input$syllables.sl, input$check.syllables, input$string, pron_nr=if(input$syllables.opt=='cmu'){get_pron_nr(input$manual.pron.phonemes, input$string)}else{NA}) %>%
    matcher(lexops, "Phonemes.CMU.pr1", input$phonemes.sl, input$check.phonemes, input$string, pron_nr=get_pron_nr(input$manual.pron.phonemes, input$string)) %>%
    matcher(lexops, "Rhyme.CMU.pr1", NA, input$check.rhyme, input$string, pron_nr=get_pron_nr(input$manual.pron.rhyme, input$string)) %>%
    matcher(lexops, sprintf("PN.%s%s.CMU.pr1", if(input$pn.log){"Log_"}else{""}, switch(input$pn.opt, "pld20"="PLD20", "cn"="Colthearts_N")), input$pn.sl, input$check.pn, input$string, pron_nr=get_pron_nr(input$manual.pron.pn, input$string)) %>%
    matcher(lexops, "PS", input$ps.sl, input$check.ps, input$string) %>%
    matcher(lexops, "CMU.PrN", input$prn.sl, input$check.prn, input$string) %>%
    # Semantic
    matcher(lexops, corpus_recode(input$fam.opt, "FAM"), input$fam.sl, input$check.fam, input$string) %>%
    matcher(lexops, corpus_recode(input$aoa.opt, "AoA"), input$aoa.sl, input$check.aoa, input$string) %>%
    matcher(lexops, corpus_recode(input$cnc.opt, "CNC"), input$cnc.sl, input$check.cnc, input$string) %>%
    matcher(lexops, corpus_recode(input$imag.opt, "IMAG"), input$imag.sl, input$check.imag, input$string) %>%
    matcher(lexops, corpus_recode(input$arou.opt, "AROU"), input$arou.sl, input$check.arou, input$string) %>%
    matcher(lexops, corpus_recode(input$val.opt, "VAL"), input$val.sl, input$check.val, input$string) %>%
    matcher(lexops, corpus_recode(input$dom.opt, "DOM"), input$dom.sl, input$check.dom, input$string) %>%
    matcher(lexops, corpus_recode(input$size.opt, "SIZE"), input$size.sl, input$check.size, input$string) %>%
    matcher(lexops, corpus_recode(input$gen.opt, "GEND"), input$gen.sl, input$check.gen, input$string) %>%
    matcher(lexops, corpus_recode(input$hum.opt, "HUM"), input$hum.sl, input$check.hum, input$string) %>%
    matcher(lexops, corpus_recode(input$rt.opt, if(input$rt.zscore){"RT_zscore"}else{"RT"}), input$rt.sl, input$check.rt, input$string) %>%
    matcher(lexops, corpus_recode(input$acc.opt, if(input$acc.zscore){"Accuracy_zscore"}else{"Accuracy"}), input$acc.sl, input$check.acc, input$string)
    
  
  # Get differences and distances
  matched_differences <- get_differences(matched, str_in=input$string)
  matched_distances <- get_distances(matched_differences)
  
  # Return the results
  if (input$results.format=='rv') {
    res <- matched
  } else if (input$results.format=='diff') {
    res <- matched_differences
  } else if (input$results.format=='dist') {
    res <- matched_distances
  }
  
  res
  
})

# sorting

matchresults <- reactive ({
  
  sort_vec <- c()
  sort_vec_desc <- c()
  
  for (sortnr in 1:5) {
    if (sprintf("match_results_sort_%i", sortnr) %in% names(input)) {
      if (input[[sprintf("match_results_sort_%i", sortnr)]] != "(None)") {
        sortnr_string <- as.character(input[[sprintf("match_results_sort_%i", sortnr)]])
        if (input[[sprintf("match_results_sort_%i_order", sortnr)]] == "Ascending") {
          sort_vec <- c(sort_vec, sortnr_string)
        } else {
          sort_vec_desc <- c(sort_vec_desc, sortnr_string)
        }
      }
      else {
        break
      }
    } else {
      break
    }
  }
  
  if (length(sort_vec)>0 & length(sort_vec_desc)==0) {
    matchresults_unsorted() %>%
      arrange(!!! syms(sort_vec))
  } else if (length(sort_vec)==0 & length(sort_vec_desc)>0) {
    matchresults_unsorted() %>%
      arrange(desc(!!! syms(sort_vec_desc)))
  } else if (length(sort_vec)>0 & length(sort_vec_desc)>0) {
    matchresults_unsorted() %>%
      arrange(!!! syms(sort_vec), desc(!!! syms(sort_vec_desc)))
  } else {
    matchresults_unsorted()
  }

})
