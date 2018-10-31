resultsdata <- reactive({
  
  str_in <- input$string
  
  out <- dplyr::select(dat, string, Length)
  out_diff <- dplyr::select(dat, string)
  out_dist <- out_diff
  
  if (str_in %in% out$string) {
    
    # Match by Length
    if(input$check.length) {
      len_tol <- (nchar(str_in)+input$length.sl[1]):(nchar(str_in)+input$length.sl[2])  # vector for allowed lengths
    } else {
      len_tol <- 1:max(dat$Length)
    }
    out <- out %>% filter(Length %in% len_tol | string==str_in)
    # calculate length diffs & distances
    out_diff <- out_diff %>% filter(string %in% out$string)
    out_dist <- out_dist %>% filter(string %in% out$string)
    out_diff$Length <- out$Length - out$Length[out$string==str_in]
    out_dist$Length <- abs(out$Length[out$string==str_in] - out$Length)
    
    # Match by Frequency
    if(input$check.frequency) {
      
      out_copy <- left_join(out, dat, by='string')
      if(input$frequency.log) {
        out_copy$bnc_w <- out_copy$bnc.wZipf
        out_copy$bnc_s <- out_copy$bnc.sZipf
        out_copy$suk <- out_copy$subtlex_uk.zipf
        out_copy$sus <- out_copy$subtlex_us.zipf
      } else {
        out_copy$bnc_w <- out_copy$bnc.wFpmw
        out_copy$bnc_s <- out_copy$bnc.sFpmw
        out_copy$suk <- out_copy$subtlex_uk.fpmw
        out_copy$sus <- out_copy$subtlex_us.fpmw
      }
      out_copy$Frequency <- rowMeans(select(out_copy, one_of(input$frequency.opt)), dims=1, na.rm=T)
      
      # calculate diffs & distances
      str_in_freq <- out_copy$Frequency[out_copy$string==str_in]
      out_copy$Frequency_diff <- out_copy$Frequency - str_in_freq
      out_copy$Frequency_dist <- abs(out_copy$Frequency_diff)
      
      
      out <- left_join(out, select(out_copy, string, Frequency, Frequency_dist), by='string') %>%
        dplyr::arrange(Frequency_dist)  # sort by frequency distance
      out <- out %>% select(-Frequency_dist)  # remove now redundant column
      
      out_diff <- left_join(out_diff, select(out_copy, string, Frequency_dist), by='string') %>%
        mutate(Frequency=out_copy$Frequency_diff) %>%
        arrange(Frequency_dist)  # sort by frequency distance
      out_diff <- out_diff %>% select(-Frequency_dist)  # remove now redundant column
      
      out_dist <- left_join(out_dist, select(out_copy, string), by='string') %>%
        mutate(Frequency=out_copy$Frequency_dist) %>%
        arrange(Frequency)  # sort by frequency distance (for this df, 'Frequency')
      
      out$Frequency <- round(out$Frequency, 2)
      out_diff$Frequency <- round(out_diff$Frequency, 2)
      out_dist$Frequency <- round(out_dist$Frequency, 2)
      
      out <- dplyr::filter(out, Frequency >= str_in_freq+input$frequency.sl[1] | string==str_in,
                           Frequency <= str_in_freq+input$frequency.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by part of speech
    if (input$check.partofspeech){
      out_copy <- left_join(out, dat, by='string')
      out_copy$pos <- switch(input$pos.opt,
                             'suk'=out_copy$subtlex_uk.DomPoS,
                             'bnc_w'=out_copy$bnc.wDomPoS,
                             'bnc_s'=out_copy$bnc.sDomPoS,
                             'elp'=out_copy$elp.DomPoS)
      str_in_pos <- if(input$check.manual.pos){
        input$manual.pos
      }else{
        out_copy$pos[out_copy$string==str_in]
      }
      out_copy <- dplyr::filter(out_copy, pos==str_in_pos | string==str_in)
      
      out <- dplyr::filter(out, string %in% out_copy$string) %>%
        mutate(PoS = out_copy$subtlex_uk.DomPoS)
      out_diff <- dplyr::filter(out_diff, string %in% out$string) %>%
        mutate(PoS = out_copy$subtlex_uk.DomPoS)
      out_dist <- dplyr::filter(out_dist, string %in% out$string) %>%
        mutate(PoS = out_copy$subtlex_uk.DomPoS)
    }
    
    # Match by Bigram Frequency
    if(input$check.bgfreq) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$BG.Frequency <- rowMeans(select(out_copy, one_of(input$bgfreq.opt)), dims=1, na.rm=T)
      
      # calculate diffs & distances
      str_in_freq <- out_copy$BG.Frequency[out_copy$string==str_in]
      out_copy$BG.Frequency_diff <- out_copy$BG.Frequency - str_in_freq
      out_copy$BG.Frequency_dist <- abs(out_copy$BG.Frequency_diff)
      
      
      out <- left_join(out, select(out_copy, string, BG.Frequency), by='string')
      
      out_diff <- left_join(out_diff, select(out_copy, string), by='string') %>%
        mutate(BG.Frequency=out_copy$BG.Frequency_diff)
      
      out_dist <- left_join(out_dist, select(out_copy, string), by='string') %>%
        mutate(BG.Frequency=out_copy$BG.Frequency_dist)
      
      out$BG.Frequency <- round(out$BG.Frequency, 2)
      out_diff$BG.Frequency <- round(out_diff$BG.Frequency, 2)
      out_dist$BG.Frequency <- round(out_dist$BG.Frequency, 2)
      
      out <- dplyr::filter(out, BG.Frequency >= str_in_freq+input$bgfreq.sl[1] | string==str_in,
                           BG.Frequency <= str_in_freq+input$bgfreq.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Orthographic Neighbourhood
    if(input$check.on) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$ON <- switch(input$on.opt, 'old20'=out_copy$old20, 'cn'=out_copy$coltheart.N)
      if(input$on.log){out_copy$ON <- log(out_copy$ON)}
      
      # calculate diffs & distances
      str_in_on <- out_copy$ON[out_copy$string==str_in]
      out_copy$on_diff <- out_copy$ON - str_in_on
      out_copy$on_dist <- abs(out_copy$on_diff)
      
      out <- left_join(out, select(out_copy, string, ON), by='string')
      out_diff <- mutate(out_diff, ON=out_copy$on_diff)
      out_dist <- mutate(out_dist, ON=out_copy$on_dist)
      
      out$ON <- round(out$ON, 2)
      out_diff$ON <- round(out_diff$ON, 2)
      out_dist$ON <- round(out_dist$ON, 2)
      
      out <- dplyr::filter(out, ON >= str_in_on+input$on.sl[1] | string==str_in,
                           ON <= str_in_on+input$on.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Orthographic Similarity
    if(input$check.os) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$OS <- as.integer(switch(input$os.opt,
                                       'ld'=vwr::levenshtein.distance(str_in, out_copy$string),
                                       'ldd'=vwr::levenshtein.damerau.distance(str_in, out_copy$string)))
      
      # calculate diffs & distances
      str_in_os <- out_copy$OS[out_copy$string==str_in]
      out_copy$os_diff <- out_copy$OS - str_in_os
      out_copy$os_dist <- abs(out_copy$os_diff)
      
      out <- left_join(out, select(out_copy, string, OS), by='string')
      out_diff <- mutate(out_diff, OS=out_copy$os_diff)
      out_dist <- mutate(out_dist, OS=out_copy$os_dist)
      
      out$OS <- round(out$OS, 2)
      out_diff$OS <- round(out_diff$OS, 2)
      out_dist$OS <- round(out_dist$OS, 2)
      
      out <- dplyr::filter(out, OS >= str_in_os+input$os.sl[1] | string==str_in,
                           OS <= str_in_os+input$os.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Syllables
    if (input$check.syllables){
      out_copy <- left_join(out, dat, by='string')
      str_in_syll <- out_copy$mhyph.syllables[out_copy$string==str_in]
      
      syll_tol <- (str_in_syll+input$syllables.sl[1]):(str_in_syll+input$syllables.sl[2])  # vector for allowed syllables
      out_copy <- dplyr::filter(out_copy, mhyph.syllables %in% syll_tol | string==str_in)
      
      out <- dplyr::filter(out, string %in% out_copy$string) %>%
        mutate(Syllables = out_copy$mhyph.syllables)
      out_diff <- dplyr::filter(out_diff, string %in% out$string) %>%
        mutate(Syllables = out_copy$mhyph.syllables)
      out_dist <- dplyr::filter(out_dist, string %in% out$string) %>%
        mutate(Syllables = out_copy$mhyph.syllables)
    }
    
    # Match by Phonemes
    if(input$check.phonemes) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$Phonemes <- switch(input$phonemes.opt, 'cmu'=out_copy$cmu.N_phonemes)
      
      # calculate diffs & distances
      str_in_phonemes <- out_copy$Phonemes[out_copy$string==str_in]
      out_copy$phonemes_diff <- out_copy$Phonemes - str_in_phonemes
      out_copy$phonemes_dist <- abs(out_copy$phonemes_diff)
      
      out <- left_join(out, select(out_copy, string, Phonemes), by='string')
      out_diff <- mutate(out_diff, Phonemes=out_copy$phonemes_diff)
      out_dist <- mutate(out_dist, Phonemes=out_copy$phonemes_dist)
      
      out$Phonemes <- round(out$Phonemes, 2)
      out_diff$Phonemes <- round(out_diff$Phonemes, 2)
      out_dist$Phonemes <- round(out_dist$Phonemes, 2)
      
      out <- dplyr::filter(out, Phonemes >= str_in_phonemes+input$phonemes.sl[1] | string==str_in,
                           Phonemes <= str_in_phonemes+input$phonemes.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Phonological Neighbourhood
    if(input$check.pn) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$PN <- switch(input$pn.opt, 'pld20'=out_copy$cmu.pr1_pld20, 'cn'=out_copy$cmu.pr1_phon.coltheart.N)
      if(input$pn.log){out_copy$PN <- log(out_copy$PN)}
      
      # handle multiple pronunciations by getting value for selected pronunciation
      pron_summ <- get_pronunciations(input$string, df = dat) %>%
        unname() %>%
        lapply(arpabet_convert, to="two", sep='-') %>%
        unlist(use.names = F)
      pron_nr <- match(input$manual.pron.pn, pron_summ)
      xsource <- switch(input$pn.opt, 'pld20'='pld20', 'cn'='phon.coltheart.N')
      xsource_prx <- sprintf("cmu.pr%i_%s", pron_nr, xsource)
      str_in_pn <- dat[[xsource_prx]][dat$string==input$string]
      
      # calculate diffs & distances
      out_copy$pn_diff <- out_copy$PN - str_in_pn
      out_copy$pn_dist <- abs(out_copy$pn_diff)
      
      out <- left_join(out, select(out_copy, string, PN), by='string')
      out_diff <- mutate(out_diff, PN=out_copy$pn_diff)
      out_dist <- mutate(out_dist, PN=out_copy$pn_dist)
      
      out$PN <- round(out$PN, 2)
      out_diff$PN <- round(out_diff$PN, 2)
      out_dist$PN <- round(out_dist$PN, 2)
      
      out <- dplyr::filter(out, PN >= str_in_pn+input$pn.sl[1] | string==str_in,
                           PN <= str_in_pn+input$pn.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Phonological Similarity
    if(input$check.ps) {
      
      str_in_pronun <- dat$cmu.pronun_1letter[dat$string==input$string]
      out_copy <- left_join(out, dat, by='string')
      out_copy$PS <- as.integer(switch(input$ps.opt,
                                       'ld'=vwr::levenshtein.distance(str_in_pronun, out_copy$cmu.pronun_1letter),
                                       'ldd'=vwr::levenshtein.damerau.distance(str_in_pronun, out_copy$cmu.pronun_1letter)))
      
      # calculate diffs & distances
      str_in_ps <- out_copy$PS[out_copy$string==str_in]
      out_copy$ps_diff <- out_copy$PS - str_in_ps
      out_copy$ps_dist <- abs(out_copy$ps_diff)
      
      out <- left_join(out, select(out_copy, string, PS), by='string')
      out_diff <- mutate(out_diff, PS=out_copy$ps_diff)
      out_dist <- mutate(out_dist, PS=out_copy$ps_dist)
      
      out$PS <- round(out$PS, 2)
      out_diff$PS <- round(out_diff$PS, 2)
      out_dist$PS <- round(out_dist$PS, 2)
      
      out <- dplyr::filter(out, PS >= str_in_ps+input$ps.sl[1] | string==str_in,
                           PS <= str_in_ps+input$ps.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Familiarity
    if(input$check.fam) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$FAM <- switch(input$fam.opt, 'cp'=out_copy$cp.FAM, 'gn'=out_copy$gn.FAM)
      
      # calculate diffs & distances
      str_in_fam <- out_copy$FAM[out_copy$string==str_in]
      out_copy$fam_diff <- out_copy$FAM - str_in_fam
      out_copy$fam_dist <- abs(out_copy$fam_diff)
      
      out <- left_join(out, select(out_copy, string, FAM), by='string')
      out_diff <- mutate(out_diff, FAM=out_copy$fam_diff)
      out_dist <- mutate(out_dist, FAM=out_copy$fam_dist)
      
      out$FAM <- round(out$FAM, 2)
      out_diff$FAM <- round(out_diff$FAM, 2)
      out_dist$FAM <- round(out_dist$FAM, 2)
      
      out <- dplyr::filter(out, FAM >= str_in_fam+input$fam.sl[1] | string==str_in,
                           FAM <= str_in_fam+input$fam.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Age of Acquisition
    if(input$check.aoa) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$AoA <- switch(input$aoa.opt, 'kuperman'=out_copy$kuperman.AOA, 'gn'=out_copy$gn.AOA)
      
      # calculate diffs & distances
      str_in_aoa <- out_copy$AoA[out_copy$string==str_in]
      out_copy$aoa_diff <- out_copy$AoA - str_in_aoa
      out_copy$aoa_dist <- abs(out_copy$aoa_diff)
      
      out <- left_join(out, select(out_copy, string, AoA), by='string')
      out_diff <- mutate(out_diff, AoA=out_copy$aoa_diff)
      out_dist <- mutate(out_dist, AoA=out_copy$aoa_dist)
      
      out$AoA <- round(out$AoA, 2)
      out_diff$AoA <- round(out_diff$AoA, 2)
      out_dist$AoA <- round(out_dist$AoA, 2)
      
      out <- dplyr::filter(out, AoA >= str_in_aoa+input$aoa.sl[1] | string==str_in,
                           AoA <= str_in_aoa+input$aoa.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Concreteness
    if(input$check.cnc) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$CNC <- switch(input$cnc.opt, 'brysbaert'=out_copy$brysbaert.CNC, 'gn'=out_copy$gn.CNC)
      
      # calculate diffs & distances
      str_in_cnc <- out_copy$CNC[out_copy$string==str_in]
      out_copy$cnc_diff <- out_copy$CNC - str_in_cnc
      out_copy$cnc_dist <- abs(out_copy$cnc_diff)
      
      out <- left_join(out, select(out_copy, string, CNC), by='string')
      out_diff <- mutate(out_diff, CNC=out_copy$cnc_diff)
      out_dist <- mutate(out_dist, CNC=out_copy$cnc_dist)
      
      out$CNC <- round(out$CNC, 2)
      out_diff$CNC <- round(out_diff$CNC, 2)
      out_dist$CNC <- round(out_dist$CNC, 2)
      
      out <- dplyr::filter(out, CNC >= str_in_cnc+input$cnc.sl[1] | string==str_in,
                           CNC <= str_in_cnc+input$cnc.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Imageability
    if(input$check.imag) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$IMAG <- switch(input$imag.opt, 'cp'=out_copy$cp.IMAG, 'gn'=out_copy$gn.IMAG)
      
      # calculate diffs & distances
      str_in_imag <- out_copy$IMAG[out_copy$string==str_in]
      out_copy$imag_diff <- out_copy$IMAG - str_in_imag
      out_copy$imag_dist <- abs(out_copy$imag_diff)
      
      out <- left_join(out, select(out_copy, string, IMAG), by='string')
      out_diff <- mutate(out_diff, IMAG=out_copy$imag_diff)
      out_dist <- mutate(out_dist, IMAG=out_copy$imag_dist)
      
      out$IMAG <- round(out$IMAG, 2)
      out_diff$IMAG <- round(out_diff$IMAG, 2)
      out_dist$IMAG <- round(out_dist$IMAG, 2)
      
      out <- dplyr::filter(out, IMAG >= str_in_imag+input$imag.sl[1] | string==str_in,
                           IMAG <= str_in_imag+input$imag.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Arousal
    if(input$check.arou) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$AROU <- switch(input$arou.opt, 'warriner'=out_copy$warriner.AROU, 'gn'=out_copy$gn.AROU)
      
      # calculate diffs & distances
      str_in_arou <- out_copy$AROU[out_copy$string==str_in]
      out_copy$arou_diff <- out_copy$AROU - str_in_arou
      out_copy$arou_dist <- abs(out_copy$arou_diff)
      
      out <- left_join(out, select(out_copy, string, AROU), by='string')
      out_diff <- mutate(out_diff, AROU=out_copy$arou_diff)
      out_dist <- mutate(out_dist, AROU=out_copy$arou_dist)
      
      out$AROU <- round(out$AROU, 2)
      out_diff$AROU <- round(out_diff$AROU, 2)
      out_dist$AROU <- round(out_dist$AROU, 2)
      
      out <- dplyr::filter(out, AROU >= str_in_arou+input$arou.sl[1] | string==str_in,
                           AROU <= str_in_arou+input$arou.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Valence
    if(input$check.val) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$VAL <- switch(input$val.opt, 'warriner'=out_copy$warriner.VAL, 'gn'=out_copy$gn.VAL)
      
      # calculate diffs & distances
      str_in_val <- out_copy$VAL[out_copy$string==str_in]
      out_copy$val_diff <- out_copy$VAL - str_in_val
      out_copy$val_dist <- abs(out_copy$val_diff)
      
      out <- left_join(out, select(out_copy, string, VAL), by='string')
      out_diff <- mutate(out_diff, VAL=out_copy$val_diff)
      out_dist <- mutate(out_dist, VAL=out_copy$val_dist)
      
      out$VAL <- round(out$VAL, 2)
      out_diff$VAL <- round(out_diff$VAL, 2)
      out_dist$VAL <- round(out_dist$VAL, 2)
      
      out <- dplyr::filter(out, VAL >= str_in_val+input$val.sl[1] | string==str_in,
                           VAL <= str_in_val+input$val.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Dominance
    if(input$check.dom) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$DOM <- switch(input$dom.opt, 'warriner'=out_copy$warriner.DOM, 'gn'=out_copy$gn.DOM)
      
      # calculate diffs & distances
      str_in_dom <- out_copy$DOM[out_copy$string==str_in]
      out_copy$dom_diff <- out_copy$DOM - str_in_dom
      out_copy$dom_dist <- abs(out_copy$dom_diff)
      
      out <- left_join(out, select(out_copy, string, DOM), by='string')
      out_diff <- mutate(out_diff, DOM=out_copy$dom_diff)
      out_dist <- mutate(out_dist, DOM=out_copy$dom_dist)
      
      out$DOM <- round(out$DOM, 2)
      out_diff$DOM <- round(out_diff$DOM, 2)
      out_dist$DOM <- round(out_dist$DOM, 2)
      
      out <- dplyr::filter(out, DOM >= str_in_dom+input$dom.sl[1] | string==str_in,
                           DOM <= str_in_dom+input$dom.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Semantic Size
    if(input$check.size) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$SIZE <- switch(input$size.opt, 'gn'=out_copy$gn.SIZE)
      
      # calculate diffs & distances
      str_in_size <- out_copy$SIZE[out_copy$string==str_in]
      out_copy$size_diff <- out_copy$SIZE - str_in_size
      out_copy$size_dist <- abs(out_copy$size_diff)
      
      out <- left_join(out, select(out_copy, string, SIZE), by='string')
      out_diff <- mutate(out_diff, SIZE=out_copy$size_diff)
      out_dist <- mutate(out_dist, SIZE=out_copy$size_dist)
      
      out$SIZE <- round(out$SIZE, 2)
      out_diff$SIZE <- round(out_diff$SIZE, 2)
      out_dist$SIZE <- round(out_dist$SIZE, 2)
      
      out <- dplyr::filter(out, SIZE >= str_in_size+input$imag.sl[1] | string==str_in,
                           SIZE <= str_in_size+input$imag.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Semantic Gender
    if(input$check.gen) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$GEND <- switch(input$gen.opt, 'gn'=out_copy$gn.GEND)
      
      # calculate diffs & distances
      str_in_gen <- out_copy$GEND[out_copy$string==str_in]
      out_copy$gen_diff <- out_copy$GEND - str_in_gen
      out_copy$gen_dist <- abs(out_copy$gen_diff)
      
      out <- left_join(out, select(out_copy, string, GEND), by='string')
      out_diff <- mutate(out_diff, GEND=out_copy$gen_diff)
      out_dist <- mutate(out_dist, GEND=out_copy$gen_dist)
      
      out$GEN <- round(out$GEND, 2)
      out_diff$GEND <- round(out_diff$GEND, 2)
      out_dist$GEND <- round(out_dist$GEND, 2)
      
      out <- dplyr::filter(out, GEND >= str_in_gen+input$gen.sl[1] | string==str_in,
                           GEND <= str_in_gen+input$gen.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by RT
    if(input$check.rt) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$RT <- switch(input$rt.opt,
                            'blp'=if(input$rt.zscore) {out_copy$blp.rt.zscore} else {out_copy$blp.rt},
                            'elp'=if(input$rt.zscore) {out_copy$elp.rt.zscore} else {out_copy$elp.rt})
      
      # calculate diffs & distances
      str_in_rt <- out_copy$RT[out_copy$string==str_in]
      out_copy$rt_diff <- out_copy$RT - str_in_rt
      out_copy$rt_dist <- abs(out_copy$rt_diff)
      
      out <- left_join(out, select(out_copy, string, RT), by='string')
      out_diff <- mutate(out_diff, RT=out_copy$rt_diff)
      out_dist <- mutate(out_dist, RT=out_copy$rt_dist)
      
      out$RT <- round(out$RT, 2)
      out_diff$RT <- round(out_diff$RT, 2)
      out_dist$RT <- round(out_dist$RT, 2)
      
      out <- dplyr::filter(out, RT >= str_in_rt+input$rt.sl[1] | string==str_in,
                           RT <= str_in_rt+input$rt.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Match by Accuracy
    if(input$check.acc) {
      
      out_copy <- left_join(out, dat, by='string')
      out_copy$Acc <- switch(input$acc.opt,
                             'blp'=if(input$acc.zscore) {out_copy$blp.accuracy.zscore} else {out_copy$blp.accuracy},
                             'elp'=if(input$acc.zscore) {out_copy$elp.accuracy.zscore} else {out_copy$elp.accuracy})
      
      # calculate diffs & distances
      str_in_acc <- out_copy$Acc[out_copy$string==str_in]
      out_copy$acc_diff <- out_copy$Acc - str_in_acc
      out_copy$acc_dist <- abs(out_copy$acc_diff)
      
      out <- left_join(out, select(out_copy, string, Acc), by='string')
      out_diff <- mutate(out_diff, Acc=out_copy$acc_diff)
      out_dist <- mutate(out_dist, Acc=out_copy$acc_dist)
      
      out$Acc <- round(out$Acc, 2)
      out_diff$Acc <- round(out_diff$Acc, 2)
      out_dist$Acc <- round(out_dist$Acc, 2)
      
      out <- dplyr::filter(out, Acc >= str_in_acc+input$acc.sl[1] | string==str_in,
                           Acc <= str_in_acc+input$acc.sl[2] | string==str_in)
      out_diff <- dplyr::filter(out_diff, string %in% out$string)
      out_dist <- dplyr::filter(out_dist, string %in% out$string)
    }
    
    # Return the results
    if (input$results.format=='rv') {
      res <- out
    } else if (input$results.format=='diff') {
      res <- out_diff
    } else if (input$results.format=='dist') {
      res <- out_dist
    }
    
    # Rename variables as required
    if ('Word.Frequency' %in% names(res)) {
      res <- res %>% dplyr::rename(Word.Frequency=Frequency)
    }
    
  } else {
    res <- as.data.frame(NA)  # handles strings which are not in the database
  }
  
  res
  
})