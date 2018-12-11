## GENERATE

# Frequency
output$plot.freq_gen <- renderPlot({
  out_copy <- dat
  if(input$frequency.log_gen) {
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
  out_copy$Frequency <- rowMeans(select(out_copy, one_of(input$frequency.opt_gen)), dims=1, na.rm=T)
  dens.plot(x='Frequency', selected=input$check.frequency_gen,
            redline=NA,
            shade=c(input$frequency.sl_gen[1], input$frequency.sl_gen[2]),
            df=out_copy,
            boxtype='primary',
            text.lowscale='Less Frequent', text.highscale='More Frequent')
})

## MATCH

# Length
output$plot.length <- renderPlot({
  str_in_x <- dat$Length[dat$string==input$string]
  dens.plot(x='Length', selected=input$check.length,
            redline=str_in_x,
            shade=c(str_in_x + input$length.sl[1], str_in_x + input$length.sl[2]),
            boxtype='warning',
            text.lowscale='Shorter', text.highscale='Longer')
})
# Frequency
output$plot.freq <- renderPlot({
  out_copy <- lexops
  column <- corpus_recode(input$frequency.opt, if(input$frequency.log){"Zipf"}else{"fpmw"})
  out_copy$Frequency <- rowMeans(select(out_copy, one_of(column)), dims=1, na.rm=T)
  str_in_x <- out_copy$Frequency[out_copy$string==input$string]
  dens.plot(x='Frequency', selected=input$check.frequency,
            redline=str_in_x,
            shade=c(str_in_x + input$frequency.sl[1], str_in_x + input$frequency.sl[2]),
            df=out_copy,
            boxtype='primary',
            text.lowscale='Less Frequent', text.highscale='More Frequent')
})
# Part of Speech
output$plot.pos <- renderPlot({
  str_in_x <- switch(input$pos.opt,
                     'suk'=dat$subtlex_uk.DomPoS[dat$string==input$string],
                     'bnc_w'=dat$bnc.wDomPoS[dat$string==input$string],
                     'bnc_s'=dat$bnc.sDomPoS[dat$string==input$string],
                     'elp'=dat$elp.DomPoS[dat$string==input$string])
  
  selectedpos <- if(input$check.manual.pos){
    input$manual.pos
  } else {
    str_in_x
  }
  
  pos.plot(xname = switch(input$pos.opt,
                          'suk'='subtlex_uk.DomPoS',
                          'bnc_w'='bnc.wDomPoS',
                          'bnc_s'='bnc.sDomPoS',
                          'elp'='elp.DomPoS'),
           selected=input$check.partofspeech, PoS=selectedpos,
           label_top_N = if(input$pos.opt=='bnc_w') {4} else {if(input$pos.opt=='elp') {3} else {5}})
})
# Bigram Frequency
output$plot.bgfreq <- renderPlot({
  out_copy <- dat
  out_copy$BG.Frequency <- rowMeans(select(out_copy, one_of(input$bgfreq.opt)), dims=1, na.rm=T)
  str_in_x <- out_copy$BG.Frequency[out_copy$string==input$string]
  dens.plot(x='BG.Frequency', selected=input$check.bgfreq,
            redline=str_in_x,
            shade=c(str_in_x + input$bgfreq.sl[1], str_in_x + input$bgfreq.sl[2]),
            df=out_copy,
            boxtype='warning',
            text.lowscale='Less Frequent', text.highscale='More Frequent')
})
# Orthographic Neighbourhood
output$plot.on <- renderPlot({
  xsource <- switch(input$on.opt, 'old20'='old20', 'cn'='coltheart.N')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  x_lowtext <- switch(input$on.opt, 'old20'='Larger Neighborhood', 'cn'='Smaller Neighborhood')
  x_hightext <- switch(input$on.opt, 'old20'='Smaller Neighborhood', 'cn'='Larger Neighborhood')
  dens.plot(x=xsource, selected=input$check.on,
            redline=str_in_x,
            shade=c(str_in_x + input$on.sl[1], str_in_x + input$on.sl[2]),
            boxtype='warning',
            text.lowscale=x_lowtext, text.highscale=x_hightext,
            log.transform = input$on.log)
})
# Orthographic Similarity
output$plot.os <- renderPlot({
  dat_copy <- dat
  dat_copy$OS <- as.integer(switch(input$os.opt,
                                   'ld'=vwr::levenshtein.distance(input$string, dat_copy$string),
                                   'ldd'=vwr::levenshtein.damerau.distance(input$string, dat_copy$string)))
  str_in_x <- dat_copy$OS[dat_copy$string==input$string]
  dens.plot(x='OS', df=dat_copy, selected=input$check.os,
            redline=str_in_x,
            shade=c(str_in_x + input$os.sl[1], str_in_x + input$os.sl[2]),
            boxtype='warning',
            text.lowscale='More Similar', text.highscale='Less Similar')
})
# Syllables
output$plot.syllables <- renderPlot({
  if (input$syllables.opt=='mp'){
    xsource_prx <- 'mhyph.syllables'
    xsource_pr1 <- xsource_prx
  } else if (input$syllables.opt=='cmu') {
    xsource_pr1 <- sprintf("cmu.pr1_syllables")
    # handle multiple pronunciations by getting value for selected pronunciation
    pron_summ <- get_pronunciations(input$string, df = dat) %>%
      unname() %>%
      lapply(arpabet_convert, to="two", sep='-') %>%
      unlist(use.names = F)
    pron_nr <- match(input$manual.pron.syllables, pron_summ)
    xsource_prx <- sprintf("cmu.pr%i_syllables", pron_nr)
  }
  str_in_x <- dat[[xsource_prx]][dat$string==input$string]
  dens.plot(x=xsource_pr1, selected=input$check.syllables,
            redline=str_in_x,
            shade=c(str_in_x + input$syllables.sl[1], str_in_x + input$syllables.sl[2]),
            boxtype='info',
            text.lowscale='Fewer', text.highscale='More')
})
# Phonemes
output$plot.phonemes <- renderPlot({
  # handle multiple pronunciations by getting value for selected pronunciation
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  pron_nr <- match(input$manual.pron.phonemes, pron_summ)
  xsource_prx <- sprintf("cmu.pr%i_N_phonemes", pron_nr)
  str_in_x <- dat[[xsource_prx]][dat$string==input$string]
  dens.plot(x='cmu.pr1_N_phonemes', selected=input$check.phonemes,
            redline=str_in_x,
            shade=c(str_in_x + input$phonemes.sl[1], str_in_x + input$phonemes.sl[2]),
            boxtype='info',
            text.lowscale='Fewer', text.highscale='More')
})
# Phonological Neighbourhood
output$plot.pn <- renderPlot({
  xsource <- switch(input$pn.opt, 'pld20'='pld20', 'cn'='phon.coltheart.N')
  xsource_pr1 <- sprintf("cmu.pr1_%s", xsource)
  # handle multiple pronunciations by getting value for selected pronunciation
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  pron_nr <- match(input$manual.pron.pn, pron_summ)
  xsource <- switch(input$pn.opt, 'pld20'='pld20', 'cn'='phon.coltheart.N')
  xsource_prx <- sprintf("cmu.pr%i_%s", pron_nr, xsource)
  str_in_x <- dat[[xsource_prx]][dat$string==input$string]
  x_lowtext <- switch(input$pn.opt, 'pld20'='Larger Neighborhood', 'cn'='Smaller Neighborhood')
  x_hightext <- switch(input$pn.opt, 'pld20'='Smaller Neighborhood', 'cn'='Larger Neighborhood')
  dens.plot(x=xsource_pr1, selected=input$check.pn,
            redline=str_in_x,
            shade=c(str_in_x + input$pn.sl[1], str_in_x + input$pn.sl[2]),
            boxtype='info',
            text.lowscale=x_lowtext, text.highscale=x_hightext,
            log.transform = input$pn.log)
})
# Phonological Similarity
output$plot.ps <- renderPlot({
  # handle multiple pronunciations by getting value for selected pronunciation
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  pron_nr <- match(input$manual.pron.ps, pron_summ)
  xsource_prx <- sprintf("cmu.pr%i_pronun_1letter", pron_nr)
  str_in_pronun <- dat[[xsource_prx]][dat$string==input$string]
  dat_copy <- dat
  dat_copy$PS <- as.integer(switch(input$ps.opt,
                                   'ld'=vwr::levenshtein.distance(str_in_pronun, dat$cmu.pr1_pronun_1letter),
                                   'ldd'=vwr::levenshtein.damerau.distance(str_in_pronun, dat$pr1_cmu.pronun_1letter)))
  str_in_x <- dat_copy$PS[dat_copy$string==input$string]
  dens.plot(x='PS', df=dat_copy, selected=input$check.ps,
            redline=str_in_x,
            shade=c(str_in_x + input$ps.sl[1], str_in_x + input$ps.sl[2]),
            boxtype='info',
            text.lowscale='More Similar', text.highscale='Less Similar')
})
# Rhyme
output$plot.rhyme <- renderPlot({
  rhyme.plot(input$string, get_pron_nr(input$manual.pron.rhyme, input$string), selected=input$check.rhyme, dat)
})
# Number of Pronunciations
output$plot.prn <- renderPlot({
  str_in_x <- dat$cmu.alternatives[dat$string==input$string]
  dens.plot(x='cmu.alternatives', selected=input$check.prn,
            redline=str_in_x,
            shade=c(str_in_x + input$prn.sl[1], str_in_x + input$prn.sl[2]),
            boxtype='info',
            text.lowscale='Fewer', text.highscale='More')
})
# Familiarity
output$plot.fam <- renderPlot({
  xsource <- switch(input$fam.opt, 'gn'='gn.FAM', 'cp'='cp.FAM')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.fam,
            redline=str_in_x,
            shade=c(str_in_x + input$fam.sl[1], str_in_x + input$fam.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Familiar', text.highscale='More Familiar')
})
# Age of Acquisition
output$plot.aoa <- renderPlot({
  xsource <- switch(input$aoa.opt, 'gn'='gn.AOA', 'kuperman'='kuperman.AOA', 'bb'='bb.AOA')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.aoa,
            redline=str_in_x,
            shade=c(str_in_x + input$aoa.sl[1], str_in_x + input$aoa.sl[2]),
            boxtype = 'success',
            text.lowscale='Earlier', text.highscale='Later')
})
# Concreteness
output$plot.cnc <- renderPlot({
  xsource <- switch(input$cnc.opt, 'gn'='gn.CNC', 'brysbaert'='brysbaert.CNC')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.cnc,
            redline=str_in_x,
            shade=c(str_in_x + input$cnc.sl[1], str_in_x + input$cnc.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Concrete', text.highscale='More Concrete')
})
# Imageability
output$plot.imag <- renderPlot({
  xsource <- switch(input$imag.opt, 'gn'='gn.IMAG', 'cp'='cp.IMAG')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.imag,
            redline=str_in_x,
            shade=c(str_in_x + input$imag.sl[1], str_in_x + input$imag.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Imageable', text.highscale='More Imageable')
})
# Arousal
output$plot.arou <- renderPlot({
  xsource <- switch(input$arou.opt, 'gn'='gn.AROU', 'warriner'='warriner.AROU')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.arou,
            redline=str_in_x,
            shade=c(str_in_x + input$arou.sl[1], str_in_x + input$arou.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Arousing', text.highscale='More Arousing')
})
# Valence
output$plot.val <- renderPlot({
  xsource <- switch(input$val.opt, 'gn'='gn.VAL', 'warriner'='warriner.VAL')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.val,
            redline=str_in_x,
            shade=c(str_in_x + input$val.sl[1], str_in_x + input$val.sl[2]),
            boxtype = 'success',
            text.lowscale='More Negative', text.highscale='More Positive')
})
# Dominance
output$plot.dom <- renderPlot({
  xsource <- switch(input$dom.opt, 'gn'='gn.DOM', 'warriner'='warriner.DOM')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.dom,
            redline=str_in_x,
            shade=c(str_in_x + input$dom.sl[1], str_in_x + input$dom.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Dominant', text.highscale='More Dominant')
})
# Size
output$plot.size <- renderPlot({
  xsource <- switch(input$size.opt, 'gn'='gn.SIZE')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.size,
            redline=str_in_x,
            shade=c(str_in_x + input$size.sl[1], str_in_x + input$size.sl[2]),
            boxtype = 'success',
            text.lowscale='Smaller', text.highscale='Larger')
})
# Gender
output$plot.gen <- renderPlot({
  xsource <- switch(input$gen.opt, 'gn'='gn.GEND')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.gen,
            redline=str_in_x,
            shade=c(str_in_x + input$gen.sl[1], str_in_x + input$gen.sl[2]),
            boxtype = 'success',
            text.lowscale='More Feminine', text.highscale='More Masculine')
})
# Humour
output$plot.hum <- renderPlot({
  xsource <- switch(input$hum.opt, 'eh'='eh.HUM')
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.hum,
            redline=str_in_x,
            shade=c(str_in_x + input$hum.sl[1], str_in_x + input$hum.sl[2]),
            boxtype = 'success',
            text.lowscale='Less Funny', text.highscale='More Funny')
})
# Response Time
output$plot.rt <- renderPlot({
  xsource <- if(input$rt.zscore){
    switch(input$rt.opt, 'blp'='blp.rt.zscore', 'elp'='elp.rt.zscore')
  } else {
    switch(input$rt.opt, 'blp'='blp.rt', 'elp'='elp.rt')
  }
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.rt,
            redline=str_in_x,
            shade=c(str_in_x + input$rt.sl[1], str_in_x + input$rt.sl[2]),
            boxtype = 'danger',
            text.lowscale='Faster', text.highscale='Slower')
})
# Accuracy
output$plot.acc <- renderPlot({
  xsource <- if(input$acc.zscore){
    switch(input$acc.opt, 'blp'='blp.accuracy.zscore', 'elp'='elp.accuracy.zscore')
  } else {
    switch(input$acc.opt, 'blp'='blp.accuracy', 'elp'='elp.accuracy')
  }
  str_in_x <- dat[[xsource]][dat$string==input$string]
  dens.plot(x=xsource, selected=input$check.acc,
            redline=str_in_x,
            shade=c(str_in_x + input$acc.sl[1], str_in_x + input$acc.sl[2]),
            boxtype = 'danger',
            text.lowscale='Less Accurate', text.highscale='More Accurate')
})