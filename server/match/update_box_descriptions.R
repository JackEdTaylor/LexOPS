## GENERATE

# Frequency
output$descr.frequency_gen <- renderText({
  if (input$check.frequency_gen) {
    t <- "Will filter by"
    # values information
    if (input$frequency.log_gen) {
      freqval.name <- 'Zipf'
      t <- sprintf('%s log-transformed frequency per million (%s)', t, freqval.name)
    } else {
      freqval.name <- 'fpmw'
      t <- sprintf('%s raw frequency per million (%s)', t, freqval.name)
    }
    # averaging information
    if (length(input$frequency.opt_gen)>1) {
      t <- sprintf('%s, averaged across %i corpora', t, length(input$frequency.opt_gen))
    } else {
      t <- sprintf("%s, using only one ('%s') corpus", t, input$frequency.opt_gen[1])
    }
    # tolerance information
    t <- sprintf('%s. Will find words with %s values between %.2f and %.2f', t, freqval.name, abs(input$frequency.sl_gen[1]), input$frequency.sl_gen[2])
    # handle errors
    if(length(input$frequency.opt_gen)<1){
      t <- 'WARNING: No corpora selected!?'
    }
  } else {
    t <- "Will not filter by frequency"
  }
  sprintf('%s.', t)  # return resulting text with period at end of sentence
})

## MATCH

# Length
output$descr.length <- renderText({
  if(input$check.length) {
    if(input$length.sl[1]==0 & input$length.sl[2]==0){
      "Will match by length exactly."
    } else {
      if(input$length.sl[1]!=0 & input$length.sl[2]==0){
        sprintf('Will match by length, but allow words with %i fewer characters.', abs(input$length.sl[1]))
      } else if(input$length.sl[1]==0 & input$length.sl[2]!=0){
        sprintf('Will match by length, but allow words with %i more characters.', input$length.sl[2])
      } else {
        sprintf('Will match by length, but allow words with %i fewer or %i more characters.', abs(input$length.sl[1]), input$length.sl[2])
      }
    }
  } else{
    "Will not match by length."
  }
})

# Part of speech
output$descr.partofspeech <- renderText({
  dom.str <- if(input$check.manual.pos) {''} else {' dominant'}
  according.str <- sprintf(' according to %s', switch(input$pos.opt, 'suk'='SUBTLEX-UK', 'bnc_w'='the BNC (written)', 'bnc_s'='the BNC (spoken)', 'elp'='the English Lexicon Project (ELP)'))
  man.str <- if(input$check.manual.pos) {' manually'} else {''}
  
  selected.pos <- if(input$check.manual.pos){
    input$manual.pos
  } else {
    switch(input$pos.opt,
           'suk'=dat$subtlex_uk.DomPoS[dat$string==input$string],
           'bnc_w'=dat$bnc.wDomPoS[dat$string==input$string],
           'bnc_s'=dat$bnc.sDomPoS[dat$string==input$string],
           'elp'=dat$elp.DomPoS[dat$string==input$string])
  }
  
  if (input$check.partofspeech) {
    sprintf('Will match by%s part of speech%s ("%s")%s.', dom.str, man.str, selected.pos, according.str)
  } else {
    'Will not match by part of speech.'
  }
})

# Frequency
output$descr.frequency <- renderText({
  if (input$check.frequency) {
    t <- "Will match by"
    # values information
    if (input$frequency.log) {
      freqval.name <- 'Zipf'
      t <- sprintf('%s log-transformed frequency per million (%s)', t, freqval.name)
    } else {
      freqval.name <- 'fpmw'
      t <- sprintf('%s raw frequency per million (%s)', t, freqval.name)
    }
    # averaging information
    if (length(input$frequency.opt)>1) {
      t <- sprintf('%s, averaged across %i corpora', t, length(input$frequency.opt))
    } else {
      t <- sprintf("%s, using only one ('%s') corpus", t, input$frequency.opt[1])
    }
    # tolerance information
    if (input$frequency.sl[1]!=0 & input$frequency.sl[2]!=0) {
      t <- sprintf('%s. Will accept %s values %.2f smaller or %.2f greater than the target word', t, freqval.name, abs(input$frequency.sl[1]), input$frequency.sl[2])
    } else if (input$frequency.sl[1]!=0 & input$frequency.sl[2]==0) {
      t <- sprintf('%s. Will accept %s values %.2f smaller than the target word', t, freqval.name, abs(input$frequency.sl[1]))
    } else if (input$frequency.sl[1]==0 & input$frequency.sl[2]!=0) {
      t <- sprintf('%s. Will accept %s values %.2f greater than the target word', t, freqval.name, input$frequency.sl[2])
    } else if (input$frequency.sl[1]==0 & input$frequency.sl[2]==0) {
      t <- sprintf('%s. Will match by %s values exactly (not recommended)', t, freqval.name)
    }
    # handle errors
    if(length(input$frequency.opt)<1){
      t <- 'WARNING: No corpora selected!?'
    }
  } else {
    t <- "Will not match by frequency"
  }
  sprintf('%s.', t)  # return resulting text with period at end of sentence
})

# Bigram Frequency
output$descr.bgfreq <- renderText({
  if (input$check.bgfreq) {
    t <- "Will match by mean bigram frequency"
    # averaging information
    if (length(input$frequency.opt)>1) {
      t <- sprintf('%s, averaged across %i corpora', t, length(input$bgfreq.opt))
    } else {
      t <- sprintf("%s, using only one ('%s') corpus", t, input$bgfreq.opt[1])
    }
    # tolerance information
    if (input$bgfreq.sl[1]!=0 & input$bgfreq.sl[2]!=0) {
      t <- sprintf('%s. Will accept values %.4f smaller or %.4f greater than the target word', t, abs(input$bgfreq.sl[1]), input$bgfreq.sl[2])
    } else if (input$bgfreq.sl[1]!=0 & input$bgfreq.sl[2]==0) {
      t <- sprintf('%s. Will accept values %.4f smaller than the target word', t, abs(input$bgfreq.sl[1]))
    } else if (input$bgfreq.sl[1]==0 & input$bgfreq.sl[2]!=0) {
      t <- sprintf('%s. Will accept values %.4f greater than the target word', t, input$bgfreq.sl[2])
    } else if (input$bgfreq.sl[1]==0 & input$bgfreq.sl[2]==0) {
      t <- sprintf('%s. Will match by values exactly (not recommended)', t)
    }
    # handle errors
    if(length(input$bgfreq.opt)<1){
      t <- 'WARNING: No corpora selected!?'
    }
  } else {
    t <- "Will not match by mean bigram frequency"
  }
  sprintf('%s.', t)  # return resulting text with period at end of sentence
})

# Orthographic Neighborhood
output$descr.on <- renderText({
  if (input$check.on) {
    ontype <- switch(input$on.opt, 'old20' = 'Orthographic Levenshtein Distance 20 (OLD20)', 'cn' = "Coltheart's N")
    onlog <- if(input$on.log){'log-transformed'}else{'raw'}
    t <- sprintf('Will match by ON, measured in %s %s.', onlog, ontype)
    if(input$on.sl[1]==0 & input$on.sl[2]==0){
      boundary <- sprintf(' Will match by values exactly (not recommended).')
    } else if(input$on.sl[1]==0 & input$on.sl[2]!=0){
      boundary <- sprintf(' Will allow values %.2f greater than the target word.', input$on.sl[2])
    } else if(input$on.sl[1]!=0 & input$on.sl[2]==0){
      boundary <- sprintf(' Will allow values %.2f smaller than the target word.', abs(input$on.sl[1]))
    } else if(input$on.sl[1]!=0 & input$on.sl[2]!=0){
      boundary <- sprintf(' Will allow values %.2f smaller or %.2f greater than the target word.', abs(input$on.sl[1]), input$on.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by ON'
  }
  t
})

# Orthographic Similarity
output$descr.os <- renderText({
  if (input$check.os){
    ostype <- switch(input$os.opt, 'ld' = 'Levenshtein Distance (LD)', 'ldd' = 'Levenshtein-Damerau Distance (LDD)')
    t <- sprintf('Will match by OS to the target word, measured in %s.', ostype)
    if(input$os.sl[1]==0 & input$os.sl[2]==0){
      t <- sprintf('WARNING: The only entry with an %s value of 0 will be the target word itself!', toupper(input$os.opt))
    } else {
      t <- sprintf('%s Will allow %s values from %i to %i.', t, toupper(input$os.opt), input$os.sl[1], input$os.sl[2])
    }
  } else {
    t <- 'Will not match by OS to the target word'
  }
  t
})

# Syllables
output$descr.syllables <- renderText({
  if(input$check.syllables) {
    if(input$syllables.sl[1]==0 & input$syllables.sl[2]==0){
      t <- "Will match by number of syllables exactly"
    } else {
      if(input$syllables.sl[1]!=0 & input$syllables.sl[2]==0){
        t <- sprintf('Will match by number of syllables, but allow words with %i fewer syllables', abs(input$syllables.sl[1]))
      } else if(input$syllables.sl[1]==0 & input$syllables.sl[2]!=0){
        t <- sprintf('Will match by number of syllables, but allow words with %i more syllables', input$syllables.sl[2])
      } else {
        t <- sprintf('Will match by number of syllables, but allow words with %i fewer or %i more syllables', abs(input$syllables.sl[1]), input$syllables.sl[2])
      }
    }
    sprintf('%s, according to the %s', t, switch(input$syllables.opt, 'cmu'='CMU Pronouncing Dictionary', 'mp'='Moby Project'))
  } else{
    "Will not match by number of syllables."
  }
})

# Phonemes
output$descr.phonemes <- renderText({
  if(input$check.phonemes) {
    if(input$phonemes.sl[1]==0 & input$phonemes.sl[2]==0){
      "Will match by number of phonemes exactly."
    } else {
      if(input$phonemes.sl[1]!=0 & input$phonemes.sl[2]==0){
        sprintf('Will match by number of phonemes, but allow words with %i fewer phonemes', abs(input$phonemes.sl[1]))
      } else if(input$phonemes.sl[1]==0 & input$phonemes.sl[2]!=0){
        sprintf('Will match by number of phonemes, but allow words with %i more phonemes', input$phonemes.sl[2])
      } else {
        sprintf('Will match by number of phonemes, but allow words with %i fewer or %i more phonemes.', abs(input$phonemes.sl[1]), input$phonemes.sl[2])
      }
    }
  } else{
    "Will not match by number of phonemes."
  }
})

# Rhyme
output$descr.rhyme <- renderText({
  # get rhyme sound
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  pron_nr <- match(input$manual.pron.phonemes, pron_summ)
  xsource_prx <- sprintf("cmu.pr%i_rhymesound", pron_nr)
  str_in_rhymesound <- dat[[xsource_prx]][dat$string==input$string]
  if (input$check.rhyme) {
    t <- sprintf('Will match by Rhyme ("%s").', gsub("_", "-", str_in_rhymesound))
  } else {
    t <- 'Will not match by Rhyme.'
  }
  t
})

# Phonological Neighborhood
output$descr.pn <- renderText({
  if (input$check.pn) {
    pntype <- switch(input$pn.opt, 'pld20' = 'Phonological Levenshtein Distance 20 (PLD20)', 'cn' = "Coltheart's N")
    pnlog <- if(input$pn.log){'log-transformed'}else{'raw'}
    t <- sprintf('Will match by PN, measured in %s %s.', pnlog, pntype)
    if(input$pn.sl[1]==0 & input$pn.sl[2]==0){
      boundary <- sprintf(' Will match by values exactly (not recommended).')
    } else if(input$pn.sl[1]==0 & input$pn.sl[2]!=0){
      boundary <- sprintf(' Will allow values %.2f greater than the target word.', input$pn.sl[2])
    } else if(input$pn.sl[1]!=0 & input$pn.sl[2]==0){
      boundary <- sprintf(' Will allow values %.2f smaller than the target word.', abs(input$pn.sl[1]))
    } else if(input$pn.sl[1]!=0 & input$pn.sl[2]!=0){
      boundary <- sprintf(' Will allow values %.2f smaller or %.2f greater than the target pronunciation.', abs(input$pn.sl[1]), input$pn.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by PN'
  }
  t
})

# Phonological Similarity
output$descr.ps <- renderText({
  if (input$check.ps){
    pstype <- switch(input$ps.opt, 'ld' = 'Levenshtein Distance (LD)', 'ldd' = 'Levenshtein-Damerau Distance (LDD)')
    t <- sprintf('Will match by PS to the target word, measured in %s.', pstype)
    if(input$ps.sl[1]==0 & input$ps.sl[2]==0){
      t <- sprintf('WARNING: The only entry with an %s value of 0 will be the target word itself!', toupper(input$os.opt))
    } else {
      t <- sprintf('%s Will allow %s values from %i to %i.', t, toupper(input$ps.opt), input$ps.sl[1], input$ps.sl[2])
    }
  } else {
    t <- 'Will not match by PS to the target word'
  }
  t
})

# Number of Pronunciations
output$descr.prn <- renderText({
  if(input$check.prn) {
    if(input$prn.sl[1]==0 & input$prn.sl[2]==0){
      "Will match by number of pronunciations exactly."
    } else {
      if(input$prn.sl[1]!=0 & input$prn.sl[2]==0){
        sprintf('Will match by number of pronunciations, but allow words with %i fewer phonemes', abs(input$prn.sl[1]))
      } else if(input$prn.sl[1]==0 & input$prn.sl[2]!=0){
        sprintf('Will match by number of pronunciations, but allow words with %i more phonemes', input$prn.sl[2])
      } else {
        sprintf('Will match by number of pronunciations, but allow words with %i fewer or %i more phonemes.', abs(input$prn.sl[1]), input$prn.sl[2])
      }
    }
  } else{
    "Will not match by number of pronunciations."
  }
})

# Familiarity
output$descr.fam <- renderText({
  box_descr_numeric("Familiarity", "ratings", input$fam.opt, input$fam.sl, input$check.fam)
})

# Age of Acquisition
output$descr.aoa <- renderText({
  aoavariabletype <- if (length(input$aoa.opt==1)) {
    if (input$aoa.opt=="bb") {
      "scores"
    } else {
      "ratings"
    }
  } else {
    if ("bb" %in% input$aoa.opt) {
      "ratings and scores"
    } else {
      "ratings"
    }
  }
  box_descr_numeric("Age of Acquisition", aoavariabletype, input$aoa.opt, input$aoa.sl, input$check.aoa)
})

# Concreteness
output$descr.cnc <- renderText({
  box_descr_numeric("Concreteness", "ratings", input$cnc.opt, input$cnc.sl, input$check.cnc)
})

# Arousal
output$descr.arou <- renderText({
  box_descr_numeric("Arousal", "ratings", input$arou.opt, input$arou.sl, input$check.arou)
})

# Valence
output$descr.val <- renderText({
  box_descr_numeric("Valence", "ratings", input$val.opt, input$val.sl, input$check.val)
})

# Dominance
output$descr.dom <- renderText({
  box_descr_numeric("Dominance", "ratings", input$dom.opt, input$dom.sl, input$check.dom)
})

# Imageability
output$descr.imag <- renderText({
  box_descr_numeric("Imageability", "ratings", input$imag.opt, input$imag.sl, input$check.imag)
})

# Semantic Size
output$descr.size <- renderText({
  box_descr_numeric("Semantic Size", "ratings", input$size.opt, input$size.sl, input$check.size)
})

# Semantic Gender
output$descr.gen <- renderText({
  box_descr_numeric("Semantic Gender", "ratings", input$gen.opt, input$gen.sl, input$check.gen)
})

# Humour
output$descr.hum <- renderText({
  box_descr_numeric("Humour", "ratings", input$hum.opt, input$hum.sl, input$check.hum)
})

# Response Time
output$descr.rt <- renderText({
  if (input$check.rt) {
    rtsource <- switch(input$rt.opt, 'blp'='the British Lexicon Project', 'elp'='the English Lexicon Project')
    zscoretext <- if (input$rt.zscore) {' Z-Scored'} else {' raw'}
    t <- sprintf('Will match by%s Lexical Decision Response Time according to %s.', zscoretext, rtsource)
    if(input$rt.sl[1]==0 & input$rt.sl[2]==0) {
      boundary <- sprintf(' Will match by RT exactly (not recommended).')
    } else if(input$rt.sl[1]==0 & input$rt.sl[2]!=0) {
      boundary <- sprintf(' Will allow RT %.2f greater than the target word.', input$rt.sl[2])
    } else if(input$rt.sl[1]!=0 & input$rt.sl[2]==0) {
      boundary <- sprintf(' Will allow RT %.2f smaller than the target word.', abs(input$rt.sl[1]))
    } else if(input$rt.sl[1]!=0 & input$rt.sl[2]!=0) {
      boundary <- sprintf(' Will allow RT %.2f smaller or %.2f greater than the target word.', abs(input$rt.sl[1]), input$rt.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Lexical Decision Response Time.'
  }
  t
})

# Accuracy
output$descr.acc <- renderText({
  if (input$check.acc) {
    accsource <- switch(input$acc.opt, 'blp'='the British Lexicon Project', 'elp'='the English Lexicon Project')
    zscoretext <- if (input$acc.zscore) {' Z-Scored'} else {' raw'}
    t <- sprintf('Will match by%s Lexical Decision Accuracy according to %s.', zscoretext, accsource)
    if(input$acc.sl[1]==0 & input$acc.sl[2]==0) {
      boundary <- sprintf(' Will match by Accuracy exactly (not recommended).')
    } else if(input$acc.sl[1]==0 & input$acc.sl[2]!=0) {
      boundary <- sprintf(' Will allow Accuracy %.2f greater than the target word.', input$acc.sl[2])
    } else if(input$acc.sl[1]!=0 & input$acc.sl[2]==0) {
      boundary <- sprintf(' Will allow Accuracy %.2f smaller than the target word.', abs(input$acc.sl[1]))
    } else if(input$acc.sl[1]!=0 & input$acc.sl[2]!=0) {
      boundary <- sprintf(' Will allow Accuracy %.2f smaller or %.2f greater than the target word.', abs(input$acc.sl[1]), input$acc.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Lexical Decision Accuracy.'
  }
  t
})