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
      "Will match by number of syllables exactly."
    } else {
      if(input$syllables.sl[1]!=0 & input$syllables.sl[2]==0){
        sprintf('Will match by number of syllables, but allow words with %i fewer syllables', abs(input$syllables.sl[1]))
      } else if(input$syllables.sl[1]==0 & input$syllables.sl[2]!=0){
        sprintf('Will match by number of syllables, but allow words with %i more syllables', input$syllables.sl[2])
      } else {
        sprintf('Will match by number of syllables, but allow words with %i fewer or %i more syllables.', abs(input$syllables.sl[1]), input$syllables.sl[2])
      }
    }
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
      boundary <- sprintf(' Will allow values %.2f smaller or %.2f greater than the target word.', abs(input$pn.sl[1]), input$pn.sl[2])
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

# Familiarity
output$descr.fam <- renderText({
  if (input$check.fam) {
    famsource <- switch(input$fam.opt, 'cp' = 'Clark and Paivio (2004)', 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Familiarity ratings (from 1 to 7) according to %s.', famsource)
    if(input$fam.sl[1]==0 & input$fam.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$fam.sl[1]==0 & input$fam.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$fam.sl[2])
    } else if(input$fam.sl[1]!=0 & input$fam.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$fam.sl[1]))
    } else if(input$fam.sl[1]!=0 & input$fam.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$fam.sl[1]), input$fam.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Familiarity.'
  }
  t
})

# Age of Acquisition
output$descr.aoa <- renderText({
  if (input$check.aoa) {
    aoasource <- switch(input$aoa.opt, 'kuperman' = 'Kuperman et al. (2012)', 'gn' = 'the Glasgow Norms')
    aoatype <- switch(input$aoa.opt, 'kuperman'='estimates', 'gn'='ratings')
    aoarange <- switch(input$aoa.opt, 'kuperman'='from 1 to 25 years of age', 'gn'='from 1 to 7')
    t <- sprintf('Will match by Age of Acquisition %s (%s) according to %s.', aoatype, aoarange, aoasource)
    if(input$aoa.sl[1]==0 & input$aoa.sl[2]==0){
      boundary <- sprintf(' Will match by %s exactly (not recommended).', aoatype)
    } else if(input$aoa.sl[1]==0 & input$aoa.sl[2]!=0){
      boundary <- sprintf(' Will allow %s %.2f greater than the target word.', aoatype, input$aoa.sl[2])
    } else if(input$aoa.sl[1]!=0 & input$aoa.sl[2]==0){
      boundary <- sprintf(' Will allow %s %.2f smaller than the target word.', aoatype, abs(input$aoa.sl[1]))
    } else if(input$aoa.sl[1]!=0 & input$aoa.sl[2]!=0){
      boundary <- sprintf(' Will allow %s %.2f smaller or %.2f greater than the target word.', aoatype, abs(input$aoa.sl[1]), input$aoa.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Age of Acquisition.'
  }
  t
})

# Concreteness
output$descr.cnc <- renderText({
  if (input$check.cnc) {
    cncsource <- switch(input$cnc.opt, 'brysbaert' = 'Brysbaert et al. (2014)', 'gn' = 'the Glasgow Norms')
    cncrange <- switch(input$cnc.opt, 'brysbaert' = 'from 1 to 5', 'gn' = 'from 1 to 7')
    t <- sprintf('Will match by Concreteness ratings (%s) according to %s.', cncrange, cncsource)
    if(input$cnc.sl[1]==0 & input$cnc.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$cnc.sl[1]==0 & input$cnc.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$cnc.sl[2])
    } else if(input$cnc.sl[1]!=0 & input$cnc.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$cnc.sl[1]))
    } else if(input$cnc.sl[1]!=0 & input$cnc.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$cnc.sl[1]), input$cnc.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Concreteness.'
  }
  t
})

# Arousal
output$descr.arou <- renderText({
  if (input$check.arou) {
    arousource <- switch(input$arou.opt, 'warriner' = 'Warriner et al. (2013)', 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Arousal ratings (from 1 to 9) according to %s.', arousource)
    if(input$arou.sl[1]==0 & input$arou.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$arou.sl[1]==0 & input$arou.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$arou.sl[2])
    } else if(input$arou.sl[1]!=0 & input$arou.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$arou.sl[1]))
    } else if(input$arou.sl[1]!=0 & input$arou.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$arou.sl[1]), input$arou.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Arousal.'
  }
  t
})

# Valence
output$descr.val <- renderText({
  if (input$check.val) {
    valsource <- switch(input$val.opt, 'warriner' = 'Warriner et al. (2013)', 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Valence ratings (from 1 to 9) according to %s.', valsource)
    if(input$val.sl[1]==0 & input$val.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$val.sl[1]==0 & input$val.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$val.sl[2])
    } else if(input$val.sl[1]!=0 & input$val.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$val.sl[1]))
    } else if(input$val.sl[1]!=0 & input$val.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$val.sl[1]), input$val.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Valence.'
  }
  t
})

# Dominance
output$descr.dom <- renderText({
  if (input$check.dom) {
    domsource <- switch(input$dom.opt, 'warriner' = 'Warriner et al. (2013)', 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Dominance ratings (from 1 to 9) according to %s.', domsource)
    if(input$dom.sl[1]==0 & input$dom.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$dom.sl[1]==0 & input$dom.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$dom.sl[2])
    } else if(input$dom.sl[1]!=0 & input$dom.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$dom.sl[1]))
    } else if(input$dom.sl[1]!=0 & input$dom.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$dom.sl[1]), input$dom.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Dominance.'
  }
  t
})

# Imageability
output$descr.imag <- renderText({
  if (input$check.imag) {
    imagsource <- switch(input$imag.opt, 'cp' = 'Clark and Paivio (2004)', 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Imageability ratings (from 1 to 7) according to %s.', imagsource)
    if(input$imag.sl[1]==0 & input$imag.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$imag.sl[1]==0 & input$imag.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$imag.sl[2])
    } else if(input$imag.sl[1]!=0 & input$imag.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$imag.sl[1]))
    } else if(input$imag.sl[1]!=0 & input$imag.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$imag.sl[1]), input$imag.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Imageability'
  }
  t
})

# Semantic Size
output$descr.size <- renderText({
  if (input$check.size) {
    sizesource <- switch(input$size.opt, 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Semantic Size ratings (from 1 to 7) according to %s.', sizesource)
    if(input$imag.sl[1]==0 & input$imag.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$imag.sl[1]==0 & input$imag.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$imag.sl[2])
    } else if(input$imag.sl[1]!=0 & input$imag.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$imag.sl[1]))
    } else if(input$imag.sl[1]!=0 & input$imag.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$imag.sl[1]), input$imag.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Semantic Size.'
  }
  t
})

# Semantic Gender
output$descr.gen <- renderText({
  if (input$check.gen) {
    gensource <- switch(input$gen.opt, 'gn' = 'the Glasgow Norms')
    t <- sprintf('Will match by Semantic Gender ratings (from 1 to 7) according to %s.', gensource)
    if(input$gen.sl[1]==0 & input$gen.sl[2]==0){
      boundary <- sprintf(' Will match by ratings exactly (not recommended).')
    } else if(input$gen.sl[1]==0 & input$gen.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f greater than the target word.', input$gen.sl[2])
    } else if(input$gen.sl[1]!=0 & input$gen.sl[2]==0){
      boundary <- sprintf(' Will allow ratings %.2f smaller than the target word.', abs(input$gen.sl[1]))
    } else if(input$gen.sl[1]!=0 & input$gen.sl[2]!=0){
      boundary <- sprintf(' Will allow ratings %.2f smaller or %.2f greater than the target word.', abs(input$gen.sl[1]), input$gen.sl[2])
    }
    t <- sprintf('%s%s', t, boundary)
  } else {
    t <- 'Will not match by Semantic Gender.'
  }
  t
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