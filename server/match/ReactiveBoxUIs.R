# Reactive change to Part of Speech box for manual PoS definition
output$manual.pos.choice <- renderUI({
  PoS.summ <- dat %>%
    group_by(switch(input$pos.opt,
                    'suk'=subtlex_uk.DomPoS,
                    'bnc_w'=bnc.wDomPoS,
                    'bnc_s'=bnc.sDomPoS,
                    'elp'=elp.DomPoS)) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    na.omit()
  if(input$check.manual.pos){
    selectInput('manual.pos', NULL, unlist(PoS.summ[1], use.names=F), width='100%')
  } else {
    NULL
  }
})

# Reactive change to Part of Speech box for PoS selection when generating stimuli
output$manual.pos.choice_gen <- renderUI({
  PoS.summ <- dat %>%
    group_by(switch(input$pos.opt_gen,
                    'suk'=subtlex_uk.DomPoS,
                    'bnc_w'=bnc.wDomPoS,
                    'bnc_s'=bnc.sDomPoS,
                    'elp'=elp.DomPoS)) %>%
    summarise(n=n()) %>%
    arrange(desc(n)) %>%
    na.omit()
  checkboxGroupInput('manual.pos_gen', NULL, unlist(PoS.summ[1], use.names=F), width='100%')
})

# Reactive change to Phonological boxes for manual pronunciation definition
output$manual.pron.syllables.choice <- renderUI ({
  if (input$syllables.opt=='cmu') {
    pron_summ <- get_pronunciations(input$string, df = dat) %>%
      unname() %>%
      lapply(arpabet_convert, to="two", sep='-') %>%
      unlist(use.names = F)
    selectInput('manual.pron.syllables', sprintf("Select Pronuciation (%i detected)", length(pron_summ)), pron_summ, width='100%')
  } else {
    NULL
  }
})
output$manual.pron.phonemes.choice <- renderUI ({
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  selectInput('manual.pron.phonemes', sprintf("Select Pronuciation (%i detected)", length(pron_summ)), pron_summ, width='100%')
})
output$manual.pron.pn.choice <- renderUI ({
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  selectInput('manual.pron.pn', sprintf("Select Pronuciation (%i detected)", length(pron_summ)), pron_summ, width='100%')
})
output$manual.pron.ps.choice <- renderUI ({
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  selectInput('manual.pron.ps', sprintf("Select Pronuciation (%i detected)", length(pron_summ)), pron_summ, width='100%')
})
output$manual.pron.rhyme.choice <- renderUI ({
  pron_summ <- get_pronunciations(input$string, df = dat) %>%
    unname() %>%
    lapply(arpabet_convert, to="two", sep='-') %>%
    unlist(use.names = F)
  selectInput('manual.pron.rhyme', sprintf("Select Pronuciation (%i detected)", length(pron_summ)), pron_summ, width='100%')
})