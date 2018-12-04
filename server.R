library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(plotly)
library(viridis)
library(ggwordcloud)
library(DT)
library(vwr)

lexops_loadingdone <- function() {
  hide(id = "loading_page", anim = TRUE, animType = "fade")    
  show("main_content")
}


# IMPORT DATA
cat(sprintf('\nIMPORTING DATA...\n'))
dat <- readRDS('dat.rds')
cat(sprintf(' -DONE.\n'))

# Create lexops dataframe and set options for visualisation
source("server/get_lexops_data.R", local=T)
lexopsraw <- lexops

# functions for visualising distributions in boxes
source("server/box_vis_functions.R", local=T)

# function for converting CMU phoneme representations between one and two-letter
source("misc_functions/arpabet_convert.R", local=T)

# function for getting a word's possible pronunciations
source("misc_functions/get_pronunciations.R", local=T)

# function for returning which alternate pronunciation for a string has been selected
source("misc_functions/get_pron_nr.R", local=T)

# functions used in matching
source("server/match/matcher_functions.R", local=T)

# Define server logic
shinyServer(function(input, output) {
  
  # get matches
  source("server/match/match.R", local=T)
  
  # put matches in datatable & sorting options
  source("server/match/results_match.R", local=T)
  
  # put matches in datatable
  output$match_results_dt <- DT::renderDataTable({
    DT::datatable(matchresults(), options=list(pageLength=25, scrollX=T))
  })
  
  # For displaying number of results under word-entry textbox in sidebar
  output$nrow.matchresults <- renderText({sprintf('%i results', nrow(matchresults())-1)})
  
  # Descriptions under boxes
  source("server/update_box_descriptions.R", local=T)
  
  # Reactive changes to matching sliders
  source("server/update_sliders.R", local=T)
  
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
  
  # Density/Histogram plots in boxes
  source("server/update_box_vis.R", local=T)
  
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
  
  # fetch tab
  source("server/fetch/fetcher.R", local=T)
  
  # Info page download button
  output$LexOPS.csv <- downloadHandler(
    filename = 'LexOPS.csv',
    content = function(file) {
      write.csv(lexopsraw, file, row.names = FALSE)
    }
  )
  
  source("server/update_visualisation.R", local=T)
  
  # loading screen finish
  lexops_loadingdone()
  
}

)

