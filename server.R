library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)
library(vwr)

# IMPORT DATA
cat(sprintf('\nIMPORTING DATA...\n'))
dat <- readRDS('dat.rds')
cat(sprintf(' -DONE.\n'))

# Create visualisation dataframe
source("server/get_vis_dat.R", local=T)

# functions for visualising distributions in boxes
source("server/box_vis_functions.R", local=T)

# Define server logic
shinyServer(function(input, output) {
  
  # get matches
  source("server/match.R", local=T)
  
  # put matches in datatable
  output$results <- DT::renderDataTable({
    DT::datatable(resultsdata(), options=list(pageLength=25))
  })
  
  # For displaying number of results under word-entry textbox in sidebar
  output$nrow.results <- renderText({sprintf('%i results', nrow(resultsdata())-1)})
  
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
      selectInput('manual.pos', NULL, unlist(PoS.summ[1], use.names=F), width=200)
    } else {
      NULL
    }
  })
  
  # Reactive change to Phonological boxes for manual pronunciation definition
  output$manual.pron.pn.choice <- renderUI ({
    pron_summ <- dat %>%
      filter(string==input$string) %>%
      select('cmu.pronun_1letter', 'cmu.pronun_1letter_alt1', 'cmu.pronun_1letter_alt2', 'cmu.pronun_1letter_alt3') %>%
      select_if(~sum(!is.na(.)) > 0) %>%
      unname()
    if (length(pron_summ) > 1) {
      selectInput('manual.pron.pn', sprintf("Select Pronuciation (%i detected) - NOT YET IMPLEMENTED", length(pron_summ)), pron_summ, width=200)
    } else {
      NULL
    }
  })
  
  
  # Density/Histogram plots in boxes
  source("server/update_box_vis.R", local=T)
  
  # Info page download button
  output$wordmatchmaker.csv <- downloadHandler(
    filename = 'wordmatchmaker.csv',
    content = function(file) {
      write.csv(vis.dat, file, row.names = FALSE)
    }
  )
  
  source("server/update_visualisation.R", local=T)
  
}

)

