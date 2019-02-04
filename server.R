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

# Create lexops dataframe
source("server/get_lexops_data.R", local=T)
lexopsraw <- lexops

# set options for visualisation
source("server/get_viscats.R", local=T)

# functions for visualising distributions in boxes
source("misc_functions/box_vis_functions.R", local=T)

# function for converting CMU phoneme representations between one and two-letter
source("misc_functions/arpabet_convert.R", local=T)

# function for calculating Euclidean distance from target word for selected columns
source("misc_functions/get_euclidean_distance.R", local=T)

# function for calculating City-Block distance from target word for selected columns
source("misc_functions/get_cityblock_distance.R", local=T)

# function for getting a word's possible pronunciations
source("misc_functions/get_pronunciations.R", local=T)

# function for returning which alternate pronunciation for a string has been selected
source("misc_functions/get_pron_nr.R", local=T)

# functions for converting between short-hand and variable names or APA citations
source("misc_functions/corpus_recoders.R", local=T)

# functions used in matching
source("server/match/matcher_functions.R", local=T)

# functions used to get the descriptions in boxes for matching
source("server/match/box_descriptions_functions.R", local=T)

# functions used in the generate tab to create the UIs
source("server/generate/splitby_UIfunction.R", local=T)
source("server/generate/controlfor_UIfunction.R", local=T)
source("server/generate/filterby_UIfunction.R", local=T)

# Define server logic
shinyServer(function(input, output) {
  
  # a reactive object with the lexops data in it
  lexopsReact <- reactive({
    res <- lexops
    
    # add uploaded variables
    if (!is.null(input$cust.opts.inputfile)) {
      if (input$cust.opts.all=="all") {
        selcols <- colnames(cust_df_raw())
        selcols <- selcols[selcols!=input$cust.opts.column]
      } else {
        selcols <- colnames(select(cust_df_raw(), input$cust.opts))
        selcols <- selcols[selcols!=input$cust.opts.column]
      }
      
      targstringcolname <- input$cust.opts.column
      
      inputfile <- cust_df_raw() %>%
        rename_at(vars(selcols), ~ sprintf("custom.%s", selcols)) %>%
        rename(string = targstringcolname)
      
      res <- res %>%
        full_join(select(inputfile, c(sprintf("custom.%s", selcols), "string")), by="string")
    }
    
    res
  })
  
  visualise.opts <- reactive({
    names(lexopsReact())[!(names(lexopsReact()) %in% c('string'))]
  })
  
  # initial numbers of boxes on generate tab
  gen_splitby_boxes_N <- reactiveVal(0)
  gen_controlfor_boxes_N <- reactiveVal(0)
  gen_filterby_boxes_N <- reactiveVal(0)
  
  # Generate tab boxes
  source("server/generate/splitby_boxes.R", local=T)
  source("server/generate/controlfor_boxes.R", local=T)
  source("server/generate/filterby_boxes.R", local=T)
  
  # Get the generated stimuli
  source("server/generate/generate.R", local=T)
  
  # Generate tab results
  source("server/generate/results_generate.R", local=T)
  
  # reactive changes to box UIs in match tab
  source("server/match/ReactiveBoxUIs.R", local=T)
  
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
  source("server/match/update_box_descriptions.R", local=T)
  
  # Reactive changes to matching sliders
  source("server/match/update_sliders.R", local=T)
  
  # Density/Histogram plots in boxes
  source("server/match/update_box_vis.R", local=T)
  
  # fetch tab
  source("server/fetch.R", local=T)
  
  # custom variables tab
  source("server/custom_variables.R", local=T)
  
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

