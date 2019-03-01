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

# simple functions for rounding decimals up or down
source("misc_functions/rounding.R", local=T)

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

# Define server logic
shinyServer(function(input, output) {
  
  # a reactive object with the lexops data in it
  lexopsReact <- reactive({
    res <- lexops
    # add uploaded variables (if any)
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
    # add "word" to database temporarily if unknown
    if (!input$matchstring %in% res$string) {
      res <- res %>%
        add_row(string = input$matchstring,
                Length = nchar(input$matchstring))
    }
    # calculate similarity measures for match tab (if any)
    tryCatch({
      if (matchboxes_N() >= 1) {
        for (i in 1:matchboxes_N()) {
          boxid <- boxid <- sprintf('matchbox_%i', i)
          boxv <- input[[sprintf('%s_vtype', boxid)]]
          boxopt <- input[[sprintf('%s.opt', boxid)]]
          # Orthographic Similarity
          if (boxv == "Orthographic Similarity") {
            column <- corpus_recode_columns(boxopt, boxv)
            if (boxopt == "ld") {
              res[[column]] <- vwr::levenshtein.distance(input$matchstring, res$string)
            }
            if (boxopt == "ldd") {
              res[[column]] <- vwr::levenshtein.damerau.distance(input$matchstring, res$string)
            }
          }
        }
      }
    },
    error = function(cond) {
      return(NULL)
    },
    warning=function(cond) {
      return(NULL)
    })
    
    res
  })
  
  visualise.opts <- reactive({
    names(lexopsReact())[!(names(lexopsReact()) %in% c('string'))]
  })
  
  # functions used in the generate tab to create the UIs
  source("server/generate/splitby_UIfunction.R", local=T)
  source("server/generate/controlfor_UIfunction.R", local=T)
  source("server/generate/filterby_UIfunction.R", local=T)
  
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
  
  # initial number of boxes on generate tab
  matchboxes_N <- reactiveVal(0)
  
  # Match tab UI
  source("server/match/match_UIfunction.R", local=T)
  
  # Match tab boxes
  source("server/match/match_boxes.R", local=T)
  
  # get matches
  source("server/match/match.R", local=T)
  
  # put matches in datatable & sorting options
  source("server/match/results_match.R", local=T)
  
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

