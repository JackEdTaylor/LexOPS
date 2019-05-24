library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(plotly)
library(viridis)
library(DT)
library(colourpicker)
library(ggplot2)
library(ggwordcloud)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(stringr)
library(forcats)
library(vwr)
library(stringdist)
library(LexOPS)

lexops_loadingdone <- function() {
  hide(id = "loading_page", anim = TRUE, animType = "slide")
  show("main_content")
}

# IMPORT DATA
cat(sprintf('\nIMPORTING DATA...\n'))
lexops <- LexOPS::lexops
cat(sprintf(' -DONE.\n'))

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

  # get the lexopsReact() reactive df
  source("server/lexops_reactive_df.R", local=T)

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
      withProgress(message="Writing database to .csv file...", value=1, {
        write.csv(lexops, file, row.names = FALSE)
      })
    }
  )

  source("server/update_visualisation.R", local=T)

  # loading screen finish
  lexops_loadingdone()

}

)

