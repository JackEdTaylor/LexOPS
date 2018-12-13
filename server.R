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
source("server/match/box_vis_functions.R", local=T)

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

# functions used in matching
source("server/match/matcher_functions.R", local=T)

# functions used to get the descriptions in boxes for matching
source("server/match/box_descriptions_functions.R", local=T)

# Define server logic
shinyServer(function(input, output) {
  
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

