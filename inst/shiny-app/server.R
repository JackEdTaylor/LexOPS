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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # initialise the dataframe
    source("server/initialise_data.R", local = TRUE)

    # create the boxes on the generate tab
    source("server/generate/controlfor_boxes.R", local = TRUE)
    source("server/generate/filterby_boxes.R", local = TRUE)
    source("server/generate/splitby_boxes.R", local = TRUE)

    # conditional UIs for the options page
    source("server/generate/options.R", local = TRUE)

    # generate a review of the stimuli
    source("server/generate/review.R", local = TRUE)

    # codify selected options
    source("server/generate/codify.R", local = TRUE)

    # generate the stimuli
    source("server/generate/generate.R", local = TRUE)

    # create the boxes on the generate tab
    source("server/match_word/filterby_boxes.R", local = TRUE)
    source("server/match_word/matchby_boxes.R", local = TRUE)

    # Get the match results
    source("server/match_word/match_word.R", local = TRUE)

    # fetch tab
    source("server/fetch.R", local = TRUE)

    # visualise tab
    source("server/visualise.R", local = TRUE)

    # custom variables tab
    source("server/custom_variables.R", local = TRUE)

    # preferences tab
    source("server/preferences.R", local = TRUE)

    # remove loading screen after first full iteration
    lexops_loadingdone()

})
