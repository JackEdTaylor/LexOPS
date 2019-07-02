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

tagList(
    useShinyjs(),
    tags$head(
        tags$link(href = "style.css", rel = "stylesheet")
    ),
    div(
        id = "loading_page",
        img(src = "lexopslogo_white.png", class = "center-fit"),
        tags$br(), tags$br(),
        icon("spinner", class = "fa-spin")
    ),

    hidden(
        div(
            id = "main_content",

            dashboardPage(title="LexOPS",
                          skin="black",
                          dashboardHeader(title=tags$a(
                              href="javascript:history.go(0)",
                              tags$img(src="lexopslogo_black_textonly.png",height="30px")),
                              titleWidth=170,
                              tags$li(tags$p(as.character(packageVersion("LexOPS")), style = "font-size: 16px;"), class="dropdown")),

                          dashboardSidebar(width=170,
                                           sidebarMenu(
                                               style = "position: fixed; overflow: visible;",  # Stationary sidebar while scrolling
                                               menuItem("Home", tabName = "home", icon=icon("home")),
                                               tags$br(),
                                               menuItem("Generate", tabName = "generate", icon=icon("cogs")),
                                               menuItem("Match Word", tabName = "match_word", icon=icon("balance-scale")),
                                               menuItem("Fetch", tabName="fetch", icon=icon("file-import")),
                                               menuItem("Visualise", tabName="visualise", icon=icon("chart-bar")),
                                               tags$br(),
                                               menuItem("Custom Variables", tabName="custom_variables", icon=icon("plus")),
                                               menuItem("Preferences", tabName="preferences", icon=icon("wrench")),
                                               menuItem("Info", tabName="info", icon=icon("info-circle"))
                                           )),

                          dashboardBody(
                              tabItems(
                                  # Home
                                  tabItem(tabName = "home",
                                          tags$h1("Welcome to LexOPS!"),
                                          tags$div(HTML(paste0("This shiny app is a front-end to the ", tags$a(href = "https://github.com/JackEdTaylor/LexOPS", "LexOPS R package"), ". LexOPS allows you to generate suitably controlled word stimuli for any possible factorial design."))),
                                          tags$br(),
                                          tags$p("The tabs in the sidebar on the left-hand side provide different options for generating stimuli and exploring data:"),
                                          tags$h3(icon("cogs"), "Generate"),
                                          tags$p("The Generate tab provides LexOPS' main functionality, generating stimuli for any possible user-specified factorial design. Here, you specify independent variables (\"splits\"), controls (variables that should be controlled for), and filters (defining a subset of the LexOPS dataset which should be used to generate the stimuli). You can then View the generated stimuli in wide or long format, and download the stimuli list as a .csv file. You can also review the generated stimuli and algorithm's performance with informative graphics. This section can also generate R code to reproduce generated stimuli."),
                                          tags$h3(icon("balance-scale"), "Match Word"),
                                          tags$p("The Match Word tab can suggest matches for specific words, matched in terms of selected variables. This is useful if you need to pick a suitable match manually from a list of candidates (e.g. suitable match for a word in a sentence). Again, the results can be downloaded as a .csv file."),
                                          tags$h3(icon("file-import"), "Fetch"),
                                          tags$p("The Fetch tab provides an easy way to get the values of variables in the LexOPS dataset associated with an existing stimuli list. The list of stimuli can be either uploaded (as a .csv, .tsv, .xls, or .xlsx file) or copied and pasted into the app. You can then view or download a dataframe with the values associated with your stimuli."),
                                          tags$h3(icon("chart-bar"), "Visualise"),
                                          tags$p("The Visualise tab allows you to plot relationships between numeric and categorical variables. You can specify variables to be plotted on x, y, and z axes, as well as a variable by which points should be coloured. The Visualise tab is integrated with other tabs, allowing you to plot, for instance, differences between conditions produced in the Generate tab."),
                                          tags$h3(icon("plus"), "Custom Variables"),
                                          tags$p("Here you can upload your own variables, for English words or words from another language. These variables can then be used in the other tabs. This allows you to integrate your own variables into stimulus generation, or generate stimulus lists for non-English stimuli."),
                                          tags$h3(icon("wrench"), "Preferences"),
                                          tags$p("Alter default preferences for the app, such as forcing sliders to be displayed as numeric inputs, or setting a random seed for stimulus generation.")
                                  ),
                                  # Generate
                                  tabItem(tabName = "generate",

                                          tabsetPanel(
                                              type = "tabs",
                                              tabPanel("Info", icon=icon("info"),
                                                       tags$h1("The Generate Pipeline"),
                                                       tags$p("The generate pipeline allows you to generate stimuli for any possible factorial design. This section of the Shiny App is a front end to the LexOPS functions, split_by(), control_for(), and generate()."),
                                                       tags$h3(icon("sliders-h"), "Specify Design"),
                                                       tags$p("Firstly, specify your design in the 'Specify Design' tab. This allows you to specify the factorial design for your experiment. Here 'splits' refer to independent variables in your factorial design, and 'controls' refer to variables which should be controlled for between conditions. You can also specify filters, to only generate stimuli from a subset of the full dataset. To add splits, controls, or filters:"),
                                                       tags$ol(
                                                           tags$li("Click the '+' button to add a box where you can specify the options for this split/control/filter."),
                                                           tags$li("Select the measure and source which you want to use from the drop-down menus."),
                                                           tags$li("Use the slider to specify the boundaries of each level of the split, how closely to match by the variable, or which section of the distribution to include in the filter."),
                                                           tags$li("Repeat for each split/control/filter that you wish to use in your design.")
                                                       ),
                                                       tags$h3(icon("wrench"), "Options"),
                                                       tags$p("Change default options for the generating algorithm to better suit your design."),
                                                       tags$h3(icon("sort-amount-down"), "Results"),
                                                       tags$p("Click the 'Generate' button to generate stimuli, and view the stimuli generated by LexOPS according to your design. This can be viewed in either long or wide format. The generated stimuli can also be written to csv format and downloaded from here. Stimuli can be re-generated by clicking the 'generate' button again."),
                                                       tags$h3(icon("search"), "Review"),
                                                       tags$p("Visualise a summary of the generated stimuli, and the algorithm's performance. More flexible options for visualisation are available in the Visualise section in the sidebar."),
                                                       tags$h3(icon("laptop-code"), "Codify"),
                                                       tags$p("Once you have generated your stimuli, this tab will generate R code that will reproduce the selected options. Exact stimuli lists can be reproduced by setting the seed in the Preferences tab in the sidebar.")
                                              ),
                                              tabPanel("Specify Design", icon=icon("sliders-h"),
                                                       fluidRow(
                                                           column(4, fluidRow(
                                                               valueBox(tags$p("Split by...", style="font-size: 75%;"), width = 12, color="light-blue", icon=icon("sitemap"),
                                                                        subtitle=fluidRow(column(12,
                                                                                                 actionButton("gen_splitby_add", icon("plus-square")),
                                                                                                 actionButton("gen_splitby_minus", icon("minus-square"))
                                                                        ))),
                                                               lapply(1:25, function(i) {
                                                                   boxid <- sprintf("gen_splitby_%i", i)
                                                                   uiOutput(boxid)
                                                               })
                                                           )),
                                                           column(4, fluidRow(
                                                               valueBox(tags$p("Control for...", style="font-size: 75%;"), width = 12, color="yellow", icon=icon("balance-scale"),
                                                                        subtitle=fluidRow(column(12,
                                                                                                 actionButton("gen_controlfor_add", icon("plus-square")),
                                                                                                 actionButton("gen_controlfor_minus", icon("minus-square"))
                                                                        ))),
                                                               lapply(1:25, function(i) {
                                                                   boxid <- sprintf("gen_controlfor_%i", i)
                                                                   uiOutput(boxid)
                                                               })
                                                           )),
                                                           column(4, fluidRow(
                                                               valueBox(tags$p("Filter by...", style="font-size: 75%;"), width = 12, color="purple", icon=icon("filter"),
                                                                        subtitle=fluidRow(column(12,
                                                                                                 actionButton("gen_filterby_add", icon("plus-square")),
                                                                                                 actionButton("gen_filterby_minus", icon("minus-square"))
                                                                        ))),
                                                               lapply(1:25, function(i) {
                                                                   boxid <- sprintf("gen_filterby_%i", i)
                                                                   uiOutput(boxid)
                                                               })
                                                           ))
                                                       )
                                              ),
                                              tabPanel("Options", icon=icon("wrench"),
                                                       tags$h3("How many stimuli should be generated?"),
                                                       numericInput("gen_stim_n", "Stimuli per Condition", value = 20, min = 1, step = 1, width = "100%"),
                                                       checkboxInput("gen_stim_n_all", "Generate as many as possible", width = "50%"),
                                                       HTML("<br>"),
                                                       tags$h3("Which condition should controls' tolerances be relative to?"),
                                                       uiOutput("gen_match_null_ui"),
                                                       HTML("<br>"),
                                                       tags$h3("What variables should be included in the long-format results?"),
                                                       selectInput("gen_res_include", "Include...", c("All available variables" = "all", "All variables used in the design" = "design"), selected = "design", width = "100%")
                                              ),
                                              tabPanel("Results", icon=icon("sort-amount-down"),
                                                       fluidRow(
                                                           column(12, div(id = "gen_console", style="overflow:auto; height:30px; background-color:#ffffff; font-family:Menlo,Monaco,Consolas,\"Courier New\",monospace;")),
                                                           column(4, actionButton("gen_regenerate", "Regenerate", icon=icon("sync-alt"), style = "width:100%; text-align:center;")),
                                                           column(4, div(selectInput("gen_data_format", NULL, c("Wide format"="wide", "Long format"="long"), selected="wide", width = "100%"), style = "text-align:center;")),
                                                           column(4, downloadButton("generated_stim_download", style = "width:100%; text-align:center;"))
                                                       ),
                                                       DT::dataTableOutput("generated_stim_dt")
                                              ),
                                              tabPanel("Review", icon=icon("search"),
                                                       tags$h1("Review Generated Stimuli"),
                                                       navlistPanel(
                                                           widths = c(2, 10),
                                                           tabPanel(
                                                               "Algorithm Performance",
                                                               tags$h3("Algorithm Performance"),
                                                               textOutput("gen_review_success_rate"),
                                                               uiOutput("gen_review_iteration_plot_ui")
                                                           ),
                                                           tabPanel(
                                                               "Filters",
                                                               tags$h3("Filters"),
                                                               uiOutput("gen_plot_filters_ui")
                                                           ),
                                                           tabPanel(
                                                               "Splits",
                                                               tags$h3("Splits"),
                                                               uiOutput("gen_plot_splits_ui")
                                                           ),
                                                           tabPanel(
                                                               "Controls",
                                                               tags$h3("Controls"),
                                                               uiOutput("gen_plot_controls_ui")
                                                           ),
                                                           tabPanel(
                                                               "Match Nulls",
                                                               tags$h3("Match Null Distribution"),
                                                               uiOutput("gen_review_null_distribution_ui")
                                                           )
                                                       )
                                              ),
                                              tabPanel("Codify", icon = icon("laptop-code"),
                                                       tags$h1("Codify"),
                                                       tags$p("This R code will generate stimuli for the design you have specified. If you've set the random seed, this code will also generate the same stimuli list as that generated in the app each time it is run. You can set the seed in the Preferences tab on the sidebar."),
                                                       verbatimTextOutput("gen_codify_text")
                                              )

                                          )
                                  ),
                                  # Match Word
                                  tabItem(tabName = "match_word",
                                          fluidRow(
                                              column(12, tags$div(id = "match_string_input", textInput("match_string", "Target String: ", "thicket", width = "250px"), tags$br())),
                                              column(12,
                                                     tabsetPanel(
                                                         type = "tabs",
                                                         tabPanel("Info", icon = icon("info"),
                                                                  tags$h1("Match Individual Words"),
                                                                  tags$p("This section creates a list of potential matches for an individual word. You might want to do this if you need to hand-pick matches (e.g. to be feasible controls in a sentential context). The target word is enetered in the textbox at the top."),
                                                                  tags$h3(icon("sliders-h"), "Specify Design"),
                                                                  tags$p("Firstly, specify your design in the 'Specify Design' tab. Here you specify the variables that should be matched by, and their tolerances. It is also possible to specify filters. Whereas matching variables can be used to get a subset of the data within a tolerance relative to the target word, the effect of filters will not change if the target word changes. As in the generate pipeline, the UI is used as follows:"),
                                                                  tags$ol(
                                                                      tags$li("Click the '+' button to add a box where you can specify the options for this split/control/filter."),
                                                                      tags$li("Select the measure and source which you want to use from the drop-down menus."),
                                                                      tags$li("Use the slider to specify the how closely to match by the variable, or which section of the distribution to include in the filter."),
                                                                      tags$li("Repeat for each match/filter that you wish to use in your design.")
                                                                  ),
                                                                  tags$h3(icon("wrench"), "Options"),
                                                                  tags$p("Change default options to better suit your design."),
                                                                  tags$h3(icon("sort-amount-down"), "Results"),
                                                                  tags$p("View suggested matches, ordered ascendingly in terms of Euclidean distance. The suggested matches can be downloaded here in .csv format.")
                                                         ),
                                                         tabPanel(
                                                             "Specify Design", icon = icon("sliders-h"),
                                                             column(6, fluidRow(
                                                                 valueBox(tags$p("Match by...", style="font-size: 75%;"), width = 12, color="yellow", icon=icon("balance-scale"),
                                                                          subtitle=fluidRow(column(12,
                                                                                                   actionButton("match_matchby_add", icon("plus-square")),
                                                                                                   actionButton("match_matchby_minus", icon("minus-square"))
                                                                          ))),
                                                                 lapply(1:25, function(i) {
                                                                     boxid <- sprintf("match_matchby_%i", i)
                                                                     uiOutput(boxid)
                                                                 })
                                                             )),
                                                             column(6, fluidRow(
                                                                 valueBox(tags$p("Filter by...", style="font-size: 75%;"), width = 12, color="purple", icon=icon("filter"),
                                                                          subtitle=fluidRow(column(12,
                                                                                                   actionButton("match_filterby_add", icon("plus-square")),
                                                                                                   actionButton("match_filterby_minus", icon("minus-square"))
                                                                          ))),
                                                                 lapply(1:25, function(i) {
                                                                     boxid <- sprintf("match_filterby_%i", i)
                                                                     uiOutput(boxid)
                                                                 })
                                                             ))
                                                         ),
                                                         tabPanel(
                                                             "Options", icon = icon("wrench"),
                                                             tags$h3("Filter by match tolerances?"),
                                                             checkboxInput("match_tolerance_filter", "Filter by match tolerances", value = TRUE, width = "100%"),
                                                             tags$p("If the tolerances are used as a filter, possible matches outside the tolerance will be discarded. If tolerances are not used as filters, possible matches outside the tolerance will still be included. The new column, \"matchFilter\", will then indicate whether each word is within the tolerance. Disabling filtering by match tolerances can be useful if you have multiple numeric variables, and you want to find the closest match by Euclidean distance alone.")
                                                         ),
                                                         tabPanel(
                                                             "Results", icon = icon("sort-amount-down"),
                                                             fluidRow(
                                                                 column(8, tags$br()),
                                                                 column(4, downloadButton("matched_stim_download", style = "width:100%; text-align:center;"))
                                                             ),
                                                             DT::dataTableOutput("matched_stim_dt")
                                                         )
                                                     )
                                              )
                                          )
                                  ),
                                  # Fetch
                                  tabItem(tabName='fetch', uiOutput("fetch_tab_content")),
                                  # Visualise
                                  tabItem(tabName="visualise",
                                          # Get screen dimensions with java
                                          tags$head(tags$script('var dimension = [0, 0];
                                                $(document).on("shiny:connected", function(e) {
                                                dimension[0] = window.innerWidth;
                                                dimension[1] = window.innerHeight;
                                                Shiny.onInputChange("dimension", dimension);
                                                });
                                                $(window).resize(function(e) {
                                                dimension[0] = window.innerWidth;
                                                dimension[1] = window.innerHeight;
                                                Shiny.onInputChange("dimension", dimension);
                                                });
                                                ')),
                                          uiOutput("visualise_tab_content"),
                                          uiOutput("visualisation_ui_box")
                                          ),
                                  # Custom Variables
                                  tabItem(tabName='custom_variables',
                                          fluidRow(
                                              box(
                                                  title='1) Upload Data', status='primary',
                                                  collapsible=T, collapsed=F, width=12,
                                                  fluidRow(
                                                      column(12, uiOutput('cust_opts_inputfile_choice')),
                                                      column(12, textOutput('cust_filename')),
                                                      column(12, uiOutput('cust_opts_filehasheaders_choice')),
                                                      column(12, br()),
                                                      column(12, uiOutput('cust_opts_column_choice'))
                                                  )),
                                              box(
                                                  title='2) Choose Target Features', status='primary',
                                                  collapsible=T, collapsed=F, width=12,
                                                  fluidRow(
                                                      column(12, radioButtons('cust_opts_all', NULL, c("Include all columns from uploaded data as custom variables"="all", "select which variables to include manually"="some"))),
                                                      column(12, uiOutput("cust_opts_choice"))
                                                  )
                                              ),
                                              box(
                                                  title='3) Summary of Uploaded Variables', status='primary',
                                                  collapsible=T, collapsed=F, width=12,
                                                  fluidRow(
                                                      column(12, tableOutput('cust_uploadedvars'))
                                                  )
                                              )
                                          )),
                                  # Preferences
                                  tabItem(tabName='preferences',
                                          box(title = "Random Seed", status = "primary",
                                              collapsible = F, width = 12,
                                              fluidRow(
                                                  column(12, checkboxInput("preference_use_a_random_seed", "Use a random seed for the Generate Pipeline", FALSE)),
                                                  column(12, numericInput("preference_random_seed", "Random seed value", value = sample(1:2147483647, 1), step = 1, max = 2147483647))
                                              )
                                          ),
                                          box(
                                              title="UI Preferences", status='primary',
                                              collabsible=F, width=12,
                                              fluidRow(
                                                  column(12, radioButtons('preference_toleranceUI', 'How Should Tolerances be Selected?', c('Click and Drag Sliders'='slider', 'Type Numeric Input'='numericinput'), selected='slider', inline=T))
                                              )
                                          )
                                  ),
                                  # Info
                                  tabItem(tabName='info',
                                          box(
                                              title=NULL, status='primary',
                                              collabsible=F, width=12,
                                              fluidRow(
                                                  column(12, align = "center", tags$p(sprintf("LexOPS version %s", packageVersion("LexOPS")), style='font-size:20px; color:black;')),
                                                  column(12, br()),
                                                  column(12, br()),
                                                  column(4, align="center",
                                                         fluidRow(
                                                             column(12, tags$a(href = 'https://jackt.shinyapps.io/lexops/', icon('book-open'), style='font-size:75px; color:black;')),
                                                             column(12, tags$a(href = 'https://jackt.shinyapps.io/lexops/', 'shinyapps.io', style='font-size:25px; color:black;'))
                                                         )),
                                                  column(4, align="center",
                                                         fluidRow(
                                                             column(12, tags$a(href = 'https://github.com/JackEdTaylor/LexOPS', icon('github'), style='font-size:75px; color:black;')),
                                                             column(12, tags$a(href = 'https://github.com/JackEdTaylor/LexOPS', 'GitHub Repository', style='font-size:25px; color:black;'))
                                                         )),
                                                  column(4, align="center",
                                                         fluidRow(
                                                             column(12, tags$a(href = '#???', icon('file-alt'), style='font-size:75px; color:black;')),
                                                             column(12, tags$a(href = '#???', 'Read the Paper', style='font-size:25px; color:black;'))
                                                         )),
                                                  column(12, br()),
                                                  column(12, br())
                                              ),
                                              fluidRow(
                                                  column(12, br()),
                                                  column(12, align="center", downloadButton('full_dataset_download', 'Download the LexOPS Dataset', style='font-size:110%')))
                                          )
                                  )

                              )
                          )
            )
        )
    )
)
