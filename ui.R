library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)
library(colourpicker)

# Visualilsation vector categories - needed in UI for Visualisation tab
vis.cats <- c('Word Frequency', 'Part of Speech', 'Length', 'Bigram Probability', 'Orthographic Neighbourhood', 'Syllables', 'Phonemes', 'Rhyme', 'Phonological Neighbourhood', 'Number of Pronunciations', 'Familiarity', 'Age of Acquisition', 'Concreteness', 'Arousal', 'Valence', 'Dominance', 'Imageability', 'Semantic Size', 'Semantic Gender', 'Humour', 'Word Prevalence', 'Proportion Known', 'Lexical Decision Response Time', 'Lexical Decision Accuracy', 'Custom Variable')

tagList(
  useShinyjs(),
  tags$head(
    tags$link(href = "style.css", rel = "stylesheet")
  ),
  div(
    id = "loading_page",
    img(src = "lexopslogo_white.png", class = 'center-fit'),
    br(), br(),
    img(src = "ajax-loader.gif")
  ),
  
  hidden(
    div(
      id = "main_content",
      dashboardPage(title='LexOPS',
                    skin='black',
                    
                    dashboardHeader(title=tags$a(href="javascript:history.go(0)",
                                                 tags$img(src='lexopslogo_black_textonly.png', height='30px')),
                                    titleWidth=200),
                    
                    dashboardSidebar(width=200,
                                     sidebarMenu(
                                       style = 'position: fixed; overflow: visible;',  # Stationary sidebar while scrolling
                                       menuItem('Generate', icon=icon('cogs'),
                                                startExpanded = T,
                                                menuSubItem('Options', tabName="generate_options", icon=icon('sliders-h')),
                                                menuSubItem('Results', tabName="generate_results", icon=icon('sort-amount-down'))),
                                       menuItem('Match', icon=icon('balance-scale'),
                                                startExpanded = F,
                                                textInput('matchstring', 'Word:', 'thicket', width="90%"),  # must explicitly give width of UI items in sidebar to avoid overflowing
                                                column(1, textOutput('nrow.matchresults')),
                                                br(), br(),
                                                menuSubItem('Options', tabName="match_options", icon=icon('sliders-h')),
                                                menuSubItem('Results', tabName="match_results", icon=icon('sort-amount-down'))),
                                       menuItem('Fetch', tabName='fetch', icon=icon('file-import')),
                                       menuItem('Visualise', tabName="visualise", icon=icon('chart-bar')),
                                       br(),
                                       menuItem('Custom Variables', tabName='custom_variables', icon=icon('plus')),
                                       menuItem('Info', tabName='info', icon=icon('info-circle'))
                                     )),
                    
                    dashboardBody(
    
                      tabItems(
                        tabItem(tabName = 'generate_options',
                                fluidRow(
                                  column(4, fluidRow(
                                    valueBox("Split by...", width = 12, color='light-blue', icon=icon("sitemap"),
                                             subtitle=fluidRow(column(12,
                                                                      actionButton('gen_splitby_add', icon("plus-square")),
                                                                      actionButton('gen_splitby_minus', icon("minus-square"))
                                             ))),
                                    lapply(1:25, function(i) {
                                      boxid <- sprintf('gen_splitby_%i', i)
                                      uiOutput(boxid)
                                    })
                                  )),
                                  column(4, fluidRow(
                                    valueBox("Control for...", width = 12, color='yellow', icon=icon("balance-scale"),
                                             subtitle=fluidRow(column(12,
                                                                      actionButton('gen_controlfor_add', icon("plus-square")),
                                                                      actionButton('gen_controlfor_minus', icon("minus-square"))
                                             ))),
                                    lapply(1:25, function(i) {
                                      boxid <- sprintf('gen_controlfor_%i', i)
                                      uiOutput(boxid)
                                    })
                                  )),
                                  column(4, fluidRow(
                                    valueBox("Filter by...", width = 12, color='purple', icon=icon("filter"),
                                             subtitle=fluidRow(column(12,
                                                                      actionButton('gen_filterby_add', icon("plus-square")),
                                                                      actionButton('gen_filterby_minus', icon("minus-square"))
                                             ))),
                                    lapply(1:25, function(i) {
                                      boxid <- sprintf('gen_filterby_%i', i)
                                      uiOutput(boxid)
                                    })
                                  ))
                                )),
                        tabItem(tabName = 'generate_results',
                                fluidRow(
                                  box(title='Results Options', status='primary',
                                      collapsible=T, collapsed=F, width=12,
                                      fluidRow(
                                        column(6, align="center", actionButton("gen_generate", "Generate/Regenerate Stimuli List", icon=icon("redo-alt"))),
                                        column(6, align="center", downloadButton('generated.csv', 'Download Generated Stimuli')),
                                        column(12, br()),
                                        column(12, br()),
                                        column(6, radioButtons('gen_limit_N', 'Number of Items', c('Generate N Items'='N', 'Generate All Possible Items'='all'), 'N')),
                                        column(6, uiOutput('gen_N_stim_choice')),
                                        column(12, uiOutput('gen_dataformat_choice')),
                                        column(12, HTML('&nbsp;')),
                                        box(
                                          title='Condition-Matching Options', status='primary',
                                          collapsible=T, collapsed=T, width=12,
                                          fluidRow(
                                            column(12, uiOutput('gen_controlnull_choice')),
                                            column(12, checkboxInput('gen_check.dist', 'Filter by Euclidean/CityBlock Distance from Null Condition', 0)),
                                            uiOutput('gen_dist_opts_choice')
                                          ))
                                        ))),
                                DT::dataTableOutput('gen_results_dt')),
                        # Match Options tab
                        tabItem(tabName='match_options',
                                
                                fluidRow(
                                  column(12, fluidRow(
                                    valueBox("Match by...", width = 12, color='light-blue', icon=icon("balance-scale"),
                                             subtitle=fluidRow(column(12,
                                                                      actionButton('match_add', icon("plus-square")),
                                                                      actionButton('match_minus', icon("minus-square"))
                                             ))),
                                    lapply(1:50, function(i) {
                                      boxid <- sprintf('matchbox_%i', i)
                                      uiOutput(boxid)
                                    })
                                  ))
                                )
                                
                                ),
                        # Match Results tab
                        tabItem(tabName='match_results',
                                fluidRow(
                                  box(title='Results Options', status='primary',
                                      collapsible=T, collapsed=T, width=12,
                                      fluidRow(
                                        column(12, h5(strong("Download"))),
                                        column(12, downloadButton('matched.csv', 'Download Suggested Matches')),
                                        column(12, br()),
                                        column(12, h5(strong("Results Format"))),
                                        column(12, selectInput('results.format', NULL, c('Raw Values'='rv',
                                                                                                     'Distances (Absolute Difference from Target Word)'='dist',
                                                                                                     'Differences (from Target Word)'='diff'), selected='dist')),
                                        column(12, h5(strong("Ignore Filters"))),
                                        column(12, checkboxInput('check.match.ignorefilters', 'Ignore Tolerances on Options Tab', 0)),
                                        column(12, br()),
                                        box(
                                          title='Sorting Options', status='primary',
                                          collapsible=T, collapsed=T, width=12,
                                          fluidRow(
                                            column(6, uiOutput('match_results_sort_1_choice')),
                                            column(6, uiOutput('match_results_sort_1_order_choice')),
                                            column(6, uiOutput('match_results_sort_2_choice')),
                                            column(6, uiOutput('match_results_sort_2_order_choice')),
                                            column(6, uiOutput('match_results_sort_3_choice')),
                                            column(6, uiOutput('match_results_sort_3_order_choice')),
                                            column(6, uiOutput('match_results_sort_4_choice')),
                                            column(6, uiOutput('match_results_sort_4_order_choice')),
                                            column(6, uiOutput('match_results_sort_5_choice')),
                                            column(6, uiOutput('match_results_sort_5_order_choice'))
                                          )),
                                        box(
                                          title='Distance Measures', status='primary',
                                          collapsible=T, collapsed=T, width=12,
                                          fluidRow(
                                            column(12, h5(strong("Euclidean Distance"))),
                                            column(12, checkboxInput('check.matchdist.ed', 'Calculate Euclidean Distance', 1)),
                                            column(12, textOutput('match_results_ed_all_choice_text')),
                                            column(12, uiOutput('match_results_ed_all_choice')),
                                            column(12, uiOutput('match_results_ed_opts_choice')),
                                            column(12, br()),
                                            column(12, h5(strong("City Block Distance"))),
                                            column(12, checkboxInput('check.matchdist.cb', 'Calculate City Block Distance', 0)),
                                            column(12, textOutput('match_results_cb_all_choice_text')),
                                            column(12, uiOutput('match_results_cb_all_choice')),
                                            column(12, uiOutput('match_results_cb_opts_choice'))
                                          ))
                                      ))
                                  
                                ),
                                DT::dataTableOutput('match_results_dt')
                        ),
                        # Fetch tab
                        tabItem(tabName='fetch',
                                fluidRow(
                                  box(
                                    title='1) Stimuli Input', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, radioButtons('fetch.inputtype', 'Input Type', c("File (.csv, .tsv, .xls, .xlsx)"="file", "Text Box (type/copy-paste the words in)"="cp"))),
                                      column(12, uiOutput('fetch.opts.inputfile.choice')),
                                      column(12, uiOutput('fetch.opts.inputtext.choice')),
                                      column(12, textOutput('fetch.filename')),
                                      column(12, uiOutput('fetch.opts.filehasheaders.choice')),
                                      column(12, br()),
                                      column(12, uiOutput('fetch.opts.column.choice')),
                                      column(12, uiOutput('fetch.textsep.choice'))
                                    )),
                                  box(
                                    title='2) Choose Target Features', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, uiOutput('fetch.includeorig.choice')),
                                      column(12, radioButtons('fetch.opts.all', NULL, c("Include all LexOPS Features"="all", "Select LexOPS Features Manually"="some"))),
                                      lapply(1:length(vis.cats), function(catnr) {
                                        column(12, uiOutput(sprintf("fetch.opts.choice.%i", catnr)))
                                      })
                                    )
                                  ),
                                  box(
                                    title='3) Results', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, textOutput('fetch.results.plsinput')),
                                      column(12, uiOutput('fetched.csv.choice')),
                                      br(), br(), br(),
                                      column(12, DT::dataTableOutput('fetch_df_res_dt'))
                                    )
                                  )
                                )),
                        # Visualise tab
                        tabItem(
                          
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
                          tabName='visualise',
                          fluidRow(box(
                            title='Plot Controls', status='primary',
                            collapsible = T, width=12,
                            fluidRow(
                              column(6, selectInput('vis.xaxis.opts', 'X Axis', vis.cats, "Word Frequency")),
                              column(6, uiOutput('vis.xsource.choice'))
                            ),
                            fluidRow(
                              column(6, selectInput('vis.yaxis.opts', 'Y Axis', vis.cats, "Lexical Decision Response Time")),
                              column(6, uiOutput('vis.ysource.choice'))
                            ),
                            fluidRow(
                              column(6, selectInput('vis.zaxis.opts', 'Z Axis', c('(None)', vis.cats))),
                              column(6, uiOutput('vis.zsource.choice'))
                            ),
                            fluidRow(
                              column(6, selectInput('vis.colour.opts', 'Colour', c('(None)', 'Generated Stimuli', 'Generated Stimuli Condition', 'Target Match Word', 'Suggested Matches', 'Words Uploaded to Fetch Tab', 'Part of Speech', vis.cats), "(None)")),
                              column(6, uiOutput('vis.coloursource.choice'))
                            ),
                            br(),
                            fluidRow(
                              column(6, sliderInput('vis.opacity.sl', 'Point Opacity', value=0.85, min=0.1, max=1, step=0.05)),
                              column(6, sliderInput('vis.pointsize.sl', 'Point Size', value=4, min=1, max=10, step=1))
                            ),
                            br(),
                            fluidRow(
                              column(6, colourInput('vis.bgcolour', 'Background Colour', value="black")),
                              column(6, colourInput('vis.textcolour', 'Text Colour', value="white"))
                            ),
                            br(),
                            br(),
                            fluidRow(
                              column(12, align="center", actionButton('vis.generateplot', 'Regenerate Plot', icon=icon("chart-bar"), style='font-size:125%'))
                            )
                          )),
                          
                          uiOutput('visualisation.ui_box')
                        ),
                        # Custom Variables tab
                        tabItem(tabName='custom_variables',
                                fluidRow(
                                  box(
                                    title='1) Upload Data', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, uiOutput('cust.opts.inputfile.choice')),
                                      column(12, textOutput('cust.filename')),
                                      column(12, uiOutput('cust.opts.filehasheaders.choice')),
                                      column(12, br()),
                                      column(12, uiOutput('cust.opts.column.choice'))
                                    )),
                                  box(
                                    title='2) Choose Target Features', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, radioButtons('cust.opts.all', NULL, c("Include all Columns from Uploaded Data as Custom Variables"="all", "Select which Variables to Include Manually"="some"))),
                                      column(12, uiOutput("cust.opts.choice"))
                                    )
                                  ),
                                  box(
                                    title='3) Summary of Uploaded Variables', status='primary',
                                    collapsible=T, collapsed=F, width=12,
                                    fluidRow(
                                      column(12, tableOutput('cust.uploadedvars'))
                                    )
                                  )
                                  )),
                        # Info tab
                        tabItem(tabName='info',
                                downloadButton('LexOPS.csv', 'Download the LexOPS Database')
                        )
                          )
                      
                      )
                    )
    )
)
  
  
  
  
)

