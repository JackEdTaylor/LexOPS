library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)

# Visualilsation vector categories - needed in UI for Visualisation tab
vis.cats <- c('Word Frequency', 'Part of Speech', 'Length', 'Bigram Probability', 'Orthographic Neighbourhood', 'Syllables', 'Phonemes', 'Rhyme', 'Phonological Neighbourhood', 'Number of Pronunciations', 'Familiarity', 'Age of Acquisition', 'Concreteness', 'Arousal', 'Valence', 'Dominance', 'Imageability', 'Semantic Size', 'Semantic Gender', 'Humour', 'Lexical Decision Response Time', 'Lexical Decision Accuracy', 'Custom Variable')

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
                    
                    dashboardHeader(title=tags$a(href='http://www.psy.gla.ac.uk',
                                                 tags$img(src='lexopslogo_black_textonly.png', height='30px')),
                                    titleWidth=200),
                    
                    dashboardSidebar(width=200,
                                     sidebarMenu(
                                       style = 'position: fixed; overflow: visible;',  # Stationary sidebar while scrolling
                                       menuItem('Generate', tabName='generate_options', icon=icon('cogs'),
                                                startExpanded = T,
                                                menuSubItem('Options', tabName="generate_options", icon=icon('sliders-h')),
                                                menuSubItem('Generated Stimuli', tabName="generate_results", icon=icon('sort-amount-down'))),
                                       menuItem('Match', icon=icon('search'),
                                                startExpanded = F,
                                                textInput('string', 'Word:', 'thicket', width="100%"),  # must explicitly give width of UI items in sidebar to avoid overflowing
                                                column(1, textOutput('nrow.matchresults')),
                                                br(), br(),
                                                menuSubItem('Select & Filter', tabName="match_options", icon=icon('sliders-h')),
                                                menuSubItem('Suggested Matches', tabName="match_results", icon=icon('sort-amount-down'))),
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
                                    valueBox("Split by...", width = 12, color='light-blue',
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
                                    valueBox("Control for...", width = 12, color='yellow',
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
                                    valueBox("Filter by...", width = 12, color='purple',
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
                                  box(title='Options', status='primary',
                                      collapsible=T, collapsed=F, width=12,
                                      fluidRow(
                                        column(6, align="center", actionButton("gen_generate", "Generate/Regenerate Stimuli List", icon=icon("redo-alt"))),
                                        column(6, align="center", downloadButton('generated.csv', 'Download Generated Stimuli')),
                                        column(12, br()),
                                        column(12, br()),
                                        column(6, radioButtons('gen_limit_N', 'Number of Items', c('Generate N Items'='N', 'Generate All Possible Items'='all'), 'N')),
                                        column(6, uiOutput('gen_N_stim_choice')),
                                        column(12, radioButtons('gen_dataformat', 'Data Format', c('Wide'='wide', 'Long'='long'), 'wide')),
                                        column(12, HTML('&nbsp;')),
                                        box(
                                          title='Condition-Matching Options', status='primary',
                                          collapsible=T, collapsed=T, width=12,
                                          fluidRow(
                                            column(12, uiOutput('gen_controlnull_choice')),
                                            column(12, checkboxInput('gen_check.dist', 'Filter by Euclidean/CityBlock Distance', 0)),
                                            uiOutput('gen_dist_opts_choice')
                                          ))
                                        ))),
                                DT::dataTableOutput('gen_results_dt')),
                        # Match Options tab
                        tabItem(tabName='match_options',
                                fluidRow(
                                  # Lexical Header
                                  valueBox("Lexical Features", subtitle=NULL, width = 12, color='light-blue', icon=icon("book-open")),
                                  # Frequency box
                                  box(
                                    width=6,
                                    title='Word Frequency', status='primary', solidHeader=T,
                                    checkboxInput('check.frequency', 'Match by Frequency', 0),
                                    checkboxInput('frequency.log', 'Log Transform (Zipf)', 1),
                                    checkboxGroupInput('frequency.opt', 'Corpora',
                                                       c('BNC (written)'='bnc_w', 'BNC (spoken)'='bnc_s', 'SUBTLEX-UK'='suk', 'SUBTLEX-US'='sus'),
                                                       c('bnc_w', 'bnc_s', 'suk', 'sus'),
                                                       inline=T),
                                    br(),
                                    uiOutput('frequency.sl.choice'),
                                    textOutput('descr.frequency'),
                                    plotOutput('plot.freq', height='170px')
                                  ),
                                  # Part of Speech box
                                  box(
                                    width=6,
                                    title='Part of Speech (PoS)', status='primary', solidHeader=T,
                                    checkboxInput('check.partofspeech', 'Match by Part of Speech', 0),
                                    radioButtons('pos.opt', 'Corpus',
                                                 c('SUBTLEX-UK'='suk',
                                                   'BNC (written)'='bnc_w',
                                                   'BNC (spoken)'='bnc_s',
                                                   'English Lexicon Project (ELP)'='elp'),
                                                 selected='suk',
                                                 inline=T),
                                    checkboxInput('check.manual.pos', 'Select Manually', 0),
                                    uiOutput('manual.pos.choice'),
                                    textOutput('descr.partofspeech'),
                                    plotOutput('plot.pos', height='200px')
                                  ),
                                  # Orthographic Header
                                  valueBox("Orthographic Features", subtitle=NULL, width = 12, color='yellow', icon=icon("font")),
                                  # Length box
                                  box(
                                    width=6,
                                    title='Length', status='warning', solidHeader=T,
                                    checkboxInput('check.length', 'Match by Length (Number of Characters)', 0),
                                    br(),
                                    sliderInput('length.sl', NULL, value=c(0, 0), min=-5, max=5, step=1),
                                    textOutput('descr.length'),
                                    plotOutput('plot.length', height='170px')
                                  ),
                                  # Bigram Frequency box
                                  box(
                                    width=6,
                                    title='Bigram Probability (BG)', status='warning', solidHeader=T,
                                    checkboxInput('check.bgfreq', 'Match by Bigram Probability', 0),
                                    checkboxGroupInput('bgfreq.opt', 'Corpora',
                                                       c('BNC (written)'='bnc.wbg', 'BNC (spoken)'='bnc.sbg', 'SUBTLEX-UK'='subtlex_uk.bg', 'SUBTLEX-US'='subtlex_us.bg'),
                                                       c('bnc.wbg', 'bnc.sbg', 'subtlex_uk.bg', 'subtlex_us.bg'),
                                                       inline=T),
                                    br(),
                                    sliderInput('bgfreq.sl', NULL, value=c(-0.002, 0.002), min=-.01, max=.01, step=.001),
                                    textOutput('descr.bgfreq'),
                                    plotOutput('plot.bgfreq', height='170px')
                                  ),
                                  # Orthographic Neighbourhood Size
                                  box(
                                    width=6,
                                    title='Orthographic Neighbourhood Size (ON)', status='warning', solidHeader=T,
                                    checkboxInput('check.on', 'Match by ON', 0),
                                    checkboxInput('on.log', 'Log Transform', 0),
                                    radioButtons('on.opt', 'Measure',
                                                 c('Orthographic Levenshtein Distance 20 (OLD20)'='old20', "Coltheart's N"='cn'),
                                                 'old20',
                                                 inline=T),
                                    br(),
                                    uiOutput('on.sl.choice'),
                                    textOutput('descr.on'),
                                    plotOutput('plot.on', height='170px')
                                  ),
                                  # Orthographic Similarity (to target word) box
                                  box(
                                    width=6,
                                    title='Orthographic Similarity (OS)', status='warning', solidHeader=T,
                                    checkboxInput('check.os', 'Match by OS to Target Word', 0),
                                    radioButtons('os.opt', 'Measure',
                                                 c('Levenshtein Distance (LD)'='ld', 'Levenshtein-Damerau Distance (LDD)'='ldd'),
                                                 'ld',
                                                 inline=T),
                                    br(),
                                    sliderInput('os.sl', NULL, value=c(0, 6), min=0, max=25, step=1),
                                    textOutput('descr.os'),
                                    plotOutput('plot.os', height='170px')
                                  ),
                                  # Phonological Header
                                  valueBox("Phonological Features", subtitle=NULL, width = 12, color='purple', icon=icon("volume-up")),
                                  # Syllables box
                                  box(
                                    width=6,
                                    title='Syllables', status='info', solidHeader=T,
                                    checkboxInput('check.syllables', 'Match by Number of Syllables', 0),
                                    radioButtons('syllables.opt', 'Source(s)',
                                                 c('CMU Pronouncing Dictionary'='cmu', 'Moby Project'='mp'),
                                                 'cmu',
                                                 inline=T),
                                    uiOutput('manual.pron.syllables.choice'),
                                    br(),
                                    sliderInput('syllables.sl', NULL, value=c(0, 0), min=-5, max=5, step=1),
                                    textOutput('descr.syllables'),
                                    plotOutput('plot.syllables', height='170px')
                                  ),
                                  # Phonemes box
                                  box(
                                    width=6,
                                    title='Phonemes', status='info', solidHeader=T,
                                    checkboxInput('check.phonemes', 'Match by Number of Phonemes', 0),
                                    radioButtons('phonemes.opt', 'Source(s)',
                                                 c('CMU Pronouncing Dictionary'='cmu'),
                                                 'cmu',
                                                 inline=T),
                                    uiOutput('manual.pron.phonemes.choice'),
                                    br(),
                                    sliderInput('phonemes.sl', NULL, value=c(0, 0), min=-5, max=5, step=1),
                                    textOutput('descr.phonemes'),
                                    plotOutput('plot.phonemes', height='170px')
                                  ),
                                  # Rhyme box
                                  box(
                                    width=6,
                                    title='Rhyme', status='info', solidHeader=T,
                                    checkboxInput('check.rhyme', 'Match by Rhyme', 0),
                                    radioButtons('rhyme.opt', 'Source(s)',
                                                 c('CMU Pronouncing Dictionary'='cmu'),
                                                 'cmu',
                                                 inline=T),
                                    uiOutput('manual.pron.rhyme.choice'),
                                    br(),
                                    textOutput('descr.rhyme'),
                                    plotOutput('plot.rhyme', height='160px')
                                  ),
                                  # Phonological Neighbourhood Size
                                  box(
                                    width=6,
                                    title='Phonological Neighbourhood Size (PN)', status='info', solidHeader=T,
                                    checkboxInput('check.pn', 'Match by PN', 0),
                                    checkboxInput('pn.log', 'Log Transform', 0),
                                    radioButtons('pn.source', 'Source',
                                                 c('CMU Pronouncing Dictionary'='cmu'),
                                                 'cmu',
                                                 inline=T),
                                    radioButtons('pn.opt', 'Measure',
                                                 c('Phonological Levenshtein Distance 20 (PLD20)'='pld20', "Coltheart's N"='cn'),
                                                 'pld20',
                                                 inline=T),
                                    uiOutput('manual.pron.pn.choice'),
                                    br(),
                                    uiOutput('pn.sl.choice'),
                                    textOutput('descr.pn'),
                                    plotOutput('plot.pn', height='170px')
                                  ),
                                  # Phonological Similarity (to target word) box
                                  box(
                                    width=6,
                                    title='Phonological Similarity (PS)', status='info', solidHeader=T,
                                    checkboxInput('check.ps', 'Match by PS to Target Word', 0),
                                    radioButtons('ps.opt', 'Measure',
                                                 c('Levenshtein Distance (LD)'='ld', 'Levenshtein-Damerau Distance (LDD)'='ldd'),
                                                 'ld',
                                                 inline=T),
                                    uiOutput('manual.pron.ps.choice'),
                                    br(),
                                    sliderInput('ps.sl', NULL, value=c(0, 6), min=0, max=25, step=1),
                                    textOutput('descr.ps'),
                                    plotOutput('plot.ps', height='170px')
                                  ),
                                  # Number of Pronunciations
                                  box(
                                    width=6,
                                    title='Number of Pronunciations (PrN)', status='info', solidHeader=T,
                                    checkboxInput('check.prn', 'Match by PrN', 0),
                                    radioButtons('prn.opt', 'Source(s)',
                                                 c('CMU Pronouncing Dictionary'='cmu'),
                                                 'cmu',
                                                 inline=T),
                                    br(),
                                    sliderInput('prn.sl', NULL, value=c(0, 0), min=-3, max=3, step=1),
                                    textOutput('descr.prn'),
                                    plotOutput('plot.prn', height='170px')
                                  ),
                                  # Semantic Header
                                  valueBox("Semantic Features", subtitle=NULL, width = 12, color='green', icon=icon("lightbulb")),
                                  # Familiarity box
                                  box(
                                    width=6,
                                    title='Familiarity (FAM)', status='success', solidHeader=T,
                                    checkboxInput('check.fam', 'Match by Familiarity Ratings', 0),
                                    checkboxGroupInput('fam.opt', 'Source(s)',
                                                       c('Glasgow Norms'='gn', 'Clark and Paivio (2004)'='cp'),
                                                       selected='gn',
                                                       inline=T),
                                    br(),
                                    sliderInput('fam.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.fam'),
                                    plotOutput('plot.fam', height='170px')
                                  ),
                                  # Age of Acquisition box
                                  box(
                                    width=6,
                                    title='Age of Acquisition (AoA)', status='success', solidHeader=T,
                                    checkboxInput('check.aoa', 'Match by Age of Acquisition Ratings/Estimates', 0),
                                    checkboxGroupInput('aoa.opt', 'Source(s)',
                                                       c('Kuperman et al. (2012)'='kuperman', 'Glasgow Norms'='gn', 'Brysbaert & Biemiller (2017)'='bb'),
                                                       selected='kuperman',
                                                       inline=T),
                                    br(),
                                    uiOutput('aoa.sl.choice'),
                                    textOutput('descr.aoa'),
                                    plotOutput('plot.aoa', height='170px')
                                  ),
                                  # Concreteness box
                                  box(
                                    width=6,
                                    title='Concreteness (CNC)', status='success', solidHeader=T,
                                    checkboxInput('check.cnc', 'Match by Concreteness Ratings', 0),
                                    checkboxGroupInput('cnc.opt', 'Source(s)',
                                                       c('Brysbaert et al. (2014)'='brysbaert', 'Glasgow Norms'='gn'),
                                                       selected='brysbaert',
                                                       inline=T),
                                    br(),
                                    uiOutput('cnc.sl.choice'),
                                    textOutput('descr.cnc'),
                                    plotOutput('plot.cnc', height='170px')
                                  ),
                                  # Imageability box
                                  box(
                                    width=6,
                                    title='Imageability (IMAG)', status='success', solidHeader=T,
                                    checkboxInput('check.imag', 'Match by Imageability Ratings', 0),
                                    checkboxGroupInput('imag.opt', 'Source(s)',
                                                       c('Glasgow Norms'='gn', 'Clark and Paivio (2004)'='cp'),
                                                       selected='gn',
                                                       inline=T),
                                    br(),
                                    sliderInput('imag.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.imag'),
                                    plotOutput('plot.imag', height='170px')
                                  ),
                                  # Arousal box
                                  box(
                                    width=6,
                                    title='Arousal (AROU)', status='success', solidHeader=T,
                                    checkboxInput('check.arou', 'Match by Arousal Ratings', 0),
                                    checkboxGroupInput('arou.opt', 'Source(s)',
                                                       c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                                       selected='warriner',
                                                       inline=T),
                                    br(),
                                    sliderInput('arou.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.arou'),
                                    plotOutput('plot.arou', height='170px')
                                  ),
                                  # Valence box
                                  box(
                                    width=6,
                                    title='Valence (VAL)', status='success', solidHeader=T,
                                    checkboxInput('check.val', 'Match by Valence Ratings', 0),
                                    checkboxGroupInput('val.opt', 'Source(s)',
                                                       c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                                       selected='warriner',
                                                       inline=T),
                                    br(),
                                    sliderInput('val.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.val'),
                                    plotOutput('plot.val', height='170px')
                                  ),
                                  # Dominance box
                                  box(
                                    width=6,
                                    title='Dominance (DOM)', status='success', solidHeader=T,
                                    checkboxInput('check.dom', 'Match by Dominance Ratings', 0),
                                    checkboxGroupInput('dom.opt', 'Source(s)',
                                                       c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                                       selected='warriner',
                                                       inline=T),
                                    br(),
                                    sliderInput('dom.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.dom'),
                                    plotOutput('plot.dom', height='170px')
                                  ),
                                  # Semantic Size box
                                  box(
                                    width=6,
                                    title='Semantic Size (SIZE)', status='success', solidHeader=T,
                                    checkboxInput('check.size', 'Match by Semantic Size Ratings', 0),
                                    checkboxGroupInput('size.opt', 'Source(s)',
                                                       c('Glasgow Norms'='gn'),
                                                       selected='gn',
                                                       inline=T),
                                    br(),
                                    sliderInput('size.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.size'),
                                    plotOutput('plot.size', height='170px')
                                  ),
                                  # Semantic Gender box
                                  box(
                                    width=6,
                                    title='Semantic Gender (GEND)', status='success', solidHeader=T,
                                    checkboxInput('check.gen', 'Match by Semantic Gender Ratings', 0),
                                    checkboxGroupInput('gen.opt', 'Source(s)',
                                                       c('Glasgow Norms'='gn'),
                                                       selected='gn',
                                                       inline=T),
                                    br(),
                                    sliderInput('gen.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                                    textOutput('descr.gen'),
                                    plotOutput('plot.gen', height='170px')
                                  ),
                                  # Humour box
                                  box(
                                    width=6,
                                    title='Humour (HUM)', status='success', solidHeader=T,
                                    checkboxInput('check.hum', 'Match by Humour Ratings', 0),
                                    checkboxGroupInput('hum.opt', 'Source(s)',
                                                       c('Engelthaler & Hills (2018)'='eh'),
                                                       selected='eh',
                                                       inline=T),
                                    br(),
                                    sliderInput('hum.sl', NULL, value=c(-0.3, 0.3), min=-2, max=2, step=0.1),
                                    textOutput('descr.hum'),
                                    plotOutput('plot.hum', height='170px')
                                  ),
                                  # Behavioural Header
                                  valueBox("Behavioural Features", subtitle=NULL, width = 12, color='red', icon=icon("stopwatch")),
                                  # LDT RT box
                                  box(
                                    width=6,
                                    title='Lexical Decision Response Time (RT)', status='danger', solidHeader=T,
                                    checkboxInput('check.rt', 'Match by RT', 0),
                                    checkboxInput('rt.zscore', 'Z-Score', 1),
                                    radioButtons('rt.opt', 'Source(s)',
                                                 c('British Lexicon Project (BLP)'='blp', 'English Lexicon Project (ELP)'='elp'),
                                                 selected='blp',
                                                 inline=T),
                                    br(),
                                    uiOutput('rt.sl.choice'),
                                    textOutput('descr.rt'),
                                    plotOutput('plot.rt', height='170px')
                                  ),
                                  # LDT Accuracy
                                  box(
                                    width=6,
                                    title='Lexical Decision Accuracy (Acc)', status='danger', solidHeader=T,
                                    checkboxInput('check.acc', 'Match by Accuracy', 0),
                                    checkboxInput('acc.zscore', 'Z-Score', 1),
                                    radioButtons('acc.opt', 'Source(s)',
                                                 c('British Lexicon Project (BLP)'='blp', 'English Lexicon Project (ELP)'='elp'),
                                                 selected='blp',
                                                 inline=T),
                                    br(),
                                    uiOutput('acc.sl.choice'),
                                    textOutput('descr.acc'),
                                    plotOutput('plot.acc', height='170px')
                                  )
                                  
                                )),
                        # Match Results tab
                        tabItem(tabName='match_results',
                                fluidRow(
                                  box(title='Options', status='primary',
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
                                        column(12, checkboxInput('check.match.ignorefilters', 'Ignore Filters on Select & Filter Tab', 0)),
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
                              column(6, selectInput('vis.colour.opts', 'Colour', c('(None)', 'Target Match Word', 'Suggested Matches', 'Words Uploaded to Fetch Tab', 'Part of Speech', vis.cats), "Suggested Matches")),
                              column(6, uiOutput('vis.coloursource.choice'))
                            ),
                            br(),
                            fluidRow(
                              column(6, sliderInput('vis.opacity.sl', 'Point Opacity', value=0.85, min=0.1, max=1, step=0.05)),
                              column(6, sliderInput('vis.pointsize.sl', 'Point Size', value=4, min=1, max=10, step=1))
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

