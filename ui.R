library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
library(plotly)
library(viridis)
library(DT)

# Visualilsation vector categories
vis.cats <- c('Length', 'Syllables', 'Word Frequency', 'Bigram Frequency', 'Orthographic Neighbourhood', 'Familiarity', 'Age of Acquisition', 'Concreteness', 'Arousal', 'Valence', 'Dominance', 'Imageability', 'Semantic Size', 'Semantic Gender', 'Lexical Decision Response Time', 'Lexical Decision Accuracy')

shinyUI(dashboardPage(skin='black',
                    
  dashboardHeader(title='LexOPS',
                  titleWidth=200),
  
  dashboardSidebar(width=200,
                   sidebarMenu(
                     style = 'position: fixed; overflow: visible;',  # Stationary sidebar while scrolling
                     menuItem('Generate', tabname='generate', icon=icon('cogs')),
                     br(),
                     menuItem('Match', tabname='match', icon=icon('search'),
                              startExpanded = T,
                              textInput('string', 'Word:', 'thicket', width="100%"),  # must explicitly give width of UI items in sidebar to avoid overflowing
                              column(1, textOutput('nrow.results')),
                              br(), br(),
                              menuSubItem('Options', tabName="match_options", icon=icon('sliders-h')),
                              menuSubItem('Suggested Matches', tabName="match_results", icon=icon('sort-amount-down'))),
                     br(),
                     menuItem('Fetch', tabname='fetch', icon=icon('file-import')),
                     br(),
                     menuItem('Visualise', tabName="visualise", icon=icon('chart-bar')),
                     menuItem('Info', tabName='info', icon=icon('info-circle'))
                     )),
  
  dashboardBody(
    
    # set 'info' status to a nice purple, and change default purple box colour to same shade
    # reduce size of the fontawesome icons used in the feature headings
    # change headings' icon colour to white instead of grey
    # change LexOPS header to be left-aligned
    tags$style(HTML(".box.box-solid.box-info>.box-header {
                    color:#fff;
                    background:#641e68
                    }
                    
                    .box.box-solid.box-info{
                    border-bottom-color:#641e68;
                    border-left-color:#641e68;
                    border-right-color:#641e68;
                    border-top-color:#641e68;
                    }
                    
                    .bg-purple {
                    background-color:#641e68!important
                    }
                    
                    .icon-large>.fa { font-size: 45px; }
                    
                    .small-box .icon-large {
                    color:rgba(255, 255, 255, 0.9);
                    bottom:-5px;
                    right:10px
                    }
                    
                    .main-header .logo {
                    text-align:left;
                    }")),
    
    tabItems(
    # Options tab
    tabItem(tabName='match_options',
            fluidRow(
              # Lexical Header
              valueBox("Lexical Features", subtitle=NULL, width = 12, color='light-blue', icon=icon("book")),
              # Frequency box
              box(
                width=6,
                title='Word Frequency', status='primary', solidHeader=T,
                checkboxInput('check.frequency', 'Match by Frequency', 1),
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
                checkboxInput('check.partofspeech', 'Match by Part of Speech', 1),
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
                checkboxInput('check.length', 'Match by Length (Number of Characters)', 1),
                br(),
                sliderInput('length.sl', NULL, value=c(0, 0), min=-5, max=5, step=1),
                textOutput('descr.length'),
                plotOutput('plot.length', height='170px')
              ),
              # Bigram Frequency box
              box(
                width=6,
                title='Bigram Frequency (BG.Frequency)', status='warning', solidHeader=T,
                checkboxInput('check.bgfreq', 'Match by Bigram Frequency', 0),
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
                radioButtons('syllables.opt', 'Source',
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
                radioButtons('phonemes.opt', 'Source',
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
                title='Rhyme Sound', status='info', solidHeader=T,
                checkboxInput('check.rhyme', 'Match by Rhyme Sound', 0),
                radioButtons('rhyme.opt', 'Source',
                             c('CMU Pronouncing Dictionary'='cmu'),
                             'cmu',
                             inline=T),
                uiOutput('manual.pron.rhyme.choice'),
                textOutput('descr.rhyme'),
                br()
                ),
              # Phonological Neighbourhood Size
              box(
                width=6,
                title='Phonological Neighbourhood Size (PN)', status='info', solidHeader=T,
                checkboxInput('check.pn', 'Match by PN', 0),
                checkboxInput('pn.log', 'Log Transform', 0),
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
              # Semantic Header
              valueBox("Semantic Features", subtitle=NULL, width = 12, color='green', icon=icon("lightbulb")),
              # Familiarity box
              box(
                width=6,
                title='Familiarity (FAM)', status='success', solidHeader=T,
                checkboxInput('check.fam', 'Match by Familiarity Ratings', 0),
                radioButtons('fam.opt', 'Source',
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
                radioButtons('aoa.opt', 'Source',
                             c('Kuperman et al. (2012)'='kuperman', 'Glasgow Norms'='gn'),
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
                radioButtons('cnc.opt', 'Source',
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
                radioButtons('imag.opt', 'Source',
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
                radioButtons('arou.opt', 'Source',
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
                radioButtons('val.opt', 'Source',
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
                radioButtons('dom.opt', 'Source',
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
                radioButtons('size.opt', 'Source',
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
                radioButtons('gen.opt', 'Source',
                             c('Glasgow Norms'='gn'),
                             selected='gn',
                             inline=T),
                br(),
                sliderInput('gen.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
                textOutput('descr.gen'),
                plotOutput('plot.gen', height='170px')
              ),
              # Behavioural Header
              valueBox("Behavioural Features", subtitle=NULL, width = 12, color='red', icon=icon("stopwatch")),
              # LDT RT box
              box(
                width=6,
                title='Lexical Decision Response Time (RT)', status='danger', solidHeader=T,
                checkboxInput('check.rt', 'Match by RT', 0),
                checkboxInput('rt.zscore', 'Z-Score', 1),
                radioButtons('rt.opt', 'Source',
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
                radioButtons('acc.opt', 'Source',
                             c('British Lexicon Project (BLP)'='blp', 'English Lexicon Project (ELP)'='elp'),
                             selected='blp',
                             inline=T),
                br(),
                uiOutput('acc.sl.choice'),
                textOutput('descr.acc'),
                plotOutput('plot.acc', height='170px')
              )
            
            )),
    # Results tab
    tabItem(tabName='match_results',
            selectInput('results.format', NULL, c('Raw Values'='rv', 'Distances (Absolute Difference)'='dist', 'Differences'='diff'), selected='dist'),
            DT::dataTableOutput('results')
            ),
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
          column(6, selectInput('vis.colour.opts', 'Colour', c('(None)', 'Target Word', 'Suggested Matches', 'Part of Speech', vis.cats), "Suggested Matches")),
          column(6, uiOutput('vis.coloursource.choice'))
        ),
        br(),
        fluidRow(
          column(6, sliderInput('vis.opacity.sl', 'Point Opacity', value=0.85, min=0.1, max=1, step=0.05)),
          column(6, sliderInput('vis.pointsize.sl', 'Point Size', value=4, min=1, max=10, step=1))
        )
        )),
            
            fluidRow(
                plotlyOutput('visualiseplotly') %>%
                  withSpinner()
            )),
    # Info tab
    tabItem(tabName='info',
            downloadButton('wordmatchmaker.csv', 'Word Match Maker Database')
    )
  )
  
  )
)
)
