# stimuli input

output$fetch_opts_inputfile_choice <- renderUI({
  if(input$fetch_inputtype == "file"){
    fileInput("fetch_opts_inputfile", "Choose File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv",
                         ".xls",
                         ".xlsx",
                         ".xlsm"))
  } else if(input$fetch_inputtype == "cp") {
    NULL
  }
})

output$fetch_opts_column_choice <- renderUI({
  if(input$fetch_inputtype == "file" & !is_null(fetch_df_raw())){
    col_opts <- colnames(fetch_df_raw())
    selectInput("fetch_opts_column", "Name/Index of Column Containing Words", col_opts, selected=colnames(fetch_df_raw())[1], width="100%")
  }
})

output$fetch_opts_inputtext_choice <- renderUI({
  if(input$fetch_inputtype == "file"){
    NULL
  } else if(input$fetch_inputtype == "cp") {
    textAreaInput("fetch_opts_inputtext", "Type or Copy and Paste Target Word Stimuli into the Box Below", width="100%", height="270px")
  }
})

output$fetch_opts_filehasheaders_choice <- renderUI({
  if(!is_null(input$fetch_opts_inputfile) & input$fetch_inputtype == "file") {
    checkboxInput("fetch_opts_filehasheaders", "File has Headers", T)
  } else {
    NULL
  }
})

output$fetch_textsep_choice <- renderUI({
  if(input$fetch_inputtype == "cp") {
    selectInput("fetch_textsep", "Separator", c("Newline"="\n", "Comma"=",", "Space"=" ", "Tab"="\t"))
  } else {
    NULL
  }
})

# read file/text
fetch_df_raw <- reactive({
  if (!is_null(input[["fetch_opts_inputfile"]]) & input$fetch_inputtype=="file") {
    file_ext <- tools::file_ext(input$fetch_opts_inputfile$datapath)
    if (file_ext == "csv") {
      read_csv(input$fetch_opts_inputfile$datapath, col_names=input$fetch_opts_filehasheaders)
    } else if (file_ext == "tsv") {
      read_tsv(input$fetch_opts_inputfile$datapath, col_names=input$fetch_opts_filehasheaders)
    } else if (file_ext %in% c("xls", "xlsx", "xlsm")) {
      readxl::read_excel(input$fetch_opts_inputfile$datapath, sheet=1, col_names=input$fetch_opts_filehasheaders)
    }
  } else {
    if (!is_null(input$fetch_opts_inputtext) & input$fetch_inputtype=="cp") {
      if (nchar(input$fetch_opts_inputtext)>0) {
        tibble("string"=unlist(str_split(input$fetch_opts_inputtext, input$fetch_textsep))) %>%
          filter(nchar(string)>0)
      }
    } else {
      NULL
    }
  }
})

output$fetch_filename <- renderText({
  if (!is_null(input$fetch_opts_inputfile) & input$fetch_inputtype == "file") {
    sprintf("Uploaded file: %s", input$fetch_opts_inputfile$name)
  }
})


# target features

output$fetch_includeorig_choice <- renderUI({
  if(input$fetch_inputtype == "file"){
    checkboxInput("fetch_includeorig", "Include original columns from file", value=F)
  }
})

# get a list of all the variables, and their measures and sources
vars_info <- lapply(colnames(LexOPS::lexops), function(v) {
  measure <- LexOPS::var_to_measure(v, first_cite = FALSE, title_caps = TRUE, standard_eval = TRUE)
  source <- LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)
  if (source == "") source <- "LexOPS"
  c(measure, source)
})
names(vars_info) <- colnames(lexops)
vars_info$string <- NULL
unique_measures <- unique(sapply(vars_info, function(i) i[1]))

lapply(1:length(unique_measures), function(i) {
  measure <- unique_measures[i]
  possible_vars <- vars_info[sapply(vars_info, function(i) i[1]==measure)]
  possible_sources <- sapply(possible_vars, function(i) i[2])
  possible_sources_flipped <- names(possible_sources)
  names(possible_sources_flipped) <- unname(possible_sources)

  output[[sprintf("fetch_opts_choice_%i", i)]] <- renderUI({
    if (input$fetch_opts_all=="some") {
      checkboxGroupInput(sprintf("fetch_opts_%i", i),
                         measure,
                         names(possible_sources),
                         inline=T)
    } else {
      NULL
    }

  })
})

# get fetch results

output$fetch_results_plsinput <- renderText({
  if (is_null(fetch_df_res())) {
    "Please input word stimuli..."
  } else {
    NULL
  }
})

fetch_targwordstringcolname <- reactive({
  if (input$fetch_inputtype == "file") {
    input$fetch_opts_column
  } else {
    "string"
  }
})

fetch_df_res <- reactive({
  if (!is_null(fetch_df_raw())) {
    # get list of selected features
    if (input$fetch_opts_all=="all") {
      sel_feats <- colnames(select(lexops_react(), -string))
    } else {
      sel_feats <- c()
      for (catnr in 1:length(unique_measures)) {
        sel_feats <- c(sel_feats, input[[sprintf("fetch_opts_%i", catnr)]])
      }
    }
    targwordstringcolname <- fetch_targwordstringcolname()
    # extract selected features
    lexops_f <- dplyr::rename(lexops_react(), !!targwordstringcolname:=string) %>%
      select(c(targwordstringcolname, sel_feats))
    # get the input from the file or copy-paste
    in_df <- fetch_df_raw()
    in_df[[targwordstringcolname]] <- as.character(in_df[[targwordstringcolname]])
    if (input$fetch_includeorig | input$fetch_inputtype == "cp") {
      in_df %>%
        left_join(lexops_f, by=targwordstringcolname)
    } else {
      in_df %>%
        select(input$fetch_opts_column) %>%
        left_join(lexops_f, by=targwordstringcolname)
    }
  } else {
    NULL
  }
})

# fetch results as datatable

output$fetch_df_res_dt <- DT::renderDataTable({
  if (!is_null(fetch_df_res())) {
    DT::datatable(fetch_df_res(), options=list(pageLength=10, scrollX=T))
  } else {
    NULL
  }
})

output$fetched_csv <- downloadHandler(
  filename = 'fetched.csv',
  content = function(file) {
    withProgress(message="Writing fetched data to .csv file...", value=1, {
      write.csv(fetch_df_res(), file, row.names = FALSE)
    })
  }
)

output$fetched_csv_choice <- renderUI({
  if (!is_null(fetch_df_res())) {
    column(12, downloadButton("fetched_csv", "Download Fetched Data"))
  } else {
    NULL
  }
})

output$fetch_tab_content <- renderUI({
  fluidRow(
    box(
      title='1) Stimuli Input', status='primary',
      collapsible=T, collapsed=F, width=12,
      fluidRow(
        column(12, radioButtons('fetch_inputtype', 'Input Type', c("File (.csv, .tsv, .xls, .xlsx)"="file", "Text Box (type/copy-paste the words in)"="cp"))),
        column(12, uiOutput('fetch_opts_inputfile_choice')),
        column(12, uiOutput('fetch_opts_inputtext_choice')),
        column(12, textOutput('fetch_filename')),
        column(12, uiOutput('fetch_opts_filehasheaders_choice')),
        column(12, br()),
        column(12, uiOutput('fetch_opts_column_choice')),
        column(12, uiOutput('fetch_textsep_choice'))
      )
    ),
    box(
      title='2) Choose Target Features', status='primary',
      collapsible=T, collapsed=F, width=12,
      fluidRow(
        column(12, uiOutput('fetch_includeorig_choice')),
        column(12, radioButtons('fetch_opts_all', NULL, c("Include all LexOPS Features"="all", "Select LexOPS Features Manually"="some"))),
        lapply(1:length(unique_measures), function(catnr) {
          column(12, uiOutput(sprintf("fetch_opts_choice_%i", catnr)))
        })
      )
    ),
    box(
      title='3) Results', status='primary',
      collapsible=T, collapsed=F, width=12,
      fluidRow(
        column(12, textOutput('fetch_results_plsinput')),
        column(12, uiOutput('fetched_csv_choice')),
        br(), br(), br(),
        column(12, DT::dataTableOutput('fetch_df_res_dt'))
      )
    )
  )
})
