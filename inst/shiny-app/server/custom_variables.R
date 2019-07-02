# stimuli input

output$cust_opts_inputfile_choice <- renderUI({
  fileInput("cust_opts_inputfile", "Choose File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".tsv",
                       ".xls",
                       ".xlsx",
                       ".xlsm"))
})

output$cust_opts_column_choice <- renderUI({
  if(!is.null(cust_df_raw())){
    col_opts <- colnames(cust_df_raw())
    selectInput('cust_opts_column', 'Name/Index of Column Containing Words', col_opts, selected=colnames(cust_df_raw())[1], width='100%')
  }
})

output$cust_opts_filehasheaders_choice <- renderUI({
  if(!is.null(input$cust_opts_inputfile)) {
    checkboxInput('cust_opts_filehasheaders', "File has Headers", T)
  } else {
    NULL
  }
})

# read file/text

cust_df_raw <- reactive({
  if (!is.null(input$cust_opts_inputfile)) {
    file_ext <- tools::file_ext(input$cust_opts_inputfile$datapath)
    if (file_ext == "csv") {
      read_csv(input$cust_opts_inputfile$datapath, col_names=input$cust_opts_filehasheaders)
    } else if (file_ext == "tsv") {
      read_tsv(input$cust_opts_inputfile$datapath, col_names=input$cust_opts_filehasheaders)
    } else if (file_ext %in% c("xls", "xlsx", "xlsm")) {
      readxl::read_excel(input$cust_opts_inputfile$datapath, sheet=1, col_names=input$cust_opts_filehasheaders)
    }
  } else {
    NULL
  }
})

output$cust_filename <- renderText({
  if (!is.null(input$cust_opts_inputfile)) {
    sprintf("Uploaded file: %s", input$cust_opts_inputfile$name)
  }
})


# target features

output$cust_opts_choice <- renderUI({
  if (input$cust_opts_all=="all") {
    NULL
  } else {
    custcols <- colnames(cust_df_raw())
    custcols <- custcols[custcols!=input$cust_opts_column]
    checkboxGroupInput('cust_opts', NULL, custcols, inline=T)
  }
})

# summarise the uploaded variables

output$cust_uploadedvars <- renderTable(na="-", {

  if (input$cust_opts_all=="all") {
    selcols <- colnames(cust_df_raw())
    selcols <- selcols[selcols!=input$cust_opts_column]
  } else {
    selcols <- colnames(select(cust_df_raw(), input$cust_opts))
    selcols <- selcols[selcols!=input$cust_opts_column]
  }

  tibble(
    Variable = sprintf("%s", selcols),
    Entries = as.integer(lapply(selcols, function(x) {
      cust_df_raw() %>%
        select(x) %>%
        na.omit() %>%
        nrow()
    })),
    Class = lapply(selcols, function(x) {
      class(cust_df_raw()[[x]])
    }),
    Min = as.numeric(lapply(selcols, function(x) {
      if (is.numeric(cust_df_raw()[[x]])) {
        cust_df_raw()[[x]] %>%
          min(na.rm=T) %>%
          round(2)
      } else {
        NA
      }
    })),
    Max = as.numeric(lapply(selcols, function(x) {
      if (is.numeric(cust_df_raw()[[x]])) {
        cust_df_raw()[[x]] %>%
          max(na.rm=T) %>%
          round(2)
      } else {
        NA
      }
    })),
    Mean = as.numeric(lapply(selcols, function(x) {
      if (is.numeric(cust_df_raw()[[x]])) {
        cust_df_raw()[[x]] %>%
          mean(na.rm=T) %>%
          round(2)
      } else {
        NA
      }
    })),
    SD = as.numeric(lapply(selcols, function(x) {
      if (is.numeric(cust_df_raw()[[x]])) {
        cust_df_raw()[[x]] %>%
          sd(na.rm=T) %>%
          round(2)
      } else {
        NA
      }
    }))
  )

})
