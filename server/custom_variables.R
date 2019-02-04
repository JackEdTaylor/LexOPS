# stimuli input

output$cust.opts.inputfile.choice <- renderUI({
  fileInput("cust.opts.inputfile", "Choose File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       ".tsv",
                       ".xls",
                       ".xlsx",
                       ".xlsm"))
})

output$cust.opts.column.choice <- renderUI({
  if(!is.null(cust_df_raw())){
    col_opts <- colnames(cust_df_raw())
    selectInput('cust.opts.column', 'Name/Index of Column Containing Words', col_opts, selected=colnames(cust_df_raw())[1], width='100%')
  }
})

output$cust.opts.filehasheaders.choice <- renderUI({
  if(!is.null(input$cust.opts.inputfile)) {
    checkboxInput('cust.opts.filehasheaders', "File has Headers", T)
  } else {
    NULL
  }
})

# read file/text

cust_df_raw <- reactive({
  if (!is.null(input$cust.opts.inputfile)) {
    file_ext <- tools::file_ext(input$cust.opts.inputfile$datapath)
    if (file_ext == "csv") {
      read_csv(input$cust.opts.inputfile$datapath, col_names=input$cust.opts.filehasheaders)
    } else if (file_ext == "tsv") {
      read_tsv(input$cust.opts.inputfile$datapath, col_names=input$cust.opts.filehasheaders)
    } else if (file_ext %in% c("xls", "xlsx", "xlsm")) {
      readxl::read_excel(input$cust.opts.inputfile$datapath, sheet=1, col_names=input$cust.opts.filehasheaders)
    }
  } else {
    NULL
  }
})

output$cust.filename <- renderText({
  if (!is.null(input$cust.opts.inputfile)) {
    sprintf("Uploaded file: %s", input$cust.opts.inputfile$name)
  }
})


# target features

output$cust.opts.choice <- renderUI({
  if (input$cust.opts.all=="all") {
    NULL
  } else {
    custcols <- colnames(cust_df_raw())
    custcols <- custcols[custcols!=input$cust.opts.column]
    checkboxGroupInput('cust.opts', NULL, custcols, inline=T)
  }
})

# summarise the uploaded variables

output$cust.uploadedvars <- renderTable(na="-", {
  
  if (input$cust.opts.all=="all") {
    selcols <- colnames(cust_df_raw())
    selcols <- selcols[selcols!=input$cust.opts.column]
  } else {
    selcols <- colnames(select(cust_df_raw(), input$cust.opts))
    selcols <- selcols[selcols!=input$cust.opts.column]
  }
  
  tibble(
    Variable = sprintf("custom.%s", selcols),
    Entries = as.integer(lapply(selcols, function(x) {
      cust_df_raw() %>%
        select(x) %>%
        na.omit() %>%
        nrow()
      })),
    Class = lapply(selcols, function(x) {
      class(cust_df_raw()[[x]])
      }),
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
