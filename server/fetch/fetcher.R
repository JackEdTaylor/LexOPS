# stimuli input

output$fetch.opts.inputfile.choice <- renderUI({
  if(input$fetch.inputtype == "file"){
    fileInput("fetch.opts.inputfile", "Choose File",
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv",
                         ".tsv",
                         ".xls",
                         ".xlsx",
                         ".xlsm"))
  } else if(input$fetch.inputtype == "cp") {
    NULL
  }
})

output$fetch.opts.column.choice <- renderUI({
  if(input$fetch.inputtype == "file" & !is.null(fetch_df_raw())){
    col_opts <- colnames(fetch_df_raw())
    selectInput('fetch.opts.column', 'Name/Index of Column Containing Words', col_opts, selected=colnames(fetch_df_raw())[1], width='100%')
  }
})

output$fetch.opts.inputtext.choice <- renderUI({
  if(input$fetch.inputtype == "file"){
    NULL
  } else if(input$fetch.inputtype == "cp") {
    textAreaInput("fetch.opts.inputtext", "Copy and Paste Target Word Stimuli into the Box Below", width="100%", height='270px')
  }
})

output$fetch.opts.filehasheaders.choice <- renderUI({
  if(!is.null(input$fetch.opts.inputfile) & input$fetch.inputtype == "file") {
    checkboxInput('fetch.opts.filehasheaders', "File has Headers", T)
  } else {
    NULL
  }
})

output$fetch.textsep.choice <- renderUI({
  if(input$fetch.inputtype == "cp") {
    selectInput('fetch.textsep', 'Separator', c('Newline'='\n', 'Comma'=',', 'Space'=' ', 'Tab'='\t'))
  } else {
    NULL
  }
})

# read file/text
fetch_df_raw <- reactive({
  if (!is.null(input[["fetch.opts.inputfile"]]) & input$fetch.inputtype=="file") {
    file_ext <- tools::file_ext(input$fetch.opts.inputfile$datapath)
    if (file_ext == "csv") {
      read_csv(input$fetch.opts.inputfile$datapath, col_names=input$fetch.opts.filehasheaders)
    } else if (file_ext == "tsv") {
      read_tsv(input$fetch.opts.inputfile$datapath, col_names=input$fetch.opts.filehasheaders)
    } else if (file_ext %in% c("xls", "xlsx", "xlsm")) {
      gdata::read.xls(input$fetch.opts.inputfile$datapath, sheet=1, header=input$fetch.opts.filehasheaders)
    }
  } else {
    if (!is.null(input$fetch.opts.inputtext) & input$fetch.inputtype=="cp") {
      if (nchar(input$fetch.opts.inputtext)>0) {
        tibble('string'=unlist(str_split(input$fetch.opts.inputtext, input$fetch.textsep))) %>%
          filter(nchar(string)>0)
      }
    } else {
      NULL
    }
  }
})

output$fetch.filename <- renderText({
  if (!is.null(input$fetch.opts.inputfile) & input$fetch.inputtype == "file") {
    sprintf("Uploaded file: %s", input$fetch.opts.inputfile$name)
  }
})


# target features

output$fetch.includeorig.choice <- renderUI({
  if(input$fetch.inputtype == "file"){
    checkboxInput('fetch.includeorig', 'Include original columns from file', value=F)
  }
})

lapply(1:length(vis.cats), function(catnr) {
  catname <- vis.cats[catnr]
  output[[sprintf("fetch.opts.choice.%i", catnr)]] <- renderUI({
    if (input$fetch.opts.all=="some") {
      checkboxGroupInput(sprintf("fetch.opts.%i", catnr),
                         catname,
                         vis.opt.2.source(catname),
                         inline=T)
    } else {
      NULL
    }
    
  })
})

# get fetch results

output$fetch.results.plsinput <- renderText({
  if (is.null(fetch_df_res())) {
    "Please input word stimuli..."
  } else {
    NULL
  }
})

fetch_df_res <- reactive({
  if (!is.null(fetch_df_raw())) {
    # get list of selected features
    if (input$fetch.opts.all=="all") {
      sel_feats <- colnames(select(lexops, -string))
    } else {
      sel_feats <- c()
      for (catnr in 1:length(vis.cats)) {
        sel_feats <- c(sel_feats, input[[sprintf("fetch.opts.%i", catnr)]])
      }
    }
    if (input$fetch.inputtype == "file") {
      targwordstringcolname <- input$fetch.opts.column
    } else {
      targwordstringcolname <- "string"
    }
    # extract selected features
    lexops_f <- plyr::rename(lexops, c("string"=targwordstringcolname)) %>%
      select(c(targwordstringcolname, sel_feats))
    # get the input from the file or copy-paste
    in_df <- fetch_df_raw()
    in_df[[targwordstringcolname]] <- as.character(in_df[[targwordstringcolname]])
    if (input$fetch.includeorig | input$fetch.inputtype == "cp") {
      in_df %>%
        left_join(lexops_f, by=targwordstringcolname)
    } else {
      in_df %>%
        select(input$fetch.opts.column) %>%
        left_join(lexops_f, by=targwordstringcolname)
    }
  } else {
    NULL
  }
})

# fetch results as datatable

output$fetch_df_res_dt <- DT::renderDataTable({
  if (!is.null(fetch_df_res())) {
    DT::datatable(fetch_df_res(), options=list(pageLength=10, scrollX=T))
  } else {
    NULL
  }
})
  
output$fetched.csv <- downloadHandler(
  filename = 'fetched.csv',
  content = function(file) {
    write.csv(fetch_df_res(), file, row.names = FALSE)
  }
)

output$fetched.csv.choice <- renderUI({
  if (!is.null(fetch_df_res())) {
    column(12, downloadButton('fetched.csv', 'Download Fetched Data'))
  } else {
    NULL
  }
})
