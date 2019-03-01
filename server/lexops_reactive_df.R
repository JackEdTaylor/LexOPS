# a reactive object with the lexops data in it
lexopsReact <- reactive({
  res <- lexops
  # add uploaded variables (if any)
  if (!is.null(input$cust.opts.inputfile)) {
    if (input$cust.opts.all=="all") {
      selcols <- colnames(cust_df_raw())
      selcols <- selcols[selcols!=input$cust.opts.column]
    } else {
      selcols <- colnames(select(cust_df_raw(), input$cust.opts))
      selcols <- selcols[selcols!=input$cust.opts.column]
    }
    targstringcolname <- input$cust.opts.column
    inputfile <- cust_df_raw() %>%
      rename_at(vars(selcols), ~ sprintf("custom.%s", selcols)) %>%
      rename(string = targstringcolname)
    res <- res %>%
      full_join(select(inputfile, c(sprintf("custom.%s", selcols), "string")), by="string")
  }
  # add "word" to database temporarily if unknown
  if (!input$matchstring %in% res$string) {
    res <- res %>%
      add_row(string = input$matchstring,
              Length = nchar(input$matchstring))
  }
  # calculate similarity measures for match tab (if any)
  tryCatch({
    if (matchboxes_N() >= 1) {
      for (i in 1:matchboxes_N()) {
        boxid <- boxid <- sprintf('matchbox_%i', i)
        boxv <- input[[sprintf('%s_vtype', boxid)]]
        boxopt <- input[[sprintf('%s.opt', boxid)]]
        # Orthographic Similarity
        if (boxv == "Orthographic Similarity") {
          column <- corpus_recode_columns(boxopt, boxv)
          if (boxopt == "ld") {
            res[[column]] <- vwr::levenshtein.distance(input$matchstring, res$string)
          }
          if (boxopt == "ldd") {
            res[[column]] <- vwr::levenshtein.damerau.distance(input$matchstring, res$string)
          }
        }
      }
    }
  },
  error = function(cond) {
    return(NULL)
  },
  warning=function(cond) {
    return(NULL)
  })
  
  res
})