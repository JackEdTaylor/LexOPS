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
    old20val <- as.numeric(vwr::old20(input$matchstring, lexops$string))
    res <- res %>%
      add_row(string = input$matchstring,
              Length = nchar(input$matchstring),
              ON.OLD20 = old20val,
              ON.Log_OLD20 = log(old20val))
  }
  
  # reactively calculate requested variables for target word in match tab
  if (!is.null(input$matchstring)) {
    # Orthographic Similarity
    for (measure in c("ld", "ldd")) {
      column <- corpus_recode_columns(measure, "Orthographic Similarity")
      if (measure=="ld") {
        res[[column]] <- as.integer(vwr::levenshtein.distance(input$matchstring, res$string))
      } else if (measure=="ldd") {
        res[[column]] <- as.integer(vwr::levenshtein.damerau.distance(input$matchstring, res$string))
      }
    }
    # Phonological Similarity - CMU
    matchstring_pron <- res$CMU.1letter[res$string==input$matchstring]
    for (measure in c("ld", "ldd")) {
      column <- corpus_recode_columns(measure, "Phonological Similarity", phonological_source = "cmu")
      if (measure=="ld") {
        res[[column]] <- as.integer(vwr::levenshtein.distance(matchstring_pron, res$CMU.1letter))
      } else if (measure=="ldd") {
        res[[column]] <- as.integer(vwr::levenshtein.damerau.distance(matchstring_pron, res$CMU.1letter))
      }
    }
    # Phonological Similarity - eSpeak
    matchstring_pron <- res$eSpeak.br_1letter[res$string==input$matchstring]
    for (measure in c("ld", "ldd")) {
      column <- corpus_recode_columns(measure, "Phonological Similarity", phonological_source = "espeak")
      if (measure=="ld") {
        res[[column]] <- as.integer(vwr::levenshtein.distance(matchstring_pron, res$eSpeak.br_1letter))
      } else if (measure=="ldd") {
        res[[column]] <- as.integer(vwr::levenshtein.damerau.distance(matchstring_pron, res$eSpeak.br_1letter))
      }
    }
    
  }
  
  res
})