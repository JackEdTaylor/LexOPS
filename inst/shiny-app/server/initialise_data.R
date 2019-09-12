lexops_react <- reactive({
  out <- LexOPS::lexops

  # add custom variables
  if (!is.null(input$cust_opts_inputfile)) {
    if (input$cust_opts_all=="all") {
      selcols <- colnames(cust_df_raw())
      selcols <- selcols[selcols!=input$cust_opts_column]
    } else {
      selcols <- colnames(select(cust_df_raw(), input$cust_opts))
      selcols <- selcols[selcols!=input$cust_opts_column]
    }
    targstringcolname <- input$cust_opts_column
    inputfile <- cust_df_raw() %>%
      rename_at(vars(selcols), ~ sprintf("custom.%s", selcols)) %>%
      rename(string = targstringcolname)
    out <- out %>%
      full_join(select(inputfile, c(sprintf("custom.%s", selcols), "string")), by="string")
  }

  out

})

lexops_react_vars <- reactive({
  non_vars <- c("string", "CMU.1letter", "eSpeak.br_1letter", "eSpeak.br_IPA")
  colnames(lexops_react() )[!colnames(lexops_react() ) %in% non_vars]
})

lexops_react_var_measures <- reactive({
  out <- lexops_react_vars() %>%
    sapply(function(v) LexOPS::var_to_measure(v, first_cite = FALSE, title_caps = TRUE, include_pronunciations = FALSE, standard_eval = TRUE))
  if (!is.null(input$cust_opts_inputfile)) out <- c(out, colnames(lexops_react())[grepl("^custom.", colnames(lexops_react()))])
  out
})

lexops_react_var_sources <- reactive({
  out <- lexops_react_vars() %>%
    sapply(function(v) LexOPS::var_to_source(v, first_cite = FALSE, title_caps = TRUE, include_pronunciations = FALSE, standard_eval = TRUE))

  out
})

# link to download
output$full_dataset_download <- downloadHandler(
  filename = 'LexOPS.csv',
  content = function(file) {
    withProgress(message="Writing full dataset to .csv file...", value=1, {
      write.csv(lexops_react(), file, row.names = FALSE)
    })
  }
)
