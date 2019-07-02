match_filter_opts_react <- reactive({
  if (match_filterby_boxes_N() > 0) {
    lapply(1:match_filterby_boxes_N(), function(i) {
      # collect input data about all the controls
      boxid <- sprintf("match_filterby_%i", i)
      measure <- input[[sprintf("%s_v_measure", boxid)]]
      source <- input[[sprintf("%s_v_source", boxid)]]

      if (measure=="Length") {
        var <- "Length"
      } else {
        possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
        possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
        var <- possible_vars[possible_vars_sources == source]
      }

      selection <- if (input$preference_toleranceUI == "slider" | !is.numeric(lexops_react()[[var]])) {
        input[[sprintf("%s_v_selection", boxid)]]
      } else {
        c(input[[sprintf("%s_v_selection_1", boxid)]], input[[sprintf("%s_v_selection_2", boxid)]])
      }

      # return a list with everything we need
      list(var = var, selection = selection)
    })
  } else {
    NA
  }
})

match_matchby_opts_react <- reactive({
  if (match_matchby_boxes_N() > 0) {
    lapply(1:match_matchby_boxes_N(), function(i) {
      # collect input data about all the controls
      boxid <- sprintf("match_matchby_%i", i)
      measure <- input[[sprintf("%s_v_measure", boxid)]]
      source <- input[[sprintf("%s_v_source", boxid)]]

      if (measure=="Length") {
        var <- "Length"
      } else {
        possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
        possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
        var <- possible_vars[possible_vars_sources == source]
      }

      selection <- if (input$preference_toleranceUI == "slider" | !is.numeric(lexops_react()[[var]])) {
        input[[sprintf("%s_v_selection", boxid)]]
      } else {
        c(input[[sprintf("%s_v_selection_1", boxid)]], input[[sprintf("%s_v_selection_2", boxid)]])
      }

      # return a list with everything we need
      list(var = var, selection = selection)
    })
  } else {
    NA
  }
})

matched_stim <- reactive({

  # get the filters
  filter_opts <- match_filter_opts_react()

  # get the controls
  match_opts <- match_matchby_opts_react()

  df <- lexops_react()

  # filters
  if (match_filterby_boxes_N() > 0) {
    for (i in 1:match_filterby_boxes_N()) {
      filt_var <- filter_opts[[i]]$var
      filt_sel <- filter_opts[[i]]$selection
      if (is.numeric(df[[filt_var]])) {
        # numeric filter
        df <- df %>%
          dplyr::filter(dplyr::between(!!(dplyr::sym(filt_var)), filt_sel[1], filt_sel[2]))
      } else {
        # categorical filter
        df <- df %>%
          dplyr::filter(!!(dplyr::sym(filt_var)) %in% filt_sel)
      }
    }
  }

  # match the target word
  if (match_matchby_boxes_N() > 0) {
    # wrangle the list of match options to a LexOPS-friendly format
    match_vars <- lapply(1:match_matchby_boxes_N(), function(i) {
      match_var <- match_opts[[i]]$var
      if (is.numeric(lexops_react()[[match_var]])) {
        selection <- match_opts[[i]]$selection
        c(match_var, selection[1], selection[2])
      } else {
        match_var
      }
    })
    # do the matching
    df <- df %>%
      LexOPS::match_word(target = input$match_string, vars = match_vars, filter = input$match_tolerance_filter)
  }

  df

})

# put in a data table
output$matched_stim_dt <- DT::renderDataTable({
  DT::datatable(matched_stim(), options=list(pageLength=25, scrollX=T), rownames=F)
})

# link to download
output$matched_stim_download <- downloadHandler(
  filename = 'matched_stimuli.csv',
  content = function(file) {
    withProgress(message="Writing stimuli to .csv file...", value=1, {
      write.csv(matched_stim(), file, row.names = FALSE)
    })
  }
)
