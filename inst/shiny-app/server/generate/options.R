observeEvent(input$gen_stim_n_all, {
  if (input$gen_stim_n_all) {
    shinyjs::disable("gen_stim_n")
  } else {
    shinyjs::enable("gen_stim_n")
  }
})

output$gen_match_null_ui <- renderUI({
  # get the possible conditions
  conditions <- if (gen_splitby_boxes_N() > 0) {
    lapply(1:gen_splitby_boxes_N(), function(i) {
      boxid <- sprintf("gen_splitby_%i", i)
      box_letter <- LETTERS[i]
      n_levels <- input[[sprintf("%s_v_n_levels", boxid)]]
      sprintf("%s%i", box_letter, 1:n_levels)
    }) %>%
      expand.grid(stringsAsFactors = FALSE) %>%
      apply(1, paste, collapse="_")
  } else {
    NULL
  }

  selectInput("gen_match_null", "Match Null (Condition which stimuli will be matched relative to, i.e. within 0.1 and -0.1 Zipf of this condition)",
              c(
                conditions,
                "Balanced (use each condition as a control null an equal number of times, in a random order)" = "balanced",
                "Random (select control nulls entirely randomly)" = "random"
              ), selected = "balanced", width = "100%")
})

# in case the results are created before the gen_match_null_ui is rendered, assume the default
gen_match_null <- reactive({
  if (is.null(input$gen_match_null)) {
    "balanced"
  } else {
    input$gen_match_null
  }
})
