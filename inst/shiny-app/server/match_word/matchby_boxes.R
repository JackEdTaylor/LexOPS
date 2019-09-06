# initialise number of boxes
match_matchby_boxes_N <- reactiveVal(0)

# Counting N boxes
observeEvent(input$match_matchby_add, {
  match_matchby_boxes_N(match_matchby_boxes_N() + 1)
})
observeEvent(input$match_matchby_minus, {
  if (match_matchby_boxes_N()>0) {
    match_matchby_boxes_N(match_matchby_boxes_N() - 1)
  }
})

# Display N boxes
observeEvent(match_matchby_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf("match_matchby_%i", i)
    if (i <= match_matchby_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})

# Build boxes" UIs
lapply(1:25, function(i) {
  boxid <- sprintf("match_matchby_%i", i)

  # source selection
  output[[sprintf("%s_v_source_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    if (is.null(measure)) {
      out <- NULL
    } else if (measure == "(None)") {
      out <- NULL
    } else if (measure == "Length") {
      out <- NULL
    } else if (grepl("^custom.", measure)) {
      out <- NULL
    } else {
      vars_sources <- names(lexops_react_var_measures())[lexops_react_var_measures()==measure] %>%
        sapply(function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)) %>%
        unname()
      out <- selectInput(sprintf("%s_v_source", boxid), "according to...", vars_sources)
    }
    out
  })

  # tolerance/levels selection
  output[[sprintf("%s_v_selection_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]

    if (is.null(source) & measure!="Length" & !grepl("^custom.", measure)) {
      out <- NULL
    } else {
      if (measure=="Length") {
        var <- "Length"
      } else if (grepl("^custom.", measure)) {
        var <- measure
      } else {
        possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
        possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
        var <- possible_vars[possible_vars_sources == source]
      }
      out <- if (is.numeric(lexops_react()[[var]])) {
        sl <- LexOPS:::sensible_slider_vals(lexops_react()[[var]], n_levels=1, is_tolerance=TRUE)

        if (input$preference_toleranceUI == "slider") {
          sliderInput(sprintf("%s_v_selection", boxid), label = "with a tolerance of...", min = sl$min, max = sl$max, value = sl$value, step = sl$step)
        } else {
          fluidRow(
            column(6, numericInput(sprintf("%s_v_selection_1", boxid), label = "tolerance min", value = sl$value[1], step = sl$step)),
            column(6, numericInput(sprintf("%s_v_selection_2", boxid), label = "tolerance max", value = sl$value[2], step = sl$step))
          )
        }
      } else {
        # no selection for categorical matching; control for exactly
        NULL
      }
    }
    out
  })

  # plot the visualisation
  output[[sprintf("%s_v_plot", boxid)]] <- renderPlot({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]

    if (measure=="Length") {
      var <- "Length"
    } else if (grepl("^custom.", measure)) {
      var <- measure
    } else {
      possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
      possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
      var <- possible_vars[possible_vars_sources == source]
    }
    out <- if (is.numeric(lexops_react()[[var]])) {

      selection <- if (input$preference_toleranceUI == "slider") {
        input[[sprintf("%s_v_selection", boxid)]]
      } else {
        c(input[[sprintf("%s_v_selection_1", boxid)]], input[[sprintf("%s_v_selection_2", boxid)]])
      }

      match_string <- if (is.null(input$match_string)) NA else input$match_string
      LexOPS:::box_vis(var, box_type = "warning", tol = selection, match_string = match_string, df = lexops_react())
    } else {
      # no selection for categorical matching; control for exactly
      LexOPS:::box_vis(var, box_type = "warning", df = lexops_react())
    }

    out
  })

  # put the plot in a UI (this removes the whitespace if no plot is rendered)
  output[[sprintf("%s_v_plot_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]
    if ((is.null(source) & measure != "Length" & !grepl("^custom.", measure)) | measure == "(None)") {
      NULL
    } else {
      plotOutput(sprintf("%s_v_plot", boxid), height="170px")
    }
  })

})

# Put the UIs built above into their boxes
lapply(1:25, function(i) {
  boxid <- sprintf("match_matchby_%i", i)
  output[[boxid]] <- renderUI({
    box(title=i, width=12, status="warning", solidHeader=T,
        selectInput(sprintf("%s_v_measure", boxid), "Match by...", c("(None)", unname(lexops_react_var_measures()) )),
        uiOutput(sprintf("%s_v_source_ui", boxid)),
        uiOutput(sprintf("%s_v_selection_ui", boxid)),
        uiOutput(sprintf("%s_v_plot_ui", boxid), height="170px"),
        id = boxid
    )
  })
})
