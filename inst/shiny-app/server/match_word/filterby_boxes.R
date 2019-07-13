# initialise number of boxes
match_filterby_boxes_N <- reactiveVal(0)

# Counting N boxes
observeEvent(input$match_filterby_add, {
  match_filterby_boxes_N(match_filterby_boxes_N() + 1)
})
observeEvent(input$match_filterby_minus, {
  if (match_filterby_boxes_N()>0) {
    match_filterby_boxes_N(match_filterby_boxes_N() - 1)
  }
})

# Display N boxes
observeEvent(match_filterby_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf("match_filterby_%i", i)
    if (i <= match_filterby_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})

# Build boxes" UIs
lapply(1:25, function(i) {
  boxid <- sprintf("match_filterby_%i", i)

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
        sl <- LexOPS:::sensible_slider_vals(lexops_react()[[var]], n_levels=1, is_tolerance=FALSE)

        if (input$preference_toleranceUI == "slider") {
          sliderInput(sprintf("%s_v_selection", boxid), label = "including variables within...", min = sl$min, max = sl$max, value = sl$value, step = sl$step)
        } else {
          fluidRow(
            column(6, numericInput(sprintf("%s_v_selection_1", boxid), label = "filter min", value = sl$value[1], step = sl$step)),
            column(6, numericInput(sprintf("%s_v_selection_2", boxid), label = "filter max", value = sl$value[2], step = sl$step))
          )
        }
      } else {
        var_cats <- lexops_react() %>%
          dplyr::filter(!is.na(!!(dplyr::sym(var)))) %>%
          dplyr::group_by(!!(dplyr::sym(var))) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::arrange(desc(n)) %>%
          pull(!!(dplyr::sym(var)))
        checkboxGroupInput(sprintf("%s_v_selection", boxid), label = "including variables in the categories of...", choices = var_cats, selected = var_cats[1], inline=TRUE)
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

    selection <- if (input$preference_toleranceUI == "slider") {
      input[[sprintf("%s_v_selection", boxid)]]
    } else {
      c(input[[sprintf("%s_v_selection_1", boxid)]], input[[sprintf("%s_v_selection_2", boxid)]])
    }

    out <- LexOPS:::box_vis(var, box_type = "info", tol = selection, match_string = input$match_string, shade_relative = FALSE, df = lexops_react())

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
  boxid <- sprintf("match_filterby_%i", i)
  output[[boxid]] <- renderUI({
    box(title=i, width=12, status="info", solidHeader=T,
        selectInput(sprintf("%s_v_measure", boxid), "Filter by...", c("(None)", unname(lexops_react_var_measures()) )),
        uiOutput(sprintf("%s_v_source_ui", boxid)),
        uiOutput(sprintf("%s_v_selection_ui", boxid)),
        uiOutput(sprintf("%s_v_plot_ui", boxid), height="170px"),
        id = boxid
    )
  })
})
