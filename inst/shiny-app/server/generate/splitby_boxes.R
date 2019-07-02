# initialise number of boxes
gen_splitby_boxes_N <- reactiveVal(0)

# Counting N boxes
observeEvent(input$gen_splitby_add, {
  gen_splitby_boxes_N(gen_splitby_boxes_N() + 1)
})
observeEvent(input$gen_splitby_minus, {
  if (gen_splitby_boxes_N()>0) {
    gen_splitby_boxes_N(gen_splitby_boxes_N() - 1)
  }
})

# Display N boxes
observeEvent(gen_splitby_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf("gen_splitby_%i", i)
    if (i <= gen_splitby_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})

# Build boxes" UIs
lapply(1:25, function(i) {
  boxid <- sprintf("gen_splitby_%i", i)

  # source selection
  output[[sprintf("%s_v_source_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    if (is.null(measure)) {
      out <- NULL
    } else if (measure == "(None)") {
      out <- NULL
    } else if (measure == "Length") {
      out <- NULL
    } else if (measure == "Random") {
      out <- NULL
    } else {
      vars_sources <- names(lexops_react_var_measures())[lexops_react_var_measures()==measure] %>%
        sapply(function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)) %>%
        unname()
      out <- selectInput(sprintf("%s_v_source", boxid), "according to...", vars_sources)
    }
    out
  })

  # Define N levels
  output[[sprintf("%s_v_n_levels_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    if (is.null(measure) | measure == "(None)") {
      NULL
    } else {
      numericInput(sprintf("%s_v_n_levels", boxid), "into _ levels...", value = 2, min = 2, step = 1)
    }
  })

  # tolerance/levels selection
  output[[sprintf("%s_v_selection_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]
    n_levels <- input[[sprintf("%s_v_n_levels", boxid)]]

    if (is.null(n_levels) | measure=="Random" | (is.null(source) & measure!="Length")) {
      out <- NULL
    } else {

      if (measure=="Length") {
        var <- "Length"
      } else {
        possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
        possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
        var <- possible_vars[possible_vars_sources == source]
      }

      if (is.numeric(lexops_react()[[var]])) {
        sl <- LexOPS:::sensible_slider_vals(lexops_react()[[var]], n_levels=n_levels, is_tolerance=FALSE)
      } else {
        var_cats <- lexops_react() %>%
          dplyr::filter(!is.na(!!(dplyr::sym(var)))) %>%
          dplyr::group_by(!!(dplyr::sym(var))) %>%
          dplyr::summarise(n = dplyr::n()) %>%
          dplyr::arrange(desc(n)) %>%
          pull(!!(dplyr::sym(var)))
      }

      # build the selection UIs for each level
      out <- lapply(1:n_levels, function(lvl_i) {
        if (is.numeric(lexops_react()[[var]])) {
          if (input$preference_toleranceUI == "slider") {
            sliderInput(sprintf("%s_v_selection_%i", boxid, lvl_i), label = sprintf("Level %i boundaries...", lvl_i), min = sl$min, max = sl$max, value = sl$value[[lvl_i]], step = sl$step)
          } else {
            fluidRow(
              column(6, numericInput(sprintf("%s_v_selection_%i_1", boxid, lvl_i), label = sprintf("Level %i min", lvl_i), value = sl$value[[lvl_i]][1], step = sl$step)),
              column(6, numericInput(sprintf("%s_v_selection_%i_2", boxid, lvl_i), label = sprintf("Level %i max", lvl_i), value = sl$value[[lvl_i]][2], step = sl$step))
            )
          }
        } else {
          sel_nr <- if (lvl_i > length(var_cats)) length(var_cats) else lvl_i
          checkboxGroupInput(sprintf("%s_v_selection_%i", boxid, lvl_i), label = sprintf("Level %i categories...", lvl_i), choices = var_cats, selected = var_cats[sel_nr], inline=TRUE)
        }
      })

    }


    out
  })

  # plot the visualisation
  output[[sprintf("%s_v_plot", boxid)]] <- renderPlot({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]
    n_levels <- input[[sprintf("%s_v_n_levels", boxid)]]

    if (measure=="Length") {
      var <- "Length"
    } else {
      possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
      possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
      var <- possible_vars[possible_vars_sources == source]
    }
    shade_label <- sapply(1:n_levels, function(lvl_i) sprintf("%s%i", LETTERS[i], lvl_i), USE.NAMES = FALSE)

    selection <- if (input$preference_toleranceUI == "slider") {
      lapply(1:n_levels, function(lvl_i) input[[sprintf("%s_v_selection_%i", boxid, lvl_i)]])
    } else {
      lapply(1:n_levels, function(lvl_i) c(input[[sprintf("%s_v_selection_%i_1", boxid, lvl_i)]], input[[sprintf("%s_v_selection_%i_2", boxid, lvl_i)]]))
    }

    out <- LexOPS:::box_vis(var, box_type = "primary", tol = selection, shade_label = shade_label, df = lexops_react())

    out
  })

  # put the plot in a UI (this removes the whitespace if no plot is rendered)
  output[[sprintf("%s_v_plot_ui", boxid)]] <- renderUI({
    measure <- input[[sprintf("%s_v_measure", boxid)]]
    source <- input[[sprintf("%s_v_source", boxid)]]
    if (measure == "Random") {
      tags$p("Will create a random split in the stimuli. This is useful for non-stimulus related conditions (e.g. task) that still require matched stimuli.")
    } else if ((is.null(source) & measure != "Length") | measure == "(None)") {
      NULL
    } else {
      plotOutput(sprintf("%s_v_plot", boxid), height="170px")
    }
  })

})

# Put the UIs built above into their boxes
lapply(1:25, function(i) {
  boxid <- sprintf("gen_splitby_%i", i)
  output[[boxid]] <- renderUI({
    box(title=LETTERS[i], width=12, status="primary", solidHeader=T,
        selectInput(sprintf("%s_v_measure", boxid), "Split by...", c("(None)", "Random", unname(lexops_react_var_measures()) )),
        uiOutput(sprintf("%s_v_source_ui", boxid)),
        uiOutput(sprintf("%s_v_n_levels_ui", boxid)),
        uiOutput(sprintf("%s_v_selection_ui", boxid)),
        uiOutput(sprintf("%s_v_plot_ui", boxid)),
        id = boxid
    )
  })
})
