# get screen height using java to maximise plot within window
screenheight <- reactive({
  input$dimension[2]
})

output$visualise_tab_content <- renderUI({
  measures <- unique(unname(lexops_react_var_measures()))
  fluidRow(
    box(
      title='Plot Controls', status='primary',
      collapsible = T, width=12,
      fluidRow(
        column(6, selectInput('vis_x_opt', 'X Axis', measures, "Frequency in Zipf")),
        column(6, uiOutput('vis_x_source_ui'))
      ),
      fluidRow(
        column(6, selectInput('vis_y_opt', 'Y Axis', measures, "Lexical Decision RT")),
        column(6, uiOutput('vis_y_source_ui'))
      ),
      fluidRow(
        column(6, selectInput('vis_z_opt', 'Z Axis', c('(None)', measures))),
        column(6, uiOutput('vis_z_source_ui'))
      ),
      fluidRow(
        column(6, selectInput('vis_colour_opt', 'Colour', c('(None)', 'Generated Stimuli', 'Generated Stimuli Condition', 'Target Match Word', 'Suggested Matches', 'Words Uploaded to Fetch Tab', measures), "(None)")),
        column(6, uiOutput('vis_colour_source_ui'))
      ),
      br(),
      fluidRow(
        column(6, sliderInput('vis_opacity_sl', 'Point Opacity', value=0.85, min=0.1, max=1, step=0.05)),
        column(6, sliderInput('vis_pointsize_sl', 'Point Size', value=4, min=1, max=10, step=1))
      ),
      br(),
      fluidRow(
        column(6, colourInput('vis_bgcolour', 'Background Colour', value="black")),
        column(6, colourInput('vis_textcolour', 'Text Colour', value="white"))
      ),
      br(),
      br(),
      fluidRow(
        column(12, align="center", actionButton('vis_generateplot', 'Generate Plot', icon=icon("chart-bar"), style='font-size:125%'))
      )
    )
  )
})

output$vis_x_source_ui <- renderUI({
  measure <- input$vis_x_opt
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
    out <- selectInput("vis_x_source_opt", "according to...", vars_sources)
  }
  out
})

output$vis_y_source_ui <- renderUI({
  measure <- input$vis_y_opt
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
    out <- selectInput("vis_y_source_opt", "according to...", vars_sources)
  }
  out
})

output$vis_z_source_ui <- renderUI({
  measure <- input$vis_z_opt
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
    out <- selectInput("vis_z_source_opt", "according to...", vars_sources)
  }
  out
})

output$vis_colour_source_ui <- renderUI({
  measure <- input$vis_colour_opt
  if (is.null(measure)) {
    out <- NULL
  } else if (measure == "(None)") {
    out <- NULL
  } else if (measure %in% c("Length", "Generated Stimuli", "Generated Stimuli Condition", "Target Match Word", "Suggested Matches", "Words Uploaded to Fetch Tab")) {
    out <- NULL
  } else if (grepl("^custom.", measure)) {
    out <- NULL
  } else {
    vars_sources <- names(lexops_react_var_measures())[lexops_react_var_measures()==measure] %>%
      sapply(function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE)) %>%
      unname()
    out <- selectInput("vis_colour_source_opt", "according to...", vars_sources)
  }
  out
})

# generate the plot

output$visualiseplotly <- renderPlotly({

  input$vis_generateplot

  NULL

  isolate({
    if (input$vis_x_opt=='(None)' & input$vis_y_opt=='(None)') {
      return(NULL)
    }

    axes_measures <- sapply(c("x", "y", "z", "colour"), function(ax) input[[sprintf("vis_%s_opt", ax)]])
    axes_sources <- sapply(c("x", "y", "z", "colour"), function(ax) input[[sprintf("vis_%s_source_opt", ax)]])

    axes <- sapply(c("x", "y", "z", "colour"), function(ax) {

      measure <- axes_measures[[ax]]
      source <- axes_sources[[ax]]
      if (is.null(source) & measure!="Length") {
        NULL
      } else {
        if (measure=="Length") {
          "Length"
        } else if (grepl("^custom.", measure)) {
          measure
        } else {
          possible_vars <- names(lexops_react_var_measures()[lexops_react_var_measures()==measure])
          possible_vars_sources <- sapply(possible_vars, function(v) LexOPS::var_to_source(v, first_cite = FALSE, standard_eval = TRUE))
          possible_vars[possible_vars_sources == source]
        }
      }
    }, USE.NAMES = TRUE)

    if (all(sapply(axes, is.null))) return(NULL)

    axes_titles <- axes_measures

    measures <- unique(unname(lexops_react_var_measures()))

    # get the data in a plot-friendly dataframe
    pd <- tibble(
      string = lexops_react()$string,
      x = lexops_react()[[axes[["x"]]]],
      y = lexops_react()[[axes[["y"]]]]
    )

    # Specify z axis values
    if (input$vis_z_opt!='(None)') pd$z <- lexops_react()[[axes[["z"]]]]

    # Specify colour axis values
    if (input$vis_colour_opt!='(None)') {
      if (is.null(axes[["colour"]])) {
        if (input$vis_colour_opt=="Generated Stimuli") {
          pd$colour <- ifelse(lexops_react()$string %in% LexOPS::long_format(generated_stim())$string, "Generated Stimuli", "Other Strings")
        } else if (input$vis_colour_opt=="Generated Stimuli Condition") {
          pd$colour <- dplyr::left_join(lexops_react(), LexOPS::long_format(generated_stim()), by = "string") %>%
            dplyr::pull(condition)
        } else if (input$vis_colour_opt=="Target Match Word") {
          pd$colour <- ifelse(lexops_react()$string == input$match_string, "Target Match Word", "Other Strings")
        } else if (input$vis_colour_opt=="Suggested Matches") {
          pd$colour <- ifelse(lexops_react()$string %in% matched_stim()$string, "Suggested Matches", "Other Strings")
        } else if (input$vis_colour_opt=="Words Uploaded to Fetch Tab") {
          pd$colour <- ifelse(lexops_react()$string %in% fetch_df_res()$string, "Uploaded Words", "Other Words")
        }
      } else {
        pd$colour <- lexops_react()[[axes[["colour"]]]]
      }
    }

    pd <- drop_na(pd)

    if (input$vis_colour_opt=='(None)'){
      if(input$vis_z_opt=='(None)'){
        # x * y
        pl <- plot_ly(data = pd, x = ~x, y = ~y, height=screenheight()-175, mode='markers',
                      opacity = input$vis_opacity_sl,
                      marker = list(symbol = 'circle', sizemode = 'diameter', size = input$vis_pointsize_sl),
                      text = ~paste("'", string, "'")) %>%
          add_markers() %>%
          layout(xaxis = list(title = axes_titles[["x"]], color = input$vis_textcolour),
                 yaxis = list(title = axes_titles[["y"]], color = input$vis_textcolour)) %>%
          config(displayModeBar = F)
      } else {
        # x * y * z
        pl <- plot_ly(data = pd, x = ~x, y = ~y, z = ~z, height=screenheight()-175,
                      mode='markers', type="scatter3d",
                      marker = list(symbol = 'circle', sizemode = 'diameter', size = input$vis_pointsize_sl/2),
                      opacity = input$vis_opacity_sl,
                      text = ~paste("'", string, "'")) %>%
          layout(scene = list(xaxis = list(title = axes_titles[["x"]], color = input$vis_textcolour),
                              yaxis = list(title = axes_titles[["y"]], color = input$vis_textcolour),
                              zaxis = list(title = axes_titles[["z"]], color = input$vis_textcolour))) %>%
          config(displayModeBar = F)
      }
    } else {
      # get colour scheme
      colorbarsettings <- NULL  # default of no colorbar title
      if (input$vis_colour_opt %in% measures & input$vis_colour_opt != 'Part of Speech'){
        variable_colours <- viridis_pal(option = "E")(3)  # More numerical colour scheme
      } else {
        # More nominal colour schemes
        if (input$vis_colour_opt=="Part of Speech"){
          variable_colours <- c("red", "blue", "green", "orange", "purple")
        } else {
          if (input$vis_colour_opt == "Generated Stimuli Condition") {
            variable_colours <- c("magenta", "darkorange", "firebrick1", "chartreuse", "cyan", "yellow")[1:length(unique(pd$colour))]
          } else {
            variable_colours <- viridis_pal(option = "D")(3)  # for dichotomous visualisation of yellow on purple (e.g. suggested matches)
          }
        }
      }

      if (length(unique(pd$colour))==2) {
        # get colour category with fewest members and set this as "a" and 1), and the other category as "b" and 2). This produces a fixed colour order.
        smallestcolcat <- pd %>%
          group_by(colour) %>%
          summarise(n = n()) %>%
          arrange(desc(n)) %>%
          slice(1) %>%
          pull(colour)
        pd$colour[pd$colour!=smallestcolcat] <- sprintf("2) %s", pd$colour[pd$colour!=smallestcolcat])
        pd$colour[pd$colour==smallestcolcat] <- sprintf("1) %s", pd$colour[pd$colour==smallestcolcat])
      }

      if(input$vis_z_opt=='(None)'){
        # x * y * colour
        pl <- plot_ly(data = pd, x = ~x, y = ~y, color=~colour, height=screenheight()-175, mode='markers',
                      colors = variable_colours, marker=list(size = input$vis_pointsize_sl, colorbar=colorbarsettings),
                      opacity = input$vis_opacity_sl,
                      text = ~paste("'", string, "'")) %>%
          colorbar(title=axes_titles[["colour"]],
                   titlefont=list(color=input$vis_textcolour),
                   tickcolour=input$vis_textcolour,
                   tickfont=list(color=input$vis_textcolour)) %>%
          layout(xaxis = list(title = axes_titles[["x"]], color = input$vis_textcolour),
                 yaxis = list(title = axes_titles[["y"]], color = input$vis_textcolour),
                 legend = list(font=list(color=input$vis_textcolour))) %>%
          config(displayModeBar = F)
      } else {
        # x * y * z * colour
        pl <- plot_ly(data = pd, x = ~x, y = ~y, z = ~z, color=~colour, height=screenheight()-175,
                      mode='markers', type="scatter3d", colors = variable_colours,
                      marker = list(symbol = 'circle', sizemode = 'diameter', size = input$vis_pointsize_sl/2, colorbar=colorbarsettings),
                      opacity = input$vis_opacity_sl,
                      text = ~paste("'", string, "'")) %>%
          colorbar(title=axes_titles[["colour"]],
                   titlefont=list(color=input$vis_textcolour),
                   tickcolour=input$vis_textcolour,
                   tickfont=list(color=input$vis_textcolour)) %>%
          layout(scene = list(xaxis = list(title = axes_titles[["x"]], color = input$vis_textcolour),
                              yaxis = list(title = axes_titles[["y"]], color = input$vis_textcolour),
                              zaxis = list(title = axes_titles[["z"]], color = input$vis_textcolour)),
                 legend = list(font=list(color=input$vis_textcolour))) %>%
          config(displayModeBar = F)
      }
    }

    # colour the plot background as selected, and return the output
    pl %>% layout(
      paper_bgcolor = input$vis_bgcolour,
      plot_bgcolor = input$vis_bgcolour)
  })

})

# put visualisation inside of a suitably sized box
output$visualisation_ui_box <- renderUI({
  fluidRow(box(width=12, withSpinner(plotlyOutput('visualiseplotly')), height=screenheight()-150))
})
