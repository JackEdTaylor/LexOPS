not_generated_message <- "The generate() function was not called. Specify at least one split and at least one control to use the generate algorithm."

output$gen_review_iteration_plot <- renderPlot({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated)) {
    LexOPS::plot_iterations(generated_stim()) +
      ggplot2::theme(
        axis.title = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12)
      )
  } else {
    NULL
  }
})

output$gen_review_success_rate <- renderText({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if(is.null(LexOPS_attrs$generated)) {
    not_generated_message
  } else {
    if (LexOPS_attrs$generated) {
      sprintf("End success rate (total proportion of iterations that successfully produced an item for each condition): %f.", LexOPS_attrs$success_rate)
    } else {
      not_generated_message
    }
  }
})

output$gen_review_iteration_plot_ui <- renderUI({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated)) {
    plotOutput("gen_review_iteration_plot")
  } else {
    NULL
  }
})

output$gen_plot_filters <- renderPlot({
  filt_vars <- sapply(1:gen_filterby_boxes_N(), function(i) filter_opts_react()[[i]]$var)
  LexOPS::plot_design(generated_stim(), filt_vars, point_size = 1.75, line_width = 1.25) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )
})

output$gen_plot_filters_ui <- renderUI({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated) & gen_filterby_boxes_N() > 0) {
    filt_vars <- sapply(1:gen_filterby_boxes_N(), function(i) filter_opts_react()[[i]]$var)
    filt_vars <- filt_vars[sapply(lexops_react()[filt_vars], is.numeric)]
    if (length(filt_vars)>0) {
      plotOutput("gen_plot_filters", height = "500px")
    } else {
      "No numeric filters used."
    }
  } else {
    not_generated_message
  }
})

output$gen_plot_splits <- renderPlot({
  LexOPS::plot_design(generated_stim(), "splits", point_size = 1.75, line_width = 1.25) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )
})

output$gen_plot_splits_ui <- renderUI({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated)) {
    split_vars <- sapply(1:gen_splitby_boxes_N(), function(i) split_opts_react()[[i]]$var)
    split_vars <- split_vars[split_vars != "Random"]
    split_vars <- split_vars[sapply(lexops_react()[split_vars], is.numeric)]
    if (length(split_vars)>0) {
      plotOutput("gen_plot_splits", height = "500px")
    } else {
      "No numeric splits used."
    }
  } else {
    not_generated_message
  }
})

output$gen_plot_controls <- renderPlot({
  LexOPS::plot_design(generated_stim(), "controls", point_size = 1.75, line_width = 1.25) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16),
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    )
})

output$gen_plot_controls_ui <- renderUI({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated)) {
    control_vars <- sapply(1:gen_controlfor_boxes_N(), function(i) control_opts_react()[[i]]$var)
    control_vars <- control_vars[sapply(lexops_react()[control_vars], is.numeric)]
    if (length(control_vars)>0) {
      plotOutput("gen_plot_controls", height = "500px")
    } else {
      "No numeric controls used."
    }
  } else {
    not_generated_message
  }
})

output$gen_review_null_distribution_table <- renderTable({
  generated_stim() %>%
    dplyr::group_by(match_null) %>%
    dplyr::count() %>%
    dplyr::rename("Match Null" = match_null, "Count" = n)
})

output$gen_review_null_distribution_plot <- renderPlot({
  generated_stim() %>%
    ggplot2::ggplot(ggplot2::aes(x = match_null)) +
    ggplot2::geom_bar(alpha = 0.75, fill = "grey") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      legend.text = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(
      x = "Match Null Condition",
      y = "Count"
    )
})

output$gen_review_null_distribution_ui <- renderUI({
  LexOPS_attrs <- attr(generated_stim(), "LexOPS_attrs")
  if (!is.null(LexOPS_attrs$generated)) {
    fluidRow(
      column(3, tableOutput("gen_review_null_distribution_table"), align = "center"),
      column(9, plotOutput("gen_review_null_distribution_plot"))
    )
  } else {
    not_generated_message
  }
})
