# Download button
output$matched.csv <- downloadHandler(
  filename = 'matched.csv',
  content = function(file) {
    write.csv(matchresults(), file, row.names = FALSE)
  }
)

# sorting options
output$match_results_sort_1_choice <- renderUI ({
  selectInput('match_results_sort_1', "Sort 1", c("(None)", names(matchresults_unsorted())), width='100%')
})
output$match_results_sort_2_choice <- renderUI ({
  if (input$match_results_sort_1 != "(None)" & ncol(matchresults_unsorted())>1) {
    selectInput('match_results_sort_2', "Sort 2", c("(None)", names(matchresults_unsorted())), width='100%')
  } else {
    NULL
  }
})
output$match_results_sort_3_choice <- renderUI ({
  if ("match_results_sort_2" %in% names(input)) {
    if (input$match_results_sort_2 != "(None)" & ncol(matchresults_unsorted())>2) {
      selectInput('match_results_sort_3', "Sort 3", c("(None)", names(matchresults_unsorted())), width='100%')
    } else {
      NULL
    }
  }
})
output$match_results_sort_4_choice <- renderUI ({
  if ("match_results_sort_3" %in% names(input)) {
    if (input$match_results_sort_3 != "(None)" & ncol(matchresults_unsorted())>2) {
      selectInput('match_results_sort_4', "Sort 4", c("(None)", names(matchresults_unsorted())), width='100%')
    } else {
      NULL
    }
  }
})
output$match_results_sort_5_choice <- renderUI ({
  if ("match_results_sort_4" %in% names(input)) {
    if (input$match_results_sort_4 != "(None)" & ncol(matchresults_unsorted())>2) {
      selectInput('match_results_sort_5', "Sort 5", c("(None)", names(matchresults_unsorted())), width='100%')
    } else {
      NULL
    }
  }
})
# ascending/descending
output$match_results_sort_1_order_choice <- renderUI ({
  if (input$match_results_sort_1 != "(None)") {
    selectInput('match_results_sort_1_order', "Order 1", c("Ascending", "Descending"), width='100%')
  } else {
    NULL
  }
})
output$match_results_sort_2_order_choice <- renderUI ({
  if ("match_results_sort_2" %in% names(input)) {
    if (input$match_results_sort_2 != "(None)") {
      selectInput('match_results_sort_2_order', "Order 2", c("Ascending", "Descending"), width='100%')
    } else {
      NULL
    }
  }
})
output$match_results_sort_3_order_choice <- renderUI ({
  if ("match_results_sort_4" %in% names(input)) {
    if (input$match_results_sort_3 != "(None)") {
      selectInput('match_results_sort_3_order', "Order 3", c("Ascending", "Descending"), width='100%')
    } else {
      NULL
    }
  }
})
output$match_results_sort_4_order_choice <- renderUI ({
  if ("match_results_sort_4" %in% names(input)) {
    if (input$match_results_sort_4 != "(None)") {
      selectInput('match_results_sort_4_order', "Order 4", c("Ascending", "Descending"), width='100%')
    } else {
      NULL
    }
  }
})
output$match_results_sort_5_order_choice <- renderUI ({
  if ("match_results_sort_4" %in% names(input)) {
    if (input$match_results_sort_5 != "(None)") {
      selectInput('match_results_sort_5_order', "Order 5", c("Ascending", "Descending"), width='100%')
    } else {
      NULL
    }
  }
})


# put matches in datatable
output$match_results_dt <- DT::renderDataTable({
  DT::datatable(matchresults(), options=list(pageLength=25))
})

# For displaying number of results under word-entry textbox in sidebar
output$nrow.results <- renderText({sprintf('%i results', nrow(matchresults())-1)})
