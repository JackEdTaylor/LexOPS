# Download button
output$generated.csv <- downloadHandler(
  filename = 'generated.csv',
  content = function(file) {
    write.csv(genresults(), file, row.names = FALSE)
  }
)

# put generated stimuli in datatable
output$gen_results_dt <- DT::renderDataTable({
  DT::datatable(genresults(), options=list(pageLength=25, scrollX=T))
})

# Options for controlling for variables
output$gen_controlnull_choice <- renderUI({
  if (gen_splitby_boxes_N() >= 1) {
    cells <- genlevels() %>%
      select(Level) %>%
      unlist(use.names=F)
    cells <- c("All conditions (inclusive tolerance)"="inclusive", cells)
  } else {
    cells <- c("(None)")
  }
  selectInput('gen_controlnull', "Control for Variables relative to", cells)
})

output$gen_dist_opts_choice <- renderUI({
  if (input$gen_check.dist) {
    ui <- list()
    ui[[1]] <- column(12, radioButtons('gen_dist.opt', 'Distance Measure', c('Euclidean Distance'='ed', 'City Block Distance'='cb'), selected='ed'))
    ui[[2]] <- column(12, sliderInput('gen_dist_tol', 'Distance Tolerance', min=0, max=50, value=1, step=0.1, width='100%'))
    ui
  } else {
    NULL
  }
})