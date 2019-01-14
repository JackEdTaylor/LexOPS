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
  } else {
    cells <- c("(None)")
  }
  selectInput('gen_controlnull', "Control for Variables relative to", c("All conditions (inclusive tolerance)"="inclusive", cells))
})
