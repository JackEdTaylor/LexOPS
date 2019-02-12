# N Stim
output$gen_N_stim_choice <- renderUI({
  if (input$gen_limit_N=="N") {
    column(12, numericInput('gen_N_stim', 'Number of Words per Condition', 50, min=1, max=NA, step=1))
  } else {
    NULL
  }
})

# Download button
output$generated.csv <- downloadHandler(
  filename = 'generated.csv',
  content = function(file) {
    write.csv(genresults(), file, row.names=F)
  }
)

# put generated stimuli in datatable
output$gen_results_dt <- DT::renderDataTable({
  DT::datatable(genresults(), options=list(pageLength=25, scrollX=T), rownames=F)
})

# Options for controlling for variables
output$gen_controlnull_choice <- renderUI({
  if (gen_splitby_boxes_N() >= 1) {
    cells <- genlevels() %>%
      select(Level) %>%
      unlist(use.names=F)
    cells <- c(cells, "Random (Different Null Condition Per Item)"="random")
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

# Data format
output$gen_dataformat_choice <- renderUI({
  if (gen_controlfor_boxes_N()>0){
    radioButtons('gen_dataformat', 'Data Format', c('Wide'='wide', 'Long'='long'), 'wide')
  } else {
    NULL
  }
})