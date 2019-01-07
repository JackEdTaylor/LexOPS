# Download button
output$matched.csv <- downloadHandler(
  filename = 'generated.csv',
  content = function(file) {
    write.csv(genresults(), file, row.names = FALSE)
  }
)

# put generated stimuli in datatable
output$gen_results_dt <- DT::renderDataTable({
  DT::datatable(genresults(), options=list(pageLength=25, scrollX=T))
})
