observeEvent(input$preference_use_a_random_seed, {
  if (input$preference_use_a_random_seed) {
    shinyjs::enable("preference_random_seed")
  } else {
    shinyjs::disable("preference_random_seed")
  }
})
