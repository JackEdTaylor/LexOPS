observeEvent(input$gen_filterby_add, {
  gen_filterby_boxes_N(gen_filterby_boxes_N() + 1)
})

observeEvent(input$gen_filterby_minus, {
  if (gen_filterby_boxes_N()>0) {
    gen_filterby_boxes_N(gen_filterby_boxes_N() - 1)
  }
})

observeEvent(gen_filterby_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf('gen_filterby_%i', i)
    if (i <= gen_filterby_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})


lapply(1:25, function(i) {
  boxid <- sprintf('gen_filterby_%i', i)
  output[[sprintf('%s', boxid)]] <- renderUI({
    box(title=i, width=12, status='info',
        selectInput(sprintf('%s_vtype', boxid), NULL, c('(None)', vis.cats)),
        id = boxid
    )
  })
})
