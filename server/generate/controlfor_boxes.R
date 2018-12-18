observeEvent(input$gen_controlfor_add, {
  gen_controlfor_boxes_N(gen_controlfor_boxes_N() + 1)
})

observeEvent(input$gen_controlfor_minus, {
  if (gen_controlfor_boxes_N()>0) {
    gen_controlfor_boxes_N(gen_controlfor_boxes_N() - 1)
  }
})

observeEvent(gen_controlfor_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf('gen_controlfor_%i', i)
    if (i <= gen_controlfor_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})


lapply(1:25, function(i) {
  boxid <- sprintf('gen_controlfor_%i', i)
  output[[sprintf('%s', boxid)]] <- renderUI({
    box(title=i, width=12, status='warning',
        selectInput(sprintf('%s_vtype', boxid), NULL, c('(None)', vis.cats)),
        id = boxid
    )
  })
})