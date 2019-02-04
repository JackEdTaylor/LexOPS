# Counting N boxes
observeEvent(input$gen_controlfor_add, {
  gen_controlfor_boxes_N(gen_controlfor_boxes_N() + 1)
})
observeEvent(input$gen_controlfor_minus, {
  if (gen_controlfor_boxes_N()>0) {
    gen_controlfor_boxes_N(gen_controlfor_boxes_N() - 1)
  }
})

# Display N boxes
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

# Build boxes' UIs
lapply(1:25, function(i) {
  boxid <- sprintf('gen_controlfor_%i', i)
  output[[sprintf('%s_ui', boxid)]] <- renderUI({ controlfor_UI(input[[sprintf("%s_vtype", boxid)]],
                                                                boxid) })
  output[[sprintf('%s_ui_sliders', boxid)]] <- renderUI({ controlfor_UI_sliders(input[[sprintf("%s_vtype", boxid)]],
                                                                                boxid,
                                                                                input[[sprintf("%s.opt", boxid)]],
                                                                                input[[sprintf("%s.log", boxid)]],
                                                                                lexopsReact()) })
  box_sliders <- reactive({ input[[sprintf("%s_sl", boxid)]] })
  output[[sprintf('%s_ui_vis', boxid)]] <- renderPlot({ controlfor_UI_vis(input[[sprintf("%s_vtype", boxid)]],
                                                                          boxid,
                                                                          input[[sprintf("%s.opt", boxid)]],
                                                                          input[[sprintf("%s.log", boxid)]],
                                                                          input[[sprintf("%s.source", boxid)]],
                                                                          lexopsReact(),
                                                                          box_sliders()) })
})

# Put the UIs built above into their boxes
lapply(1:25, function(i) {
  boxid <- sprintf('gen_controlfor_%i', i)
  output[[boxid]] <- renderUI({
    box(title=i, width=12, status='warning', solidHeader=T,
        selectInput(sprintf('%s_vtype', boxid), NULL, c('(None)', vis.cats)),
        uiOutput(sprintf('%s_ui', boxid)),
        uiOutput(sprintf('%s_ui_sliders', boxid)),
        plotOutput(sprintf('%s_ui_vis', boxid), height='170px'),
        id = boxid
    )
  })
})
