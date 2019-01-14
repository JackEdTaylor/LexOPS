# Counting N boxes
observeEvent(input$gen_splitby_add, {
  gen_splitby_boxes_N(gen_splitby_boxes_N() + 1)
})
observeEvent(input$gen_splitby_minus, {
  if (gen_splitby_boxes_N()>0) {
    gen_splitby_boxes_N(gen_splitby_boxes_N() - 1)
  }
})

# Display N boxes
observeEvent(gen_splitby_boxes_N(), {
  lapply (1:25, function(i) {
    boxid <- sprintf('gen_splitby_%i', i)
    if (i <= gen_splitby_boxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})

# Build boxes' UIs
lapply(1:25, function(i) {
  boxid <- sprintf('gen_splitby_%i', i)
  var_lttr <- LETTERS[i]
  output[[sprintf('%s_ui', boxid)]] <- renderUI({ splitby_UI(input[[sprintf("%s_vtype", boxid)]],
                                                             boxid) })
  output[[sprintf('%s_ui_sliders', boxid)]] <- renderUI({ splitby_UI_sliders(input[[sprintf("%s_vtype", boxid)]],
                                                                             boxid,
                                                                             input[[sprintf("%s_Nlevels", boxid)]],
                                                                             input[[sprintf("%s.opt", boxid)]],
                                                                             input[[sprintf("%s.log", boxid)]],
                                                                             var_lttr,
                                                                             lexops) })
  box_sliders <- reactive({
    sl <- list()
    for (i in 1:input[[sprintf("%s_Nlevels", boxid)]]) {
      sl[[i]] <- input[[sprintf("%s_sl%i", boxid, i)]]
    }
    sl
  })
  
  output[[sprintf('%s_ui_vis', boxid)]] <- renderPlot({ splitby_UI_vis(input[[sprintf("%s_vtype", boxid)]],
                                                                       boxid,
                                                                       input[[sprintf("%s_Nlevels", boxid)]],
                                                                       input[[sprintf("%s.opt", boxid)]],
                                                                       input[[sprintf("%s.log", boxid)]],
                                                                       input[[sprintf("%s.source", boxid)]],
                                                                       lexops,
                                                                       box_sliders(),
                                                                       var_lttr) })
})

# Put the UIs built above into their boxes
lapply(1:25, function(i) {
  var_lttr <- LETTERS[i]
  boxid <- sprintf('gen_splitby_%i', i)
  output[[boxid]] <- renderUI({
    box(title=var_lttr, width=12, status='primary', solidHeader=T,
        selectInput(sprintf('%s_vtype', boxid), NULL, c('(None)', vis.cats[vis.cats!="Rhyme"])),
        uiOutput(sprintf('%s_ui', boxid)),
        uiOutput(sprintf('%s_ui_sliders', boxid)),
        plotOutput(sprintf('%s_ui_vis', boxid), height='170px'),
        id = boxid
    )
  })
})


