# Counting N boxes
observeEvent(input$match_add, {
  matchboxes_N(matchboxes_N() + 1)
})
observeEvent(input$match_minus, {
  if (matchboxes_N()>0) {
    matchboxes_N(matchboxes_N() - 1)
  }
})

# Display N boxes
observeEvent(matchboxes_N(), {
  lapply (1:50, function(i) {
    boxid <- sprintf('matchbox_%i', i)
    if (i <= matchboxes_N()) {
      shinyjs::show(id = boxid)
    } else {
      shinyjs::hide(id = boxid)
    }
  })
})

# Build boxes' UIs
lapply(1:50, function(i) {
  boxid <- sprintf('matchbox_%i', i)
  output[[sprintf('%s_ui', boxid)]] <- renderUI({ match_UI(input[[sprintf("%s_vtype", boxid)]],
                                                           boxid,
                                                           lexopsReact(),
                                                           input$matchstring) })
  output[[sprintf('%s_ui_manual', boxid)]] <- renderUI({ match_UI_manual(input[[sprintf("%s_vtype", boxid)]],
                                                                         boxid,
                                                                         input[[sprintf("%s.opt", boxid)]],
                                                                         lexopsReact(),
                                                                         input[[sprintf('%s.auto_or_manual', boxid)]],
                                                                         input$matchstring) })
  output[[sprintf('%s_ui_sliders', boxid)]] <- renderUI({ match_UI_sliders(input[[sprintf("%s_vtype", boxid)]],
                                                                           boxid,
                                                                           input[[sprintf("%s.opt", boxid)]],
                                                                           input[[sprintf("%s.log", boxid)]],
                                                                           lexopsReact())})
  box_sliders <- reactive({ input[[sprintf("%s_sl", boxid)]] })
  output[[sprintf('%s_ui_vis', boxid)]] <- renderPlot({ match_UI_vis(input[[sprintf("%s_vtype", boxid)]],
                                                                     boxid,
                                                                     input[[sprintf("%s.opt", boxid)]],
                                                                     input[[sprintf("%s.log", boxid)]],
                                                                     input[[sprintf("%s.source", boxid)]],
                                                                     input[[sprintf('%s.auto_or_manual', boxid)]],
                                                                     input[[sprintf('%s.manual', boxid)]],
                                                                     lexopsReact(),
                                                                     box_sliders(),
                                                                     input$matchstring) })
})

# Put the UIs built above into their boxes
lapply(1:50, function(i) {
  boxid <- sprintf('matchbox_%i', i)
  output[[boxid]] <- renderUI({
    box(title=i, width=6, status='primary', solidHeader=T,
        selectInput(sprintf('%s_vtype', boxid), NULL, c('(None)', vis.cats)),
        uiOutput(sprintf('%s_ui', boxid)),
        uiOutput(sprintf('%s_ui_manual', boxid)),
        uiOutput(sprintf('%s_ui_sliders', boxid)),
        plotOutput(sprintf('%s_ui_vis', boxid), height='170px'),
        id = boxid
    )
  })
})


