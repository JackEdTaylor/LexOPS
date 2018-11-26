# Frequency
output$frequency.sl.choice <- renderUI({
  if(input$frequency.log){
    sliderInput('frequency.sl', NULL, value=c(-0.2, 0.2), min=-1, max=1, step=0.1)
  } else {
    sliderInput('frequency.sl', NULL, value=c(-300, 300), min=-10000, max=10000, step=100)
  }
})

# Orthographic Neighborhood
output$on.sl.choice <- renderUI({
  if(input$on.log){
    switch(input$on.opt,
           'old20' = sliderInput('on.sl', NULL, value=c(-0.1, 0.1), min=-0.5, max=0.5, step=0.05),
           'cn' = sliderInput('on.sl', NULL, value=c(-0.2, 0.2), min=-2, max=2, step=0.1))
  } else {
    switch(input$on.opt,
           'old20' = sliderInput('on.sl', NULL, value=c(-1, 1), min=-5, max=5, step=0.1),
           'cn' = sliderInput('on.sl', NULL, value=c(-1, 1), min=-10, max=10, step=1))
  }
})

# Phonological Neighborhood
output$pn.sl.choice <- renderUI({
  if(input$pn.log){
    switch(input$pn.opt,
           'pld20' = sliderInput('pn.sl', NULL, value=c(-0.1, 0.1), min=-0.5, max=0.5, step=0.05),
           'cn' = sliderInput('pn.sl', NULL, value=c(-0.2, 0.2), min=-2, max=2, step=0.1))
  } else {
    switch(input$pn.opt,
           'pld20' = sliderInput('pn.sl', NULL, value=c(-1, 1), min=-5, max=5, step=0.1),
           'cn' = sliderInput('pn.sl', NULL, value=c(-1, 1), min=-10, max=10, step=1))
  }
})

# Age of acquisition
output$aoa.sl.choice <- renderUI({
  switch(input$aoa.opt,
         'kuperman' = sliderInput('aoa.sl', NULL, value=c(-1, 1), min=-5, max=5, step=0.5),
         'gn'= sliderInput('aoa.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1),
         'bb'= sliderInput('aoa.sl', NULL, value=c(-1, 1), min=-12, max=12, step=1))
})

# Concreteness
output$cnc.sl.choice <- renderUI({
  switch(input$cnc.opt,
         'brysbaert' = sliderInput('cnc.sl', NULL, value=c(-0.2, 0.2), min=-1, max=1, step=0.1),
         'gn' = sliderInput('cnc.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1))
})

# Response Time
output$rt.sl.choice <- renderUI({
  if(input$rt.zscore){
    sliderInput('rt.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1, post=' SD')
  } else {
    sliderInput('rt.sl', NULL, value=c(-50, 50), min=-250, max=250, step=10, post=' ms')
  }
})

# Accuracy
output$acc.sl.choice <- renderUI({
  if(input$acc.zscore){
    sliderInput('acc.sl', NULL, value=c(-0.3, 0.3), min=-1.5, max=1.5, step=0.1, post=' SD')
  } else {
    sliderInput('acc.sl', NULL, value=c(-0.05, 0.05), min=-0.25, max=0.25, step=0.01)
  }
})