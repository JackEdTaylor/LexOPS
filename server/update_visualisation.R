# Get data for plotting
plotdata <- reactive({
  vd <- lexopsReact()
  
  if (length(input$vis.xsource)>1) {
    if (!(input$vis.xaxis.opts %in% vis.cats.non_Zscore)) {
      vd[input$vis.xsource] <- lapply(vd[input$vis.xsource], scale)
    }
    vd$xcol <- rowMeans(select(vd, one_of(input$vis.xsource)), dims=1, na.rm=T)
  } else {
    vd$xcol <- vd[[input$vis.xsource]]
  }
  
  if (length(input$vis.ysource)>1) {
    if (!(input$vis.yaxis.opts %in% vis.cats.non_Zscore)) {
      vd[input$vis.ysource] <- lapply(vd[input$vis.ysource], scale)
    }
    vd$ycol <- rowMeans(select(vd, one_of(input$vis.ysource)), dims=1, na.rm=T)
  } else {
    vd$ycol <- vd[[input$vis.ysource]]
  }
  
  if (input$vis.zaxis.opts!='(None)') {
    if (length(input$vis.zsource)>1) {
      if (!(input$vis.zaxis.opts %in% vis.cats.non_Zscore)) {
        vd[input$vis.zsource] <- lapply(vd[input$vis.zsource], scale)
      }
      vd$zcol <- rowMeans(select(vd, one_of(input$vis.zsource)), dims=1, na.rm=T)
    } else {
      vd$zcol <- vd[[input$vis.zsource]]
    }
  }
  
  if (input$vis.colour.opts!='(None)' & input$vis.colour.opts %in% vis.cats) {
    if (length(input$vis.coloursource)>1) {
      if (!(input$vis.colour.opts %in% vis.cats.non_Zscore)) {
        vd[input$vis.coloursource] <- lapply(vd[input$vis.coloursource], scale)
      }
      vd$colourcol <- rowMeans(select(vd, one_of(input$vis.coloursource)), dims=1, na.rm=T)
    } else {
      vd$colourcol <- vd[[input$vis.coloursource]]
    }
  }
  
  t <- tibble(string = vd$string,
              x = vd$xcol,
              y = vd$ycol)
  if (input$vis.zaxis.opts!='(None)'){
    t$z <- vd$zcol
  }
  if (input$vis.colour.opts=='Target Match Word'){
    t$colour <- ifelse(t$string==input$string, "Target String", "Other Strings")
  } else if (input$vis.colour.opts=='Suggested Matches'){
    t$colour <- ifelse(t$string %in% matchresults()$string, "Suggested Strings", "Other Strings")
  } else if(input$vis.colour.opts=='Words Uploaded to Fetch Tab'){
    if (is.null(fetch_df_raw()) | is.null(fetch_targwordstringcolname())) {
      t$colour <- NA
    } else {
      t$colour <- ifelse(t$string %in% fetch_df_raw()[[fetch_targwordstringcolname()]],
                         "Uploaded Items", "Other Strings")
    }
  } else if(input$vis.colour.opts %in% vis.cats){
    t$colour <- vd$colourcol
  }
  t %>%
    na.omit()  # remove values with missing data for requested visualisation variables
})

# get screen height using java to maximise plot within window
screenheight <- reactive({
  input$dimension[2]
})

# visualisation in plotly
output$visualiseplotly <- renderPlotly({
  
  xtitle <- sprintf('%s (%s)', input$vis.xaxis.opts, input$vis.xsource)
  ytitle <- sprintf('%s (%s)', input$vis.yaxis.opts, input$vis.ysource)
  ztitle <- if (input$vis.zaxis.opts %in% vis.cats) {
    sprintf('%s (%s)', input$vis.zaxis.opts, input$vis.zsource)
  } else if (input$vis.zaxis.opts != '(None)') {
    input$vis.zaxis.opts  # z axis title if error occurs
  }
  
  pd <- plotdata()
  if (input$vis.colour.opts=='(None)'){
    if(input$vis.zaxis.opts=='(None)'){
      # x * y
      plot_ly(data = pd, x = ~x, y = ~y, height=screenheight()-175, mode='markers',
              opacity = input$vis.opacity.sl,
              text = ~paste("'", string, "'")) %>%
        add_markers() %>%
        layout(xaxis = list(title = xtitle),
               yaxis = list(title = ytitle)) %>%
        config(displayModeBar = F)
    } else {
      # x * y * z
      plot_ly(data = pd, x = ~x, y = ~y, z = ~z, height=screenheight()-175,
              mode='markers', type="scatter3d",
              marker = list(symbol = 'circle', sizemode = 'diameter', size = 2.5),
              opacity = input$vis.opacity.sl,
              text = ~paste("'", string, "'")) %>%
        layout(scene = list(xaxis = list(title = xtitle),
                            yaxis = list(title = ytitle),
                            zaxis = list(title = ztitle))) %>%
        config(displayModeBar = F)
    }
  } else {
    # get colour scheme
    colorbarsettings <- NULL  # default of no colorbar title
    if (input$vis.colour.opts %in% vis.cats & input$vis.colour.opts != 'Part of Speech'){
      variable_colours <- viridis_pal(option = "E")(3)  # More numerical colour scheme
      colorbarsettings <- list(title='Test')  # title for colorbar
    } else {
      # More nominal colour schemes
      if (input$vis.colour.opts=="Part of Speech"){
        variable_colours <- c("red", "blue", "green", "orange", "purple")
      } else {
        variable_colours <- viridis_pal(option = "D")(3)  # for suggested matches and target word
      }
    }
    
    if(input$vis.zaxis.opts=='(None)'){
      # x * y * colour
      plot_ly(data = pd, x = ~x, y = ~y, color=~colour, height=screenheight()-175, mode='markers',
              colors = variable_colours, marker=list(size = input$vis.pointsize.sl, colorbar=colorbarsettings),
              opacity = input$vis.opacity.sl,
              text = ~paste("'", string, "'")) %>%
        layout(xaxis = list(title = xtitle),
               yaxis = list(title = ytitle)) %>%
        config(displayModeBar = F)
    } else {
      # x * y * z * colour
      plot_ly(data = pd, x = ~x, y = ~y, z = ~z, color=~colour, height=screenheight()-175,
              mode='markers', type="scatter3d", colors = variable_colours,
              marker = list(symbol = 'circle', sizemode = 'diameter', size = input$vis.pointsize.sl/2, colorbar=colorbarsettings),
              opacity = input$vis.opacity.sl,
              text = ~paste("'", string, "'")) %>%
        layout(scene = list(xaxis = list(title = xtitle),
                            yaxis = list(title = ytitle),
                            zaxis = list(title = ztitle))) %>%
        config(displayModeBar = F)
    }
  }
})

# source drop-down menu for visualisation
output$vis.xsource.choice <- renderUI({
  if(input$vis.xaxis.opts %in% vis.cats) {
    sources <- vis.opt.2.source(input$vis.xaxis.opts, visualise.opts())
    vd <- lexopsReact()
    if (all(sapply(vd[sources], is.numeric))) {
      checkboxGroupInput('vis.xsource', 'X Axis Source', sources, sources[1], inline=T)
    } else {
      radioButtons('vis.xsource', 'X Axis Source', sources, sources[1], inline=T)
    }
  } else {NULL}
})
output$vis.ysource.choice <- renderUI({
  if(input$vis.yaxis.opts %in% vis.cats) {
    sources <- vis.opt.2.source(input$vis.yaxis.opts, visualise.opts())
    vd <- lexopsReact()
    if (all(sapply(vd[sources], is.numeric))) {
      checkboxGroupInput('vis.ysource', 'Y Axis Source', sources, sources[1], inline=T)
    } else {
      radioButtons('vis.ysource', 'Y Axis Source', sources, sources[1], inline=T)
    }
  } else {NULL}
})
output$vis.zsource.choice <- renderUI({
  if(input$vis.zaxis.opts %in% vis.cats) {
    sources <- vis.opt.2.source(input$vis.zaxis.opts, visualise.opts())
    vd <- lexopsReact()
    if (all(sapply(vd[sources], is.numeric))) {
      checkboxGroupInput('vis.zsource', 'Z Axis Source', sources, sources[1], inline=T)
    } else {
      radioButtons('vis.zsource', 'Z Axis Source', sources, sources[1], inline=T)
    }
  } else {NULL}
})
output$vis.coloursource.choice <- renderUI({
  if(input$vis.colour.opts %in% vis.cats) {
    sources <- vis.opt.2.source(input$vis.colour.opts, visualise.opts())
    vd <- lexopsReact()
    if (all(sapply(vd[sources], is.numeric))) {
      checkboxGroupInput('vis.coloursource', 'Colour Source', sources, sources[1], inline=T)
    } else {
      radioButtons('vis.coloursource', 'Colour Source', sources, sources[1], inline=T)
    }
  } else {NULL}
})

# put visualisation inside of a suitably sized box
output$visualisation.ui_box <- renderUI({
  fluidRow(
    box(width=12, withSpinner(plotlyOutput('visualiseplotly')), height=screenheight()-150)
  )
})

