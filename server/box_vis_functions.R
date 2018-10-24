# Simple function for getting the visualisation colour (in hex) for different coloured boxes
get.box.colour <- function(boxtype='warning'){
  switch(boxtype,
         'primary'='#3c8dbc',
         'warning'='#f39c12',
         'success'='#00a65a',
         'danger'='#dd4b39',
         'info' = '#641e68')
}

# Function for generating the density/histogram plots (histogram if integer)
dens.plot <- function(x='gn.VAL', redline=3.2, selected=T, shade=c(3, 3.4), df=dat, boxtype='primary', text.lowscale='More Negative', text.highscale='More Positive', log.transform=F, force.histogram=F) {
  
  dfplot <- tibble(x=df[[x]]) %>%
    na.omit()
  
  if (log.transform) {
    dfplot$x <- log(dfplot$x)
  }
  
  if (class(dfplot$x)=='integer' | force.histogram) {
    
    # get binwidth
    binvals <- sort(unique(dfplot$x))
    bininterval <- min(abs(binvals - c(binvals[2:length(binvals)], 2)))
    
    p <- ggplot(dfplot, aes(x=x)) +
      geom_histogram(binwidth=bininterval, fill=get.box.colour(boxtype), alpha=0.5, colour=NA)
    if (force.histogram) {
      shadepadding <- 0
      } else {
      shadepadding <- 0.5
      }  # extra padding to cover bar in histogram
  } else {
    p <- ggplot(dfplot, aes(x=x)) +
      geom_density(fill=get.box.colour(boxtype), alpha=0.5, colour=NA)
    shadepadding <- 0  # no extra padding for density plots
  }
  
  if (selected) {
    p <- p +
      annotate('rect', xmin=shade[1]-shadepadding, xmax=shade[2]+shadepadding, ymin=0, ymax=Inf, alpha=0.4, colour=NA) +
      geom_vline(xintercept=redline, colour='red', size=1.25)
  }
  
  p <- p +
    labs(y=NULL, x=NULL) +
    scale_x_continuous(breaks = scales::pretty_breaks()) +
    theme_minimal()
  
  # get xbreaks displayed in plot
  build <- ggplot_build(p)
  xbreaks <- build$layout$panel_params[[1]]$x.major_source
  # replace spaces in inputs with returns
  text.lowscale <- gsub(' ', '\n', text.lowscale)
  text.highscale <- gsub(' ', '\n', text.highscale)
  # edit to add required text
  middle.xbreaklabels <- as.character(xbreaks)[2:(length(xbreaks)-1)]
  xlabels <- c(sprintf('%s\n\n%s', xbreaks[1], text.lowscale),
               middle.xbreaklabels,
               sprintf('%s\n\n%s', xbreaks[(length(xbreaks))], text.highscale))
  
  # add new xscale to plot
  p <- p + scale_x_continuous(breaks = xbreaks, labels = xlabels)
  
  p
}

# pie chart for position of speech
pos.plot <- function(xname='subtlex_uk.DomPoS', selected=T, PoS='noun', df=dat, label_top_N=5){
  
  dfplot <- tibble(x=df[[xname]]) %>%
    filter(x!=' ') %>%
    na.omit() %>%
    group_by(x) %>%
    summarise(n=n()) %>%
    arrange(desc(n))
  
  # Labels for the top N results, where N is defined in the function
  dfplot$disp_name <- as.character(dfplot$x)
  if (label_top_N < length(unique(dfplot$x))) {
    dfplot$disp_name[(label_top_N+1):nrow(dfplot)] <- ''
  }
  
  # Add additional columns to data, needed for donut plot.
  dfplot$fraction = dfplot$n / sum(dfplot$n)
  dfplot$ymax = cumsum(dfplot$fraction)
  dfplot$ymin = c(0, head(dfplot$ymax, n = -1))
  dfplot$alphalevel <- ifelse(as.character(dfplot$x)==PoS, 1, 0)
  
  # Only do alpha effect if checkbox selected
  p <- if(selected) {
    ggplot(data = dfplot, aes(fill = x, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5, alpha=alphalevel)) +
      geom_rect(colour = "grey30", show.legend = F)
  } else {
    ggplot(data = dfplot, aes(fill = x, ymax = ymax, ymin = ymin, xmax = 4, xmin = 2.5)) +
      geom_rect(colour = "grey30", show.legend = F, alpha=0.05)
  }
  
  p +
    coord_polar(theta = "y") +
    geom_text(aes(x = 3.5, y = ((ymin+ymax)/2), label=disp_name), alpha=1) +
    xlim(c(0, 4)) +
    labs(x=NULL, y=NULL) +
    theme_void() +
    scale_alpha(guide = 'none', range=c(0.05, 0.5)) +
    scale_fill_manual(values=c('#3c3cbc', '#993cbc', '#5abc3c', '#633cbc', '#bc873c', '#4bbc3c', '#bc3c3c',
                               '#3c8dbc', '#3cbc45', '#b43cbc', '#3cbcae', '#b7bc3c', '#bc3c6c'))
}