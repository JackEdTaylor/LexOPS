# Simple function for getting the visualisation colour (in hex) for different coloured boxes
get.box.colour <- function(boxtype='warning'){
  switch(boxtype,
         'primary'='#3c8dbc',
         'warning'='#f39c12',
         'success'='#00a65a',
         'danger'='#dd4b39',
         'info'='#641e68')
}

# Function for generating the density/histogram plots (histogram if integer)
dens.plot <- function(x='gn.VAL', redline=3.2, selected=T, shade=c(3, 3.4), df=dat, boxtype='primary', text.lowscale='More Negative', text.highscale='More Positive', log.transform=F, force.histogram=F, shade_label=NA) {
  
  if (!is.list(shade)) {
    shade <- list(shade)
  }
  
  dfplot <- tibble(x=df[[x]]) %>%
    na.omit()
  
  if (log.transform) {
    dfplot$x <- log(dfplot$x)
  }
  
  if (is.integer(dfplot$x) | force.histogram) {
    
    # get binwidth
    binvals <- sort(unique(dfplot$x))
    bininterval <- min(abs(binvals - c(binvals[2:length(binvals)], 2)))
    
    p <- ggplot(dfplot, aes(x=x)) +
      geom_histogram(binwidth=bininterval, fill=get.box.colour(boxtype), alpha=0.5, colour=NA)
    
    shadepadding <- 0.5  # extra padding to cover bar in histogram
  } else {
    p <- ggplot(dfplot, aes(x=x)) +
      geom_density(fill=get.box.colour(boxtype), alpha=0.5, colour=NA)
    shadepadding <- 0  # no extra padding for density plots
  }
  
  if (selected) {
    shade_i_iter <- 0
    for (shade_i in shade) {
      shade_i_iter <- shade_i_iter + 1
      p <- p +
        annotate('rect', xmin=shade_i[1]-shadepadding, xmax=shade_i[2]+shadepadding, ymin=-Inf, ymax=Inf, alpha=0.4, colour=NA)
      if (all(!is.na(shade_label))) {
        p <- p +
          annotate('label', x=shade_i[1]+((shade_i[2] - shade_i[1])/2), y=Inf, label=shade_label[shade_i_iter], vjust=1.5, fontface="bold", colour=get.box.colour(boxtype), fill="black")
      }
    }
    
    if (!is.na(redline)) {
      p <- p +
        geom_vline(xintercept=redline, colour='red', size=1.25)
    }
    
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
  
  manualcolours <- c('#3c3cbc', '#993cbc', '#5abc3c', '#633cbc', '#bc873c', '#4bbc3c', '#bc3c3c',
                    '#3c8dbc', '#3cbc45', '#b43cbc', '#3cbcae', '#b7bc3c', '#bc3c6c')
  
  # Labels for the top N results, where N is defined in the function
  dfplot$disp_name <- as.character(dfplot$x)
  if (label_top_N < length(unique(dfplot$x))) {
    dfplot$disp_name[(label_top_N+1):nrow(dfplot)] <- ''
  }
  
  # Add additional columns to data, needed for donut plot.
  dfplot$fraction <- dfplot$n / sum(dfplot$n)
  dfplot$ymax <- cumsum(dfplot$fraction)
  dfplot$ymin <- c(0, head(dfplot$ymax, n = -1))
  
  if (PoS=="all") {
    dfplot$alphalevel <- 0.5
  } else {
    dfplot$alphalevel <- ifelse(as.character(dfplot$x) %in% unlist(PoS), 0.5, 0.05)
  }
  
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
    scale_alpha_identity() +
    scale_fill_manual(values=rep(manualcolours,
                                 ceiling((nrow(dfplot)+length(manualcolours))/length(manualcolours))))
}



# word cloud for rhyme
rhyme.plot <- function(str_in="encyclopedia", pron_nr=1, df=lexops, box_opt="cmu", boxtype="primary") {
  xname <- corpus_recode_columns(box_opt, "Rhyme")
  rhymesound <- df[[xname]][dat$string==str_in]
  zipf_cols <- colnames(df)[grepl("Zipf", colnames(df))]
  df$Avg.Zipf <- rowMeans(select(df, one_of(zipf_cols)), dims=1, na.rm=T)
  df %>%
    select(string, UQ(sym(xname)), Avg.Zipf) %>%
    filter(UQ(sym(xname))==rhymesound & between(Avg.Zipf, 3, 7) & string!=str_in) %>%
    sample_n(15) %>%
    mutate(wordcloudsize=1, wordcloudalpha=0.9) %>%
    add_row(string=str_in, wordcloudsize=5, wordcloudalpha=1, .before=1) %>%
    ggplot(aes(label=string, size=wordcloudsize, alpha=wordcloudalpha)) +
    geom_text_wordcloud(colour=get.box.colour(boxtype), rm_outside=T, shape='circle') +
    theme_minimal() +
    scale_size_area(max_size=20) +
    scale_alpha_identity()
}

# Error plot
error.plot <- function(errormessage="Error!", boxtype="success") {
  tibble(qm = c(errormessage, rep("?", 99)),
         wordcloudsize = c(6, rep(1, 99)),
         wordcloudalpha = c(1, rep(0.6, 99))) %>%
    ggplot(aes(label=qm, size=wordcloudsize, alpha=wordcloudalpha)) +
    geom_text_wordcloud(colour=get.box.colour(boxtype), rm_outside=T, shape='circle') +
    theme_minimal() +
    scale_size_area(max_size=15) +
    scale_alpha_identity()
}

# General time-saving box visualisation functions
semantic_vis <- function(opts, selected, slider, prefix, text.lowscale="Less", text.highscale="More", df=lexops, str_in) {
  if (length(opts)==0){
    error.plot("No source\nselected!", "success")
  } else {
    out_copy <- df
    column <- corpus_recode(opts, prefix)
    if (length(column)>1) {
      out_copy[column] <- lapply(out_copy[column], scale)
      out_copy$xdata <- rowMeans(select(out_copy, one_of(column)), dims=1, na.rm=T)
    } else {
      out_copy$xdata <- out_copy[[column]]
    }
    str_in_x <- out_copy$xdata[out_copy$string==str_in]
    dens.plot(x="xdata", selected=selected,
              redline=str_in_x,
              shade=c(str_in_x + slider[1], str_in_x + slider[2]),
              df = out_copy,
              boxtype = 'success',
              text.lowscale=text.lowscale, text.highscale=text.highscale)
  }
}

