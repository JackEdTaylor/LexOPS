splitby_UI <- function(vtype = "Word Frequency", boxid) {
  
  if (!is.null(vtype)) {
    
    ui <- list()
    
    if (vtype == "Word Frequency") {
      ui[[1]] <- checkboxInput(sprintf('%s.log', boxid), 'Log Transform (Zipf)', 1)
      ui[[2]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Corpora',
                           c('BNC (written)'='bnc_w', 'BNC (spoken)'='bnc_s', 'SUBTLEX-UK'='suk', 'SUBTLEX-US'='sus'),
                           c('bnc_w', 'bnc_s', 'suk', 'sus'),
                           inline=T)
    } else if(vtype == "Part of Speech") {
      ui[[1]] <- radioButtons(sprintf('%s.opt', boxid), 'Corpus',
                              c('SUBTLEX-UK'='suk',
                                'BNC (written)'='bnc_w',
                                'BNC (spoken)'='bnc_s',
                                'English Lexicon Project (ELP)'='elp'),
                              selected='suk',
                              inline=T)
    } else if(vtype == "Bigram Probability") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Corpora',
                                    c('BNC (written)'='bnc.wbg', 'BNC (spoken)'='bnc.sbg', 'SUBTLEX-UK'='subtlex_uk.bg', 'SUBTLEX-US'='subtlex_us.bg'),
                                    c('bnc.wbg', 'bnc.sbg', 'subtlex_uk.bg', 'subtlex_us.bg'),
                                    inline=T)
    } else if(vtype == "Orthographic Neighbourhood") {
      ui[[1]] <- checkboxInput(sprintf('%s.log', boxid), 'Log Transform', 1)
      ui[[2]] <- radioButtons(sprintf('%s.opt', boxid), 'Measure',
                              c('Orthographic Levenshtein Distance 20 (OLD20)'='old20', "Coltheart's N"='cn'),
                              'old20',
                              inline=T)
    }
    
    ui[[length(ui)+1]] <- if (vtype != "(None)") {
      numericInput(sprintf("%s_Nlevels", boxid), 'Number of Levels', 2, min=2, max=100, step=1)
    } else {
      NULL
    }
    
    ui
    
  }
  
  
}


splitby_UI_sliders <- function(vtype, boxid, levels_N, box_opt, box_log, boxletter) {
  
  if (!is.null(levels_N)) {
    ui <- list()
    
    # defaults
    slider.range <- c(1, 10)
    slider.def_val <- c(4, 6)
    slider.step <- 0.5
    slider.valueABC <- NULL
    
    # variable-specific slider settings
    if (vtype == "Word Frequency") {
      slider.range <- c(1.1, 7.7)
      slider.def_val <- c(3, 3.5)
      slider.valueABC <- list(c(1.1, 1.6), c(7.2, 7.7), c(4.1, 4.6))
      slider.step <- 0.1
    } else if (vtype=="Length") {
      slider.range <- c(0, 25)
      slider.def_val <- c(20, 24)
      slider.valueABC <- list(c(3, 5), c(9, 11), c(6, 8))
      slider.step <- 1
    } else if (vtype=="Bigram Probability") {
      slider.range <- c(0, 0.042)
      slider.def_val <- c(0.02, 0.023)
      slider.valueABC <- list(c(.001, .003), c(.009, .011), c(.005, .007))
      slider.step <- 0.001
    } else if (vtype=="Orthographic Neighbourhood") {
      if (box_opt=="old20") {
        if (box_log) {
          slider.range <- c(0, 2.9)
          slider.def_val <- c(1, 1.2)
          slider.valueABC <- NULL
          slider.step <- .025
        } else {
          slider.range <- c(1, 18)
          slider.def_val <- c(7.5, 8)
          slider.valueABC <- NULL
          slider.step <- 0.25
        }
      } else if (box_opt=="cn") {
        if (box_log) {
          slider.range <- c(0, 4.24)
          slider.def_val <- c(1.5, 1.6)
          slider.valueABC <- NULL
          slider.step <- 0.025
        } else {
          slider.range <- c(0, 69)
          slider.def_val <- c(4, 8)
          slider.valueABC <- NULL
          slider.step <- 1
        }
      }
    }
    
    # build sliders
    if (vtype != "(None)") {
      for (i in 1:levels_N) {
        if (is.null(slider.valueABC) | i>3) {
          sl_val <- slider.def_val
        } else {
          sl_val <- slider.valueABC[[i]]
        }
        ui[[i]] <- sliderInput(sprintf("%s_sl%i", boxid, i),
                               label = sprintf("Level %s%i", boxletter, i),
                               min = slider.range[1],
                               max = slider.range[2],
                               value = sl_val,
                               step = slider.step)
      }
    }
    
    ui
  }
  
}

splitby_UI_vis <- function(vtype, boxid, levels_N, box_opt, box_log, lexops_df, shade_list, boxletter) {
  
  if (vtype != "(None)") {
    
    if (vtype == "Part of Speech") {
      
    } else {
      # numeric vtypes
      
      if (length(box_opt)==0 & !(vtype %in% c("Length"))) {
        error.plot("Select a Source", "primary")
      } else {
        
        get_rowmeans <- function(column, df) { rowMeans(select(df, one_of(column)), dims=1, na.rm=T) }
        
        if (vtype == "Word Frequency") {
          cn <- corpus_recode(box_opt, if(box_log){"Zipf"}else{"fpmw"})
          lexops_df$xval <- get_rowmeans(cn, lexops_df)
        } else if (vtype == "Length") {
          lexops_df$xval <- lexops_df$Length
        } else if (vtype == "Bigram Probability") {
          cn <- corpus_recode(box_opt, "BG")
          lexops_df$xval <- get_rowmeans(cn, lexops_df)
        } else if (vtype == "Orthographic Neighbourhood") {
          cn <- corpus_recode(box_opt, "ON", logprefix=box_log)
          lexops_df$xval <- get_rowmeans(cn, lexops_df)
        } else {
          cn <- corpus_recode(box_opt)
          lexops_df$xval <- get_rowmeans(cn, lexops_df)
        }
        
        shade_list_labels <- lapply(1:levels_N, function(i) sprintf("%s%i", boxletter, i))
        dens.plot(x="xval", redline=NA, shade=shade_list, df=lexops_df, shade_label=shade_list_labels, boxtype='primary')
      }
      
    }
    
  } else {
    
    error.plot("Select a Variable", "primary")
    
  }
  
}
