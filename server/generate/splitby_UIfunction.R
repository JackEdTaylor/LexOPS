splitby_UI <- function(vtype = "Word Frequency", boxid) {
  
  if (!is.null(vtype)) {
    
    ui <- list()
    
    if (vtype == "Word Frequency") {
      ui[[1]] <- checkboxInput(sprintf('%s.log', boxid), 'Use Zipf Scale', 1)
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
    } else if(vtype == "Syllables") {
      ui[[1]] <- radioButtons(sprintf('%s.opt', boxid), 'Source(s)',
                              c('CMU Pronouncing Dictionary'='cmu', 'Moby Project'='mp'),
                              'cmu',
                              inline=T)
    } else if(vtype == "Phonemes") {
      ui[[1]] <- radioButtons(sprintf('%s.opt', boxid), 'Source(s)',
                              c('CMU Pronouncing Dictionary'='cmu'),
                              'cmu',
                              inline=T)
    } else if(vtype == "Phonological Neighbourhood") {
      ui[[1]] <- checkboxInput(sprintf('%s.log', boxid), 'Log Transform', 1)
      ui[[2]] <- radioButtons(sprintf('%s.source', boxid), 'Source(s)',
                              c('CMU Pronouncing Dictionary'='cmu'),
                              'cmu',
                              inline=T)
      ui[[3]] <- radioButtons(sprintf('%s.opt', boxid), 'Measure',
                              c('Phonological Levenshtein Distance 20 (PLD20)'='pld20', "Coltheart's N"='cn'),
                              'pld20',
                              inline=T)
    } else if(vtype == "Number of Pronunciations") {
      ui[[1]] <- radioButtons(sprintf('%s.opt', boxid), 'Source(s)',
                              c('CMU Pronouncing Dictionary'='cmu'),
                              'cmu',
                              inline=T)
    } else if(vtype == "Familiarity") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Glasgow Norms'='gn', 'Clark and Paivio (2004)'='cp'),
                                    selected='gn',
                                    inline=T)
    } else if(vtype == "Age of Acquisition") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Kuperman et al. (2012)'='kuperman', 'Glasgow Norms'='gn', 'Brysbaert & Biemiller (2017)'='bb'),
                                    selected='kuperman',
                                    inline=T)
    } else if(vtype == "Concreteness") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Brysbaert et al. (2014)'='brysbaert', 'Glasgow Norms'='gn'),
                                    selected='brysbaert',
                                    inline=T)
    } else if(vtype == "Imageability") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Glasgow Norms'='gn', 'Clark and Paivio (2004)'='cp'),
                                    selected='gn',
                                    inline=T)
    } else if(vtype == "Arousal") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                    selected='warriner',
                                    inline=T)
    } else if(vtype == "Valence") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                    selected='warriner',
                                    inline=T)
    } else if(vtype == "Dominance") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Warriner et al. (2013)'='warriner', 'Glasgow Norms'='gn'),
                                    selected='warriner',
                                    inline=T)
    } else if(vtype == "Semantic Size") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Glasgow Norms'='gn'),
                                    selected='gn',
                                    inline=T)
    } else if(vtype == "Semantic Gender") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Glasgow Norms'='gn'),
                                    selected='gn',
                                    inline=T)
    } else if(vtype == "Humour") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Engelthaler & Hills (2018)'='eh'),
                                    selected='eh',
                                    inline=T)
    } else if (vtype == "Word Prevalence") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Brysbaert et al. (2018)'='brysbaert'),
                                    selected='brysbaert',
                                    inline=T)
    } else if (vtype == "Proportion Known") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('Brysbaert et al. (2018)'='brysbaert'),
                                    selected='brysbaert',
                                    inline=T)
    } else if(vtype == "Lexical Decision Response Time") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('British Lexicon Project (BLP)'='blp', 'English Lexicon Project (ELP)'='elp'),
                                    selected='blp',
                                    inline=T)
    } else if(vtype == "Lexical Decision Accuracy") {
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    c('British Lexicon Project (BLP)'='blp', 'English Lexicon Project (ELP)'='elp'),
                                    selected='blp',
                                    inline=T)
    } else if(vtype == "Custom Variable") {
      customopts <- vis.opt.2.source(vtype, visualise.opts())
      customopts <- gsub("custom.", "", customopts)
      ui[[1]] <- checkboxGroupInput(sprintf('%s.opt', boxid), 'Source(s)',
                                    customopts,
                                    selected = customopts[1],
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


splitby_UI_sliders <- function(vtype, boxid, levels_N, box_opt, box_log, boxletter, lexops_df, toleranceUIopt="slider") {
  
  ui <- list()
  
  if (!is.null(levels_N)) {
    
    # Categorical Variables
    if (vtype %in% c("Part of Speech", "Rhyme")) {
      
      if (vtype == "Part of Speech") {
        selectChoices <- lexops %>%
          group_by(!!sym(corpus_recode(box_opt, "PoS"))) %>%
          summarise(n=n()) %>%
          arrange(desc(n)) %>%
          na.omit()
        selectChoices <- unlist(selectChoices[1], use.names=F)
      }
      
      for (i in 1:levels_N) {
        ui[[i]] <- checkboxGroupInput(sprintf("%s_sl%i", boxid, i),
                                      label = sprintf("Level %s%i", boxletter, i),
                                      choices = selectChoices,
                                      width="100%",
                                      selected = selectChoices[i],
                                      inline=T)
      }
      
    } else if (length(box_opt)>=1 | vtype=="Length") {
      # Numerical Variables
      
      get_rowmeans <- function(column, df) { rowMeans(select(df, one_of(column)), dims=1, na.rm=T) }
      
      # defaults
      slider.range <- c(1, 10)
      slider.def_val <- c(4, 6)
      slider.step <- 0.5
      slider.valueABC <- NULL
      
      # variable-specific slider settings
      if (vtype == "Word Frequency") {
        if (box_log) {
          slider.range <- c(1.1, 7.7)
          slider.def_val <- c(3, 3.5)
          slider.valueABC <- list(c(1.1, 1.6), c(4.1, 4.6), c(2.6, 3.1))
          slider.step <- 0.1
        } else {
          slider.range <- c(0, 45000)
          slider.def_val <- c(500, 1000)
          slider.valueABC <- list(c(50, 500), c(20000, 40000), c(1000, 10000))
          slider.step <- 50
        }
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
            slider.valueABC <- list(c(.1, .3), c(1.45, 1.65), c(.775, .975))
            slider.step <- .025
          } else {
            slider.range <- c(0, 18)
            slider.def_val <- c(7.5, 8)
            slider.valueABC <- list(c(1.5, 2), c(3.5, 4), c(2.5, 3))
            slider.step <- 0.25
          }
        } else if (box_opt=="cn") {
          if (box_log) {
            slider.range <- c(0, 4.24)
            slider.def_val <- c(1.5, 1.6)
            slider.valueABC <- list(c(.55, .85), c(3.85, 4.15), c(2.2, 2.5))
            slider.step <- 0.025
          } else {
            slider.range <- c(0, 69)
            slider.def_val <- c(4, 8)
            slider.valueABC <- list(c(1, 5), c(18, 22), c(37, 41))
            slider.step <- 1
          }
        }
      } else if (vtype=="Syllables") {
        slider.range <- c(1, 11)
        slider.def_val <- c(8, 11)
        slider.valueABC <- list(c(1, 2), c(5, 7), c(3, 4))
        slider.step <- 1
      } else if (vtype=="Phonemes") {
        slider.range <- c(1, 20)
        slider.def_val <- c(8, 11)
        slider.valueABC <- list(c(2, 4), c(7, 9), c(5, 6))
        slider.step <- 1
      } else if (vtype=="Phonological Neighbourhood") {
        if (box_opt=="pld20") {
          if (box_log) {
            slider.range <- c(0, 2.9)
            slider.def_val <- c(1, 1.2)
            slider.valueABC <- list(c(.1, .3), c(1.45, 1.65), c(.775, .975))
            slider.step <- .025
          } else {
            slider.range <- c(0, 18)
            slider.def_val <- c(7.5, 8)
            slider.valueABC <- list(c(1.5, 2), c(3.5, 4), c(2.5, 3))
            slider.step <- 0.25
          }
        } else if (box_opt=="cn") {
          if (box_log) {
            slider.range <- c(0, 4.24)
            slider.def_val <- c(1.5, 1.6)
            slider.valueABC <- list(c(.55, .85), c(3.85, 4.15), c(2.2, 2.5))
            slider.step <- 0.025
          } else {
            slider.range <- c(0, 180)
            slider.def_val <- c(4, 8)
            slider.valueABC <- list(c(1, 5), c(120, 180), c(40, 50))
            slider.step <- 1
          }
        }
      } else if (vtype=="Number of Pronunciations") {
        slider.range <- c(1, 4)
        slider.def_val <- c(4, 4)
        slider.valueABC <- list(c(1, 1), c(2, 2), c(3, 3))
        slider.step <- 1
      } else if (vtype %in% c("Familiarity", "Imageability", "Semantic Size", "Semantic Gender")) {
        if (length(box_opt)==1) {
          slider.range <- c(1, 7)
          slider.def_val <- c(2.5, 3)
          slider.valueABC <- list(c(1, 2.5), c(5.5, 7), c(3.25, 4.75))
          slider.step <- 0.1
        } else {
          cn <- corpus_recode(box_opt, viscat2prefix(vtype))
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
          xval <- get_rowmeans(cn, lexops_df)
          slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
          slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
          slider.valueABC <- list(c(-2, -1.75), c(1.75, 2), c(-0.1, 0.1))
          slider.step <- .05
        }
      } else if (vtype %in% c("Dominance", "Arousal", "Valence")) {
        if (length(box_opt)==1) {
          slider.range <- c(1, 9)
          slider.def_val <- c(2.5, 3)
          slider.valueABC <- list(c(1, 3), c(7, 9), c(4.5, 5.5))
          slider.step <- 0.1
        } else {
          cn <- corpus_recode(box_opt, viscat2prefix(vtype))
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
          xval <- get_rowmeans(cn, lexops_df)
          slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
          slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
          slider.valueABC <- list(c(-2, -1.75), c(1.75, 2), c(-0.1, 0.1))
          slider.step <- .05
        }
      } else if (vtype == "Concreteness") {
        if (length(box_opt)==1) {
          slider.range <- if (box_opt=="brysbaert") c(1, 5) else c(1, 7)
          slider.def_val <- if (box_opt=="brysbaert") c(3, 5) else c(2.5, 3)
          slider.valueABC <- if (box_opt=="brysbaert") list(c(1, 1.5), c(4.5, 5), c(2.5, 3.5)) else list(c(1, 2.5), c(5.5, 7), c(3.25, 4.75))
          slider.step <- 0.1
        } else {
          cn <- corpus_recode(box_opt, "CNC")
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
          xval <- get_rowmeans(cn, lexops_df)
          slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
          slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
          slider.valueABC <- list(c(-2, -1.75), c(1.75, 2), c(-0.1, 0.1))
          slider.step <- .05
        }
      } else if (vtype == "Age of Acquisition") {
        if (length(box_opt)==1) {
          slider.range <- if (box_opt=="bb") c(2, 14) else if (box_opt=="kuperman") c(0, 25) else c(1, 7)
          slider.def_val <- if (box_opt=="bb") c(5, 7) else if (box_opt=="kuperman") c(6, 7) else c(2.5, 3)
          slider.valueABC <- if (box_opt=="bb") {
            list(c(2, 4), c(12, 14), c(6, 10))
          } else if (box_opt=="kuperman") {
            list(c(0, 5), c(15, 20), c(7.5, 12.5))
          } else {
            list(c(1, 2.5), c(5.5, 7), c(3.25, 4.75))
          }
          slider.step <- if (box_opt=="bb") 1 else 0.1
        } else {
          cn <- corpus_recode(box_opt, "AoA")
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
          xval <- get_rowmeans(cn, lexops_df)
          slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
          slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
          slider.valueABC <- list(c(-2, -1.75), c(1.75, 2), c(-0.1, 0.1))
          slider.step <- .05
        }
      } else if (vtype == "Humour") {
        slider.range <- c(1, 5)
        slider.def_val <- c(1.5, 3.5)
        slider.valueABC <- list(c(1, 1.7), c(3.3, 5), c(2, 3))
        slider.step <- 0.1
      } else if (vtype == "Word Prevalence") {
        slider.range <- c(-2, 2.6)
        slider.def_val <- c(-0.5, 0.5)
        slider.valueABC <- list(c(-2, 0), c(2, 3), c(0.5, 1.5))
        slider.step <- 0.1
      } else if (vtype == "Proportion Known") {
        slider.range <- c(0, 1)
        slider.def_val <- c(0.25, 0.35)
        slider.valueABC <- list(c(0, 0.35), c(0.9, 1), c(0.45, 0.75))
        slider.step <- 0.01
      } else if (vtype == "Lexical Decision Response Time") {
        if (length(box_opt)==1) {
          slider.range <- c(300, 1700)
          slider.def_val <- c(400, 450)
          slider.valueABC <- list(c(400, 500), c(900, 1000), c(650, 750))
          slider.step <- 10
        } else {
          cn <- corpus_recode(box_opt, viscat2prefix(vtype))
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
          xval <- get_rowmeans(cn, lexops_df)
          slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
          slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
          slider.valueABC <- list(c(-2, -1.75), c(1.75, 2), c(-0.1, 0.1))
          slider.step <- .05
        }
      } else if (vtype == "Lexical Decision Accuracy") {
        slider.range <- c(0, 1)
        slider.def_val <- c(0.25, 0.35)
        slider.valueABC <- list(c(0, 0.35), c(0.9, 1), c(0.45, 0.75))
        slider.step <- 0.01
      } else if (vtype == "Custom Variable") {
        cn <- corpus_recode(box_opt, viscat2prefix(vtype))
        if (length(box_opt)!=1) {
          lexops_df[cn] <- lapply(lexops_df[cn], scale)
        }
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(median(slider.range)-0.1, median(slider.range)+0.1)
        slider.valueABC <- list(slider.def_val-0.5, slider.def_val+0.5, slider.def_val)
        slider.step <- .05
      }
      
      # build sliders
      if (vtype != "(None)") {
        for (i in 1:levels_N) {
          if (is.null(slider.valueABC) | i>3) {
            sl_val <- slider.def_val
          } else {
            sl_val <- slider.valueABC[[i]]
          }
          
          if (toleranceUIopt=="slider") {
            ui[[i]] <- sliderInput(sprintf("%s_sl%i", boxid, i),
                              label = sprintf("Level %s%i", boxletter, i),
                              min = slider.range[1],
                              max = slider.range[2],
                              value = sl_val,
                              step = slider.step)
          } else if (toleranceUIopt=="numericinput") {
            
            if (length(sl_val)==1) {
              sl_val <- c(0, sl_val)
            }
            
            ui[[i]] <- fluidRow(
              column(6, numericInput(sprintf("%s_tol_lower%i", boxid, i),
                                     label = sprintf("Level %s%i Lower Limit", boxletter, i),
                                     value = sl_val[1],
                                     step = slider.step)),
              column(6, numericInput(sprintf("%s_tol_upper%i", boxid, i),
                                     label = sprintf("Level %s%i Upper Limit", boxletter, i),
                                     value = sl_val[2],
                                     step = slider.step))
            )
            
          }
        }
      }
      
    
    }
      
    }
    
  ui
}

splitby_UI_vis <- function(vtype, boxid, levels_N, box_opt, box_log, box_source, lexops_df, shade_list, boxletter) {
  
  if (vtype != "(None)") {
    
    if (vtype == "Part of Speech") {
      
      pos.plot(xname = corpus_recode(box_opt, "PoS"),
               selected=T,
               PoS = shade_list,
               df = lexops)
      
    } else {
      # numeric vtypes
      
      force.histogram <- F
      
      if (length(box_opt)==0 & !(vtype %in% c("Length"))) {
        error.plot("Select a Source", "primary")
      } else {
        
        get_rowmeans <- function(column, df) { rowMeans(select(df, one_of(column)), dims=1, na.rm=T) }
        
        if (vtype == "Word Frequency") {
          cn <- corpus_recode(box_opt, if(box_log){"Zipf"}else{"fpmw"})
          scaletext <- c("Less Frequent", "More Frequent")
        } else if (vtype == "Length") {
          cn <- "Length"
          scaletext <- c("Shorter", "Longer")
          force.histogram <- T
        } else if (vtype == "Bigram Probability") {
          cn <- corpus_recode(box_opt, viscat2prefix(vtype))
          scaletext <- viscat2scaletext(vtype)
        } else if (vtype == "Orthographic Neighbourhood") {
          cn <- corpus_recode(box_opt, "ON", logprefix=box_log)
          if (box_opt=="old20") scaletext <- c("Larger Neighbourhood", "Smaller Neighbourhood")
          if (box_opt=="cn") scaletext <- c("Smaller Neighbourhood", "Larger Neighbourhood")
          if (box_opt=="cn" & !box_log) force.histogram <- T
        } else if (vtype == "Phonological Neighbourhood") {
          cn <- sprintf("%s.%s", corpus_recode(box_opt, "PN", logprefix=box_log), corpus_recode(box_source))
          if (box_opt=="pld20") scaletext <- c("Larger Neighbourhood", "Smaller Neighbourhood")
          if (box_opt=="cn") scaletext <- c("Smaller Neighbourhood", "Larger Neighbourhood")
          if (box_opt=="cn" & !box_log) force.histogram <- T
        } else if (vtype == "Number of Pronunciations") {
          cn <- "CMU.PrN"
          scaletext <- c("Fewer", "More")
          force.histogram <- T
        } else if (vtype == "Lexical Decision Accuracy") {
          cn <- corpus_recode(box_opt, prefix="Accuracy")
          scaletext <- c("Less Accurate", "More Accurate")
        } else {
          if ((vtype %in% c("Phonemes", "Syllables")) | (vtype=="Age of Acquisition" & all(box_opt=="bb"))) force.histogram <- T
          cn <- corpus_recode(box_opt, viscat2prefix(vtype))
          if (length(box_opt)>1) lexops_df[cn] <- lapply(lexops_df[cn], scale)
          scaletext <- viscat2scaletext(vtype)
        }
        
        lexops_df$xval <- get_rowmeans(cn, lexops_df)
        shade_list_labels <- lapply(1:levels_N, function(i) sprintf("%s%i", boxletter, i))
        dens.plot(x="xval", redline=NA, shade=shade_list, df=lexops_df, shade_label=shade_list_labels, boxtype='primary', text.lowscale=scaletext[1], text.highscale=scaletext[2], force.histogram=force.histogram)
      }
      
    }
    
  } else {
    
    error.plot("Select a Variable", "primary")
    
  }
  
}
