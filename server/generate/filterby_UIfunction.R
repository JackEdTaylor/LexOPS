filterby_UI <- function(vtype = "Word Frequency", boxid) {
  
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
    
    ui
    
  }
  
  
}


filterby_UI_sliders <- function(vtype, boxid, box_opt, box_log, lexops_df) {
  
  ui <- NULL
  
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
    
    ui <- checkboxGroupInput(sprintf("%s_sl", boxid),
                             label = "Inclusion Criteria",
                             choices = selectChoices,
                             width="100%",
                             selected = selectChoices,
                             inline=T)
    
  } else if (length(box_opt)>=1 | vtype=="Length") {
    # Numerical Variables
    
    get_rowmeans <- function(column, df) { rowMeans(select(df, one_of(column)), dims=1, na.rm=T) }
    
    # defaults
    slider.range <- c(1, 10)
    slider.def_val <- c(4, 6)
    slider.step <- 0.5
    
    # variable-specific slider settings
    if (vtype == "Word Frequency") {
      if (box_log) {
        slider.range <- c(1.1, 7.7)
        slider.def_val <- c(2, 5)
        slider.step <- 0.1
      } else {
        slider.range <- c(0, 45000)
        slider.def_val <- c(100, 10000)
        slider.step <- 50
      }
    } else if (vtype=="Length") {
      slider.range <- c(0, 25)
      slider.def_val <- c(3, 13)
      slider.step <- 1
    } else if (vtype=="Bigram Probability") {
      slider.range <- c(0, 0.042)
      slider.def_val <- c(0.001, 0.02)
      slider.step <- 0.001
    } else if (vtype=="Orthographic Neighbourhood") {
      if (box_opt=="old20") {
        if (box_log) {
          slider.range <- c(0, 2.9)
          slider.def_val <- c(0.15, 1.8)
          slider.step <- .025
        } else {
          slider.range <- c(0, 18)
          slider.def_val <- c(1.25, 8)
          slider.step <- 0.25
        }
      } else if (box_opt=="cn") {
        if (box_log) {
          slider.range <- c(0, 4.24)
          slider.def_val <- c(0, 3)
          slider.step <- 0.025
        } else {
          slider.range <- c(0, 69)
          slider.def_val <- c(0, 40)
          slider.step <- 1
        }
      }
    } else if (vtype=="Syllables") {
      slider.range <- c(1, 11)
      slider.def_val <- c(1, 7)
      slider.step <- 1
    } else if (vtype=="Phonemes") {
      slider.range <- c(1, 20)
      slider.def_val <- c(3, 11)
      slider.step <- 1
    } else if (vtype=="Phonological Neighbourhood") {
      if (box_opt=="pld20") {
        if (box_log) {
          slider.range <- c(0, 2.9)
          slider.def_val <- c(0, 1.5)
          slider.step <- .025
        } else {
          slider.range <- c(0, 18)
          slider.def_val <- c(1, 6)
          slider.step <- 0.25
        }
      } else if (box_opt=="cn") {
        if (box_log) {
          slider.range <- c(0, 4.24)
          slider.def_val <- c(0, 4)
          slider.step <- 0.025
        } else {
          slider.range <- c(0, 180)
          slider.def_val <- c(0, 100)
          slider.step <- 1
        }
      }
    } else if (vtype=="Number of Pronunciations") {
      slider.range <- c(1, 4)
      slider.def_val <- c(1, 2)
      slider.step <- 1
    } else if (vtype %in% c("Familiarity", "Imageability", "Semantic Size", "Semantic Gender")) {
      if (length(box_opt)==1) {
        slider.range <- c(1, 7)
        slider.def_val <- c(2, 6)
        slider.step <- 0.1
      } else {
        cn <- corpus_recode(box_opt, viscat2prefix(vtype))
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(-1.5, 1.5)
        slider.step <- .05
      }
    } else if (vtype %in% c("Dominance", "Arousal", "Valence")) {
      if (length(box_opt)==1) {
        slider.range <- c(1, 9)
        slider.def_val <- c(2.5, 7.5)
        slider.step <- 0.1
      } else {
        cn <- corpus_recode(box_opt, viscat2prefix(vtype))
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(-1.5, 1.5)
        slider.step <- .05
      }
    } else if (vtype == "Concreteness") {
      if (length(box_opt)==1) {
        slider.range <- if (box_opt=="brysbaert") c(1, 5) else c(1, 7)
        slider.def_val <- if (box_opt=="brysbaert") c(2, 4) else c(2, 6)
        slider.step <- 0.1
      } else {
        cn <- corpus_recode(box_opt, "CNC")
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(-1.5, 1.5)
        slider.step <- .05
      }
    } else if (vtype == "Age of Acquisition") {
      if (length(box_opt)==1) {
        slider.range <- if (box_opt=="bb") c(2, 14) else if (box_opt=="kuperman") c(0, 25) else c(1, 7)
        slider.def_val <- if (box_opt=="bb") c(4, 12) else if (box_opt=="kuperman") c(5, 16) else c(2, 6)
        slider.step <- if (box_opt=="bb") 1 else 0.1
      } else {
        cn <- corpus_recode(box_opt, "AoA")
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(-1.5, 1.5)
        slider.step <- .05
      }
    } else if (vtype=="Humour") {
      slider.range <- c(1, 5)
      slider.def_val <- c(1.5, 3.5)
      slider.step <- 0.1
    } else if (vtype == "Lexical Decision Response Time") {
      if (length(box_opt)==1) {
        slider.range <- c(300, 1700)
        slider.def_val <- c(500, 850)
        slider.step <- 10
      } else {
        cn <- corpus_recode(box_opt, viscat2prefix(vtype))
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
        xval <- get_rowmeans(cn, lexops_df)
        slider.range <- c(floor(min(xval, na.rm=T)), ceiling(max(xval, na.rm=T)))
        slider.def_val <- c(-2, 2)
        slider.step <- .05
      }
    } else if (vtype == "Lexical Decision Accuracy") {
      slider.range <- c(0, 1)
      slider.def_val <- c(0.5, 1)
      slider.step <- 0.01
    } else if (vtype == "Custom Variable") {
      cn <- corpus_recode(box_opt, viscat2prefix(vtype))
      if (length(box_opt)!=1) {
        lexops_df[cn] <- lapply(lexops_df[cn], scale)
      }
      xval <- get_rowmeans(cn, lexops_df)
      fl <- floor(min(xval, na.rm=T))
      ce <- ceiling(max(xval, na.rm=T))
      slider.range <- c(fl, ce)
      slider.step <- case_when(
        diff(slider.range) >= 100 ~ 10,
        diff(slider.range) >= 10 ~ 1,
        diff(slider.range) >= 1 & diff(slider.range) < 10 ~ 0.1,
        diff(slider.range) < 1 ~ 0.01
      )
      slider.def_val <- c(fl + diff(slider.range)/4, ce - diff(slider.range)/4)
      slider.def_val <- case_when(
        diff(slider.range) >= 10 ~ c(floor(slider.def_val[1]), ceiling(slider.def_val[2])),
        diff(slider.range) >= 1 & diff(slider.range) < 10 ~ c(floor_dec(slider.def_val[1], 1), ceiling_dec(slider.def_val[2], 1)),
        diff(slider.range) < 1 ~ c(floor_dec(slider.def_val[1], 2), ceiling_dec(slider.def_val[2], 2))
      )
    }
    
    # build sliders
    if (vtype != "(None)") {
      sl_val <- slider.def_val
      ui <- sliderInput(sprintf("%s_sl", boxid),
                        label = "Inclusion Criteria",
                        min = slider.range[1],
                        max = slider.range[2],
                        value = sl_val,
                        step = slider.step)
    }
    
    
  }
    
  ui
}

filterby_UI_vis <- function(vtype, boxid, box_opt, box_log, box_source, lexops_df, shade_list) {
  
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
        error.plot("Select a Source", "info")
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
          if (all(box_opt=="old20")) scaletext <- c("Larger Neighbourhood", "Smaller Neighbourhood")
          if (all(box_opt=="cn")) scaletext <- c("Smaller Neighbourhood", "Larger Neighbourhood")
          if (all(box_opt=="cn" & !box_log)) force.histogram <- T
        } else if (vtype == "Phonological Neighbourhood") {
          cn <- sprintf("%s.%s", corpus_recode(box_opt, "PN", logprefix=box_log), corpus_recode(box_source))
          if (all(box_opt=="pld20")) scaletext <- c("Larger Neighbourhood", "Smaller Neighbourhood")
          if (all(box_opt=="cn")) scaletext <- c("Smaller Neighbourhood", "Larger Neighbourhood")
          if (all(box_opt=="cn") & !box_log) force.histogram <- T
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
        dens.plot(x="xval", redline=NA, shade=shade_list, df=lexops_df, boxtype='info', text.lowscale=scaletext[1], text.highscale=scaletext[2], force.histogram=force.histogram)
      }
      
    }
    
  } else {
    
    error.plot("Select a Variable", "info")
    
  }
  
}
