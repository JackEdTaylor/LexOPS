gensplits <- reactive({
  
  splits <- list()
  for (i in 1:gen_splitby_boxes_N()) {
    boxid <- sprintf('gen_splitby_%i', i)
    Nlevels <- input[[sprintf("%s_Nlevels", boxid)]]
    i_lttr <- LETTERS[i]
    if (!is.null(Nlevels)) {
      for (lvl in 1:Nlevels) {
        splits[[i_lttr]] <- append(splits[[i_lttr]], sprintf("%s%i", i_lttr, lvl))
      }
    }
  }
  
  splits
  
})

genlevels <- reactive({
  levels <- expand.grid(gensplits())
  unite(levels, "Level", 1:ncol(levels), sep="_", remove=F)
})


genresults_prematching <- reactive({
  
  input$gen_generate  # trigger recalculation if regenerate button is clicked
  
  res <- lexopsReact() %>%
    select(string)
  
  # Filter by...
  
  lexops_filt <- lexopsReact()
  
  if (gen_filterby_boxes_N() >= 1) {
    
    withProgress(message="Applying Filters...", {
      
      for (i in 1:gen_filterby_boxes_N()) {
        lexops_custom_cols <- lexopsReact()
        boxid <- sprintf('gen_filterby_%i', i)
        
        # get the box's filter
        sl <- if (input$preference.toleranceUI == 'slider') {
          input[[sprintf("%s_sl", boxid)]]
        } else if (input$preference.toleranceUI == 'numericinput') {
          c(input[[sprintf("%s_tol_lower", boxid)]], input[[sprintf("%s_tol_upper", boxid)]])
        }
        
        boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
        boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
        boxv <- input[[sprintf('%s_vtype', boxid)]]
        boxsource <- input[[sprintf("%s.source", boxid)]]
        
        if (boxv == "Phonological Neighbourhood") {
          column <- sprintf("%s.%s", corpus_recode(boxopt, "PN", logprefix=boxlog), corpus_recode(boxsource))
        } else {
          column <- corpus_recode_columns(boxopt, boxv, boxlog)
        }
        # remove duplicates (fixes bug in rendering order)
        column <- unique(column)
        if (length(column)>1) {
          # create new column, which will be the average of the variables selected
          if (!(boxv %in% vis.cats.non_Zscore)) {
            lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
          }
          colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
          lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
          if (colmeans_name %in% colnames(lexops_filt)) {lexops_filt <- select(lexops_filt, -UQ(sym(colmeans_name)))}  # avoid duplicate column names in lexops_filt
          lexops_filt <- inner_join(lexops_filt, select(lexops_custom_cols, "string", UQ(sym(colmeans_name))), by="string")
          column <- colmeans_name
        }
        # Handle Multiple Columns with Same Source
        # rename to colname.2, colname.3, colname.4 etc
        columnname_raw <- column
        columnname_raw_prefixed <- sprintf("filt.%s", columnname_raw)
        col_iter <- 1
        while (sprintf("filt.%s", column) %in% colnames(res)) {
          col_iter <- col_iter + 1
          column <- sprintf("%s.%i", columnname_raw, col_iter)
        }
        if (column != columnname_raw) {
          colnames(lexops_custom_cols)[colnames(lexops_custom_cols)==columnname_raw] <- column
          colnames(lexops_filt)[colnames(lexops_filt)==columnname_raw] <- column
        }
        # rename the original Avg.%s to Avg.%s.1 for consistency
        if (columnname_raw_prefixed %in% colnames(res) & !sprintf("%s.1", columnname_raw_prefixed) %in% colnames(res)) {
          colnames(res)[colnames(res)==columnname_raw_prefixed] <- sprintf("%s.1", columnname_raw_prefixed)
        }
        
        res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to the results df
        
        # Add "split", "cont" or "filt" as a prefix
        colnames(res)[colnames(res)==column] <- sprintf("filt.%s", column)
        
        if (is.numeric(lexops_filt[[column]])) {
          lexops_filt <- filter(lexops_filt, UQ(sym(column)) >= sl[[1]] & UQ(sym(column)) <= sl[[2]])
        } else {
          lexops_filt <- filter(lexops_filt, UQ(sym(column)) %in% sl)
        }
        
        incProgress(detail=sprintf("%s%%", round((i/gen_filterby_boxes_N())*100)))
      }
      
    })
    
    res <- filter(res, string %in% lexops_filt$string)
  }
  
  
  # Split by...
  
  if (gen_splitby_boxes_N() >= 1) {
    
    res <- mutate(res, Condition = NA)
    
    levels <- genlevels()
    
    iterated_letters <- c()
    
    withProgress(message="Processing Splits...", {
      
      splitprogress = 0
      splitcomplete = nrow(levels) * gen_splitby_boxes_N()
      
      for (lvl in 1:nrow(levels)) {
        rowlevel <- levels[lvl, ]
        lexops_filt <- lexopsReact()[lexopsReact()$string %in% res$string, ]
        
        for (i in 1:gen_splitby_boxes_N()) {
          lexops_custom_cols <- lexopsReact()[lexopsReact()$string %in% res$string, ]
          i_lttr <- LETTERS[i]
          i_lttr_lvl <- as.numeric(rowlevel[[i_lttr]])  # get which level of the variable this cell belongs to
          boxid <- sprintf('gen_splitby_%i', i)
          
          # get the level's filter
          sl <- if (input$preference.toleranceUI == 'slider') {
            input[[sprintf('%s_sl%i', boxid, i_lttr_lvl)]]
          } else if (input$preference.toleranceUI == 'numericinput') {
            c(input[[sprintf("%s_tol_lower%i", boxid, i_lttr_lvl)]], input[[sprintf("%s_tol_upper%i", boxid, i_lttr_lvl)]])
          }
          
          boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
          boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
          boxv <- input[[sprintf('%s_vtype', boxid)]]
          boxsource <- input[[sprintf("%s.source", boxid)]]
          
          if (boxv == "Phonological Neighbourhood") {
            column <- sprintf("%s.%s", corpus_recode(boxopt, "PN", logprefix=boxlog), corpus_recode(boxsource))
          } else {
            column <- corpus_recode_columns(boxopt, boxv, boxlog)
          }
          # remove duplicates (fixes bug from rendering order)
          column <- unique(column)
          if (length(column)>1) {
            # create new column, which will be the average of the variables selected
            if (!(boxv %in% vis.cats.non_Zscore)) {
              lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
            }
            colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
            lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
            if (colmeans_name %in% colnames(lexops_filt)) {lexops_filt <- select(lexops_filt, -UQ(sym(colmeans_name)))}  # avoid duplicate column names in lexops_filt
            lexops_filt <- inner_join(lexops_filt, select(lexops_custom_cols, "string", UQ(sym(colmeans_name))), by="string")
            column <- colmeans_name
          }
          
          # Handle Multiple Columns with Same Source
          # rename to colname.2, colname.3, colname.4 etc
          columnname_raw <- column
          column_newname <- sprintf("split.%s", column)
          col_iter <- 1
          while (column_newname %in% colnames(res)) {
            col_iter <- col_iter + 1
            column_newname <- sprintf("split.%s.%i", columnname_raw, col_iter)
          }
          
          if (i_lttr_lvl==1 & !i_lttr %in% iterated_letters) {  # only add the variable the first time a box refers to this variable (otherwise it'll be added twice)
            if (sprintf("split.%s", columnname_raw) %in% colnames(res)) {
              colnames(res)[colnames(res)==sprintf("split.%s", columnname_raw)] <- sprintf("split.%s.1", columnname_raw)  # rename the original Avg.%s to Avg.%s.1 for consistency
            }
            res[[column_newname]] <- lexops_custom_cols[[column]]  # copy over the column to the results df
          }
          if (is.numeric(lexops_filt[[column]])) {
            lexops_filt <- filter(lexops_filt, UQ(sym(column)) >= sl[[1]] & UQ(sym(column)) <= sl[[2]])
          } else {
            lexops_filt <- filter(lexops_filt, UQ(sym(column)) %in% sl)
          }
          
          iterated_letters <- c(iterated_letters, i_lttr)
          
        }
        
        res$Condition[res$string %in% lexops_filt$string] <- rowlevel$Level
        
        splitprogress <- splitprogress + 1
        incProgress(detail=sprintf("%s%%", round((splitprogress/splitcomplete)*100)))
      }
      
      res <- filter(res, !is.na(Condition))
      
    })
    
  }
  
  
  res
})


genresults_preformatting <- reactive({
  
  res <- genresults_prematching()
  
  # Control for...
  
  # Add relevant columns and store details
  if (gen_controlfor_boxes_N() >= 1 & gen_splitby_boxes_N() >= 1) {
    
    # Add relevant columns and store details
    control_tols <- list()  # will contain all controlled variables' names (in the res df) and associated tolerances
    columns_needing_1suffix <- c()  # will contain all columns which need renaming to have a ".1" suffix after for loop
    withProgress(message="Getting Control Variables...", {
      
      for (i in 1:gen_controlfor_boxes_N()) {
        lexops_custom_cols <- lexopsReact()[lexopsReact()$string %in% res$string, ]
        boxid <- sprintf('gen_controlfor_%i', i)
        boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
        boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
        boxv <- input[[sprintf('%s_vtype', boxid)]]
        boxsource <- input[[sprintf("%s.source", boxid)]]
        
        if (boxv == "Phonological Neighbourhood") {
          column <- sprintf("%s.%s", corpus_recode(boxopt, "PN", logprefix=boxlog), corpus_recode(boxsource))
        } else {
          column <- corpus_recode_columns(boxopt, boxv, boxlog)
        }
        # remove duplicates (fixes bug from rendering order)
        column <- unique(column)
        if (length(column)>1) {
          # create new column, which will be the average of the variables selected
          if (!(boxv %in% vis.cats.non_Zscore)) {
            lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
          }
          colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
          
          # Handle Multiple Columns with Same Source
          if (colmeans_name %in% colnames(res)) {
            colmeans_name_orig <- colmeans_name
            colnr <- 1
            while (colmeans_name %in% colnames(res)) {
              colnr <- colnr + 1
              colmeans_name <- sprintf("%s.%i", colmeans_name_orig, colnr)
            }
            colnames(lexops_custom_cols)[colnames(lexops_custom_cols)==colmeans_name_orig] <- colmeans_name
            # store variable name to rename original to Avg.%s.1 for consistency
            columns_needing_1suffix <- c(columns_needing_1suffix, colmeans_name_orig)
          }
          
          lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
          column <- colmeans_name
        }
        res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to res df
        
        #get the box's tolerance
        sl <- if (input$preference.toleranceUI == 'slider') {
          input[[sprintf("%s_sl", boxid)]]
        } else {
          c(input[[sprintf("%s_tol_lower", boxid)]], input[[sprintf("%s_tol_upper", boxid)]])
        }
        
        if (is.numeric(res[[column]])) {
          control_tols[[column]] <- sl  # store the box's tolerance under the column's name
        } else {
          control_tols[[column]] <- NA
        }
        incProgress(detail=sprintf("%s%%", round((i/gen_controlfor_boxes_N())*100)))
      }
      
    })
    
    # rename original variables to Avg.%s.1 for consistency
    for (column in columns_needing_1suffix) {
      colnames(res)[colnames(res)==column] <- sprintf("%s.1", column)
      names(control_tols)[names(control_tols)==column] <- sprintf("%s.1", column)
    }
    
    # add cont. prefix to indicate a "control for..." variable
    for (column in names(control_tols)) {
      colnames(res)[colnames(res)==column] <- sprintf("cont.%s", column)
      names(control_tols)[names(control_tols)==column] <- sprintf("cont.%s", column)
    }
    
    # get design's cells
    cells <- genlevels() %>%
      select(Level) %>%
      unlist(use.names=F)
    
    # control for the selected variables
    
    if (input$gen_check.dist) {
      dist_func <- if (input$gen_dist.opt=="cb") {get_cityblock_distance} else {get_euclidean_distance}
    }
    gen_controlnull <- if (is.null(input$gen_controlnull)) {cells[1]} else {input$gen_controlnull}  # assume first cell if selection not rendered yet
    numerics <- colnames(select_if(res, is.numeric))  # get a list of the numeric variables
    
    if (gen_controlnull == "random") {
      # randomly select one condition as "null condition" for each row of output (each item)
      newres <- res %>%
        select(Condition, string) %>%
        mutate(spread_id=1:n()) %>%
        spread(Condition, string) %>%
        select(-spread_id) %>%
        slice(sample(1:n()))
      newres$rownullcond <- sample(names(newres), nrow(newres), replace=T)
      
    } else {
      # all stimuli (for all conditions) must be within tolerance relative to selected condition ("null" condition)
      nullcond <- gen_controlnull
      otherconds <- cells[cells!=nullcond]
      newres <- res %>%
        select(Condition, string) %>%
        mutate(spread_id=1:n()) %>%
        spread(Condition, string) %>%
        select(-spread_id) %>%
        slice(sample(1:n()))
      
    }
    
    withProgress(message="Generating matches...", {
      
      none_NAs_items <- 0
      for (itemnr in 1:nrow(newres)) {
        
        if (gen_controlnull=="random") {
          nullcond <- newres$rownullcond[itemnr]
          otherconds <- cells[cells!=nullcond]
        }
        
        item_str <- newres[[nullcond]][itemnr]
        
        # get pool of potential matches, and shuffle randomly
        match_str_pool_base <- res
        if (input$gen_check.dist) {match_str_pool_base <- mutate(match_str_pool_base, dist = dist_func(res, item_str, names(control_tols)[names(control_tols) %in% numerics]))}
        
        none_NAs_this_item <- 0
        for (condnr in 1:length(otherconds)) {
          condname <- otherconds[condnr]
          
          # get pool of potential matches, and shuffle randomly
          match_str_pool <- match_str_pool_base %>%
            filter(Condition == condname) %>%
            slice(sample(1:n()))
          
          for (control_nr in 1:length(names(control_tols))) {
            control_name <- names(control_tols)[control_nr]
            control_value <- res[[control_name]][res$string==item_str]
            if (all(is.na(control_value))) {
              
              match_str_pool <- match_str_pool[0,]
              
            } else {
              
              if (control_name %in% numerics) {
                control_tol <- control_tols[[control_name]] + control_value
                if (input$gen_check.dist) {match_str_pool <- filter(match_str_pool, dist <= input$gen_dist_tol)}
                match_str_pool <- filter(match_str_pool, UQ(sym(control_name))>=control_tol[1] & UQ(sym(control_name))<=control_tol[2])
              } else {
                match_str_pool <- filter(match_str_pool, UQ(sym(control_name)) == control_value)
              }
              
            }
          }
          
          if (nrow(match_str_pool)>=1) {
            # choose random match from pool of possibilities
            match_str <- match_str_pool %>%
              sample_n(1) %>%
              select(string) %>%
              unlist(use.names=F)
            if (length(match_str)==0) {match_str <- NA}
            # remove from future pools to avoid duplications in stimuli list
            res <- filter(res, string != match_str)
          } else {
            match_str <- NA
          }
          
          # if (is.null(newres[[condname]])) {newres[[condname]] <- NA}  # ensure column exists
          newres[[condname]][itemnr] <- match_str
          
          if (!is.na(match_str)) {
            none_NAs_this_item <- none_NAs_this_item + 1
          }
        }
        
        if (none_NAs_this_item==length(otherconds)) {
          none_NAs_items <- none_NAs_items + 1
        }
        
        newres_test <- newres[1:itemnr, ]
        if (input$gen_limit_N == "N") {
          if (is.null(input$gen_N_stim)) {
            max_entries <- 50  # default if not yet rendered
          } else {
            max_entries <- input$gen_N_stim
          }
          incProgress(nrow(na.omit(newres_test))/max_entries, detail=sprintf("%i%%", round((nrow(na.omit(newres_test))/max_entries*100))))
        } else {
          max_entries <- Inf
          incProgress(itemnr/nrow(newres), detail=sprintf("%i%%", round(itemnr/nrow(newres)*100)))
        }
        
        if (none_NAs_items >= max_entries) {
          break
        }
        
      }
      
    })
    
    newres <- na.omit(newres)
    if (gen_controlnull=="random") {
      newres <- plyr::rename(newres, c("rownullcond"="Match_Null"))
    } else {
      newres <- mutate(newres, Match_Null=nullcond)
    }
    
    if (input$gen_limit_N == "N") {
      validate(
        need(nrow(newres) >= input$gen_N_stim,
             sprintf("Insufficient pool size (requested random sample of %i words per condition, from total of %i combinations in generated pool). Consider regenerating, increasing tolerances, increasing sizes of splits, or reducing desired stimulus list size.", input$gen_N_stim, nrow(newres)))
      )
    }
    
    newres <- newres %>%
      mutate(Item = row_number()) %>%
      select(Item, everything())
    
    res <- newres
    
  }
  
  res
  
})

genresults_longformat <- reactive({
  
  if (gen_controlfor_boxes_N() >= 1 & gen_splitby_boxes_N() >= 1) {
    
    res <- genresults_prematching()
    
    # Add relevant columns and store details
    control_tols <- list()  # will contain all controlled variables' names (in the res df) and associated tolerances
    columns_needing_1suffix <- c()  # will contain all columns which need renaming to have a ".1" suffix after for loop
    for (i in 1:gen_controlfor_boxes_N()) {
      lexops_custom_cols <- lexopsReact()[lexopsReact()$string %in% res$string, ]
      boxid <- sprintf('gen_controlfor_%i', i)
      boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
      boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
      boxv <- input[[sprintf('%s_vtype', boxid)]]
      boxsource <- input[[sprintf("%s.source", boxid)]]
      
      if (boxv == "Phonological Neighbourhood") {
        column <- sprintf("%s.%s", corpus_recode(boxopt, "PN", logprefix=boxlog), corpus_recode(boxsource))
      } else {
        column <- corpus_recode_columns(boxopt, boxv, boxlog)
      }
      # remove duplicates (fixes bug from rendering order)
      column <- unique(column)
      if (length(column)>1) {
        # create new column, which will be the average of the variables selected
        if (!(boxv %in% vis.cats.non_Zscore)) {
          lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
        }
        colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
        
        # Handle Multiple Columns with Same Source
        if (colmeans_name %in% colnames(res)) {
          colmeans_name_orig <- colmeans_name
          colnr <- 1
          while (colmeans_name %in% colnames(res)) {
            colnr <- colnr + 1
            colmeans_name <- sprintf("%s.%i", colmeans_name_orig, colnr)
          }
          colnames(lexops_custom_cols)[colnames(lexops_custom_cols)==colmeans_name_orig] <- colmeans_name
          # store variable name to rename original to Avg.%s.1 for consistency
          columns_needing_1suffix <- c(columns_needing_1suffix, colmeans_name_orig)
        }
        
        lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
        column <- colmeans_name
      }
      res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to res df
      #get the box's tolerance
      sl <- if (input$preference.toleranceUI == 'slider') {
        input[[sprintf("%s_sl", boxid)]]
      } else if (input$preference.toleranceUI == 'numericinput') {
        c(input[[sprintf("%s_tol_lower", boxid)]], input[[sprintf("%s_tol_upper", boxid)]])
      }
      
      if (is.numeric(res[[column]])) {
        control_tols[[column]] <- sl  # store the box's tolerance under the column's name
      } else {
        control_tols[[column]] <- NA
      }
    }
    
    # rename original variables to Avg.%s.1 for consistency
    for (column in columns_needing_1suffix) {
      colnames(res)[colnames(res)==column] <- sprintf("%s.1", column)
      names(control_tols)[names(control_tols)==column] <- sprintf("%s.1", column)
    }
    
    # add cont. prefix to indicate a "control for..." variable
    for (column in names(control_tols)) {
      colnames(res)[colnames(res)==column] <- sprintf("cont.%s", column)
      names(control_tols)[names(control_tols)==column] <- sprintf("cont.%s", column)
    }
    
    res <- genresults_preformatting() %>%
      gather("Condition", "string", 2:(ncol(genresults_preformatting())-1)) %>%
      left_join(res, by=c("string", "Condition")) %>%
      mutate(is_null = ifelse(Condition == Match_Null, T, F))
    
    # simple & fast euclidean and citblock distance functions
    euc.dist <- function(x1, x2) as.numeric(dist(rbind(x1, x2), method="euclidean"))
    cb.dist <- function(x1, x2) as.numeric(dist(rbind(x1, x2), method="manhattan"))
    
    # add euclidean and cityblock distance measures
    cont_cols <- colnames(res)[startsWith(colnames(res), "cont.") & sapply(res, is.numeric)]  # get list of the numeric columns that refer to controls
    res$cont.Euclidean.Distance <- NA
    res$cont.CityBlock.Distance <- NA
    if (length(cont_cols)>0) {
      for (itemnr in unique(res$Item)) {
        null_cols <- as.numeric(res[res$is_null==T & res$Item==itemnr, cont_cols])
        for (condname in unique(res$Condition)) {
          x_cols <- as.numeric(res[res$Condition==condname & res$Item==itemnr, cont_cols])
          res$cont.Euclidean.Distance[res$Condition==condname & res$Item==itemnr] <- euc.dist(x_cols, null_cols)
          res$cont.CityBlock.Distance[res$Condition==condname & res$Item==itemnr] <- cb.dist(x_cols, null_cols)
        }
      }
    }
    
    res <- res %>%
      select(-is_null) %>%
      select(Item, string, Condition, Match_Null, cont.Euclidean.Distance, cont.CityBlock.Distance, everything())
    
    # remove the distance columns if empty (e.g. only controlling for categorical variables)
    if (all(is.na(res$cont.Euclidean.Distance))) {
      res <- select(res, -cont.Euclidean.Distance)
    }
    if (all(is.na(res$cont.CityBlock.Distance))) {
      res <- select(res, -cont.CityBlock.Distance)
    }
    
  } else {
    
    res <- genresults_preformatting()
    
  }
  
  res
})

genresults <- reactive({
  # wide (default) or long format
  
  if (!is.null(input$gen_dataformat)) {
    
    if (input$gen_dataformat=="long" & gen_controlfor_boxes_N() >= 1 & gen_splitby_boxes_N() >= 1) {
      res <- genresults_longformat()
    } else {
      res <- genresults_preformatting()
    }
    
  } else {
    res <- genresults_preformatting()
  }
  
  res
})