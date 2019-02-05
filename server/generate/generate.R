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
    
    for (i in 1:gen_filterby_boxes_N()) {
      lexops_custom_cols <- lexopsReact()
      boxid <- sprintf('gen_filterby_%i', i)
      sl <- input[[sprintf('%s_sl', boxid)]]  # get the box's filter
      boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
      boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
      boxv <- input[[sprintf('%s_vtype', boxid)]]
      column <- corpus_recode_columns(boxopt, boxv, boxlog)
      if (length(column)>1) {
        # create new column, which will be the average of the variables selected
        if (!(boxv %in% c("Word Frequency", "Bigram Probability"))) {
          lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
        }
        colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
        lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
        lexops_filt <- inner_join(lexops_filt, select(lexops_custom_cols, "string", UQ(sym(colmeans_name))), by="string")
        column <- colmeans_name
      }
      res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to the results df
      
      if (is.numeric(lexops_filt[[column]])) {
        lexops_filt <- filter(lexops_filt, UQ(sym(column)) >= sl[[1]] & UQ(sym(column)) <= sl[[2]])
      } else {
        lexops_filt <- filter(lexops_filt, UQ(sym(column)) %in% sl)
      }
    }
    
    res <- filter(res, string %in% lexops_filt$string)
  }
  
  
  # Split by...
  
  if (gen_splitby_boxes_N() >= 1) {
    
    res <- mutate(res, Condition = NA)
    
    levels <- genlevels()
    
    lexops_custom_cols <- lexopsReact()[lexopsReact()$string %in% res$string, ]
    
    for (lvl in 1:nrow(levels)) {
      rowlevel <- levels[lvl, ]
      lexops_filt <- lexopsReact()[lexopsReact()$string %in% res$string, ]
      
      for (i in 1:gen_splitby_boxes_N()) {
        i_lttr <- LETTERS[i]
        i_lttr_lvl <- as.numeric(rowlevel[[i_lttr]])  # get which level of the variable this cell belongs to
        boxid <- sprintf('gen_splitby_%i', i)
        sl <- input[[sprintf('%s_sl%i', boxid, i_lttr_lvl)]]  # get the level's filter
        boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
        boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
        boxv <- input[[sprintf('%s_vtype', boxid)]]
        column <- corpus_recode_columns(boxopt, boxv, boxlog)
        if (length(column)>1) {
          # create new column, which will be the average of the variables selected
          if (!(boxv %in% c("Word Frequency", "Bigram Probability"))) {
            lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
          }
          colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
          lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
          lexops_filt <- inner_join(lexops_filt, select(lexops_custom_cols, "string", UQ(sym(colmeans_name))), by="string")
          column <- colmeans_name
        }
        if (!(column %in% colnames(res))) {
          res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to the results df
        }
        if (is.numeric(lexops_filt[[column]])) {
          lexops_filt <- filter(lexops_filt, UQ(sym(column)) >= sl[[1]] & UQ(sym(column)) <= sl[[2]])
        } else {
          lexops_filt <- filter(lexops_filt, UQ(sym(column)) %in% sl)
        }
        
      }
      
      res$Condition[res$string %in% lexops_filt$string] <- rowlevel$Level
    }
    
    res <- filter(res, !is.na(Condition))
    
  }
  
  res
})


genresults <- reactive({
  
  res <- genresults_prematching()
  
  lexops_custom_cols <- lexopsReact()[lexopsReact()$string %in% res$string, ]
  
  # Control for...
  
  # Add relevant columns and store details
  control_tols <- list()  # will contain all controlled variables' names (in the res df) and associated tolerances
  if (gen_controlfor_boxes_N() >= 1) {
    
    for (i in 1:gen_controlfor_boxes_N()) {
      boxid <- sprintf('gen_controlfor_%i', i)
      boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
      boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
      boxv <- input[[sprintf('%s_vtype', boxid)]]
      column <- corpus_recode_columns(boxopt, boxv, boxlog)
      if (length(column)>1) {
        # create new column, which will be the average of the variables selected
        if (!(boxv %in% c("Word Frequency", "Bigram Probability"))) {
          lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
        }
        colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
        lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
        column <- colmeans_name
      }
      res[[column]] <- lexops_custom_cols[[column]]  # copy over the column to res df
      if (is.numeric(res[[column]])) {
        control_tols[[column]] <- input[[sprintf('%s_sl', boxid)]]  # get the box's filter and store under the column's name
      } else {
        control_tols[[column]] <- NA
      }
    }
    
    
    # get design's cells
    cells <- genlevels() %>%
      select(Level) %>%
      unlist(use.names=F)
    
    # control for the selected variables
    
    if (input$gen_check.dist) {
      dist_func <- if (input$gen_dist.opt=="cb") {get_cityblock_distance} else {get_euclidean_distance}
    }
    gen_controlnull <- if (is.null(input$gen_controlnull)) {"inclusive"} else {input$gen_controlnull}  # assume inclusive if selection not rendered yet
    numerics <- colnames(select_if(res, is.numeric))  # get a list of the numeric variables
    
    if (gen_controlnull == "inclusive") {
      # all stimuli (for all conditions) must be within tolerance relative to all other matched stimuli
      
    } else {
      # all stimuli (for all conditions) must be within tolerance relative to selected condition ("null" condition)
      none_NAs_count <- 0
      nullcond <- gen_controlnull
      otherconds <- cells[cells!=nullcond]
      newres <- res %>%
        filter(Condition == nullcond) %>%
        select(Condition, string) %>%
        mutate(spread_id=1:n()) %>%
        spread(Condition, string) %>%
        select(-spread_id) %>%
        slice(sample(1:n()))
      
      withProgress(message="Generating stimuli...", value=0, {
        
        for (itemnr in 1:nrow(newres)) {
          item_str <- newres[[nullcond]][itemnr]
          
          # get pool of potential matches, and shuffle randomly
          match_str_pool_base <- res
          if (input$gen_check.dist) {match_str_pool_base <- mutate(match_str_pool_base, dist = dist_func(res, item_str, names(control_tols)[names(control_tols) %in% numerics]))}
          
          for (condnr in 1:length(otherconds)) {
            condname <- otherconds[condnr]
            
            # get pool of potential matches, and shuffle randomly
            match_str_pool <- match_str_pool_base %>%
              filter(Condition == condname) %>%
              slice(sample(1:n()))
            
            for (control_nr in 1:length(names(control_tols))) {
              control_name <- names(control_tols)[control_nr]
              control_value <- res[[control_name]][res$string==item_str]
              if (control_name %in% numerics) {
                control_tol <- control_tols[[control_name]] + control_value
                if (input$gen_check.dist) {match_str_pool <- filter(match_str_pool, dist <= input$gen_dist_tol)}
                match_str_pool <- filter(match_str_pool, UQ(sym(control_name))>=control_tol[1] & UQ(sym(control_name))<=control_tol[2])
              } else {
                match_str_pool <- filter(match_str_pool, UQ(sym(control_name)) == control_value)
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
          }
          
          newres_test <- newres[1:itemnr, ]
          incProgress(nrow(na.omit(newres_test))/input$gen_N_stim, detail=sprintf("%i%%", round((nrow(na.omit(newres_test))/input$gen_N_stim*100))))
          if (nrow(newres_test) >= input$gen_N_stim & nrow(na.omit(newres_test)) >= input$gen_N_stim) {
            break
          }
          
        }
        
      })
      
      
      
      newres <- na.omit(newres)
      newres <- newres[1:input$gen_N_stim, ] %>%
        na.omit()
      
      validate(
        need(nrow(newres) >= input$gen_N_stim,
             sprintf("Insufficient pool size (requested random sample of %i words per condition, from total of %i combinations in generated pool). Consider regenerating, increasing tolerances, increasing sizes of splits, or reducing desired stimulus list size.", input$gen_N_stim, nrow(newres)))
      )
      
      newres <- newres %>%
        mutate(Item = row_number()) %>%
        select(Item, everything())
      
      res <- newres
    }
    
  }
  
  res
  
})