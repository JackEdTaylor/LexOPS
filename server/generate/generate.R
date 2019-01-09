genresults <- reactive({
  
  res <- lexops %>%
    select(string)
  
  # Filter by...
  
  lexops_filt <- lexops
  
  if (gen_filterby_boxes_N() >= 1) {
    
    for (i in 1:gen_filterby_boxes_N()) {
      lexops_custom_cols <- lexops
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
  }
  
  res <- filter(res, string %in% lexops_filt$string)
  
  # Split by...
  
  res <- mutate(res, Condition = NA)
  
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
  
  levels <- expand.grid(splits)
  levels <- unite(levels, "Level", 1:ncol(levels), sep="_", remove=F)
  
  lexops_custom_cols <- lexops[lexops$string %in% res$string, ]
  
  for (lvl in 1:nrow(levels)) {
    rowlevel <- levels[lvl, ]
    lexops_filt <- lexops[lexops$string %in% res$string, ]
    
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
  
  res
})
