matchresults_undistanced <- reactive({
  
  tryCatch({
    matched <- lexopsReact() %>%
      select(string)
    
    lexops_match <- lexopsReact()
    
    if (matchboxes_N() >= 1) {
      
      for (i in 1:matchboxes_N()) {
        lexops_custom_cols <- lexopsReact()
        boxid <- sprintf('matchbox_%i', i)
        boxv <- input[[sprintf('%s_vtype', boxid)]]
        
        if (is.null(boxv)) {
          next  # skip iteration if the UI hasn't finished rendering
        }
        
        if (boxv != "(None)") {
          
          sl <- input[[sprintf('%s_sl', boxid)]]  # get the box's filter
          boxlog <- if (is.null(input[[sprintf('%s.log', boxid)]])) {F} else {input[[sprintf('%s.log', boxid)]]}
          boxopt <- if (is.null(input[[sprintf('%s.opt', boxid)]])) {""} else {input[[sprintf('%s.opt', boxid)]]}
          column <- corpus_recode_columns(boxopt, boxv, boxlog)
          # remove duplicates (fixes bug in rendering order)
          column <- unique(column)
          if (all(column %in% colnames(lexops_custom_cols))) {
            
            if (length(column)>1) {
              # create new column, which will be the average of the variables selected
              if (!(boxv %in% vis.cats.non_Zscore)) {
                lexops_custom_cols[column] <- lapply(lexops_custom_cols[column], scale)
              }
              colmeans_name <- sprintf("Avg.%s", viscat2prefix(boxv, boxlog))
              lexops_custom_cols[[colmeans_name]] <- rowMeans(select(lexops_custom_cols, one_of(column)), dims=1, na.rm=T)
              if (colmeans_name %in% colnames(matched)) {
                colmeans_name_orig <- colmeans_name
                colnr <- 1
                while (colmeans_name %in% colnames(matched)) {
                  colnr <- colnr + 1
                  colmeans_name <- sprintf("%s.%i", colmeans_name_orig, colnr)
                }
                colnames(lexops_custom_cols)[colnames(lexops_custom_cols)==colmeans_name_orig] <- colmeans_name
              }
              lexops_match <- inner_join(lexops_match, select(lexops_custom_cols, "string", UQ(sym(colmeans_name))), by="string")
              column <- colmeans_name
            }
            str_in_x <- lexops_custom_cols[[column]][lexops_custom_cols$string==input$matchstring]
            if (!column %in% colnames(matched)) {
              matched <- inner_join(matched, select(lexops_custom_cols, "string", UQ(sym(column))), by="string")  # copy over the column to the results df
            }
            if (is.numeric(lexops_match[[column]])) {
              lexops_match <- filter(lexops_match, UQ(sym(column)) >= str_in_x + sl[[1]] & UQ(sym(column)) <= str_in_x + sl[[2]])
            } else {
              lexops_match <- filter(lexops_match, UQ(sym(column)) == str_in_x)
            }
            
          }
          
        }
        
      }
      
      matched <- filter(matched, string %in% lexops_match$string)
    }
    
    # Get differences and distances
    matched_differences <- get_differences(matched, str_in=input$matchstring)
    matched_distances <- get_distances(matched_differences)
    
    # Select results format
    if (input$results.format=='rv') {
      res <- matched
    } else if (input$results.format=='diff') {
      res <- matched_differences
    } else if (input$results.format=='dist') {
      res <- matched_distances
    }
    
    res
  },
  
  error = function(cond) {
    return(NULL)
  }
  
  )
  
})



# add the distance measures if selected

matchresults_unsorted <- reactive ({
  
  res <- matchresults_undistanced()
  
  if (!is.null(res)) {
    
    # match by Euclidean Distance, using all columns is default, so assume this is true if the input is.null
    if (input$check.matchdist.ed) {
      target_cols <-
        if (!is.null(input$match_results_ed_all)) {
          if (input$match_results_ed_all=="manual") {
            input$match_results_ed_opts
          } else {
            colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
          }
        } else {
          colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
        }
      
      res <- res %>%
        mutate(Euclidean.Distance = get_euclidean_distance(res, input$matchstring, columns=target_cols)) %>%
        select(Euclidean.Distance, everything()) %>%
        select(string, everything())
    }
    
    if (input$check.matchdist.cb) {
      target_cols <-
        if (!is.null(input$match_results_cb_all)) {
          if (input$match_results_cb_all=="manual") {
            input$match_results_cb_opts
          } else {
            colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
          }
        } else {
          colnames(select_if(res, is.numeric))[!(colnames(select_if(res, is.numeric)) %in% c("Euclidean.Distance", "CityBlock.Distance"))]
        }
      
      res <- res %>%
        mutate(CityBlock.Distance = get_cityblock_distance(res, input$matchstring, columns=target_cols)) %>%
        select(CityBlock.Distance, everything()) %>%
        select(string, everything())
    }
    
  }

  res

})



# sorting, and removing the target word

matchresults <- reactive ({

  out <- matchresults_unsorted()
  
  if (!is.null(out)) {
    
    for (sortnr in 1:5) {
      if (sprintf("match_results_sort_%i", sortnr) %in% names(input)) {
        sortnr_string <- as.character(input[[sprintf("match_results_sort_%i", sortnr)]])
        sortnr_order <- as.character(input[[sprintf("match_results_sort_%i_order", sortnr)]])
        if (sortnr_string != "(None)") {
          if (sortnr_order == "Ascending") {
            out <- out %>%
              arrange(!!! sym(sortnr_string))
          } else {
            out <- out %>%
              arrange(desc(!!! sym(sortnr_string)))
          }
        }
      }
    }
    
    # sort by euclidean distance as default if the UIs haven't rendered
    if (is.null(input$match_results_sort_1_order)) {
      if (input$check.matchdist.ed & "Euclidean.Distance" %in% colnames(out)) {
        out <- arrange(out, Euclidean.Distance)
      }
    }
    
    out <- out %>%
      filter(string != input$matchstring)
    
  }
  
  out

})

