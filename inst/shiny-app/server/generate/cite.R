# render a table that suggests sources that need citing
output$gen_citations <- DT::renderDataTable({

  if (gen_splitby_boxes_N() > 0 | gen_controlfor_boxes_N() > 0 | gen_filterby_boxes_N() > 0) {
    # get the splits
    split_opts <- if (gen_splitby_boxes_N() > 0) sapply(split_opts_react(), function(x) x$var) else c()

    # get the controls
    control_opts <- if (gen_controlfor_boxes_N() > 0) sapply(control_opts_react(), function(x) x$var) else c()

    # get the filters
    filter_opts <- if (gen_filterby_boxes_N() > 0) sapply(filter_opts_react(), function(x) x$var) else c()

    # get the table
    c(split_opts, control_opts, filter_opts) %>%
      LexOPS::citation_table() %>%
      dplyr::mutate(url = ifelse(
        is.na(url),
        NA,
        sprintf("<a href=\"%s\" target=\"_blank\", class=\"btn btn-primary\">Link to Source</a>", url)
      )) %>%
      dplyr::rename(
        Variable = var,
        Measure = measure,
        Source = source,
        Paper = url
      ) %>%
      DT::datatable(options = list(
        paging = FALSE,
        searching = FALSE,
        dom = "t"
      ), escape = FALSE)
  } else {
    NULL
  }

})
