# render a table that suggests sources that need citing
output$gen_citations <- DT::renderDataTable({

  # get the filters
  filter_opts <- sapply(filter_opts_react(), function(x) x$var)

  # get the splits
  split_opts <- sapply(split_opts_react(), function(x) x$var)

  # get the controls
  control_opts <- sapply(control_opts_react(), function(x) x$var)

  # get the table
  c(filter_opts, split_opts, control_opts) %>%
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

})
