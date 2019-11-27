# render a table that suggests sources that need citing
output$match_citations <- DT::renderDataTable({

  # get the filters
  filter_opts <- sapply(match_filter_opts_react(), function(x) x$var)

  # get the matches
  match_opts <- sapply(match_matchby_opts_react(), function(x) x$var)

  # get the table
  c(filter_opts, match_opts) %>%
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
