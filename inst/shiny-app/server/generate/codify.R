# generate code to codify the stimulus generation
output$gen_codify_text <- renderText({

  # get the filters
  filter_opts <- filter_opts_react()

  # get the splits
  split_opts <- split_opts_react()

  # get the controls
  control_opts <- control_opts_react()

  # get the random seed info
  gen_seed <- gen_seed <- if (input$preference_use_a_random_seed) input$preference_random_seed else NA
  gen_seed_arg <- if (is.na(gen_seed)) "" else sprintf(", seed = %i", gen_seed)

  codify_var <- function(x) {
    if (grepl(" ", x)) {
      sprintf("`%s`", x)
    } else {
      x
    }
  }
  out <- "library(LexOPS)\n\nstim <- LexOPS::lexops"

  if (!is.null(input$cust_opts_inputfile)) {

    cust_file_ext <- tools::file_ext(input$cust_opts_inputfile$datapath)

    read_cust_fun <- if (cust_file_ext == "csv") {
      "read_csv"
    } else if (cust_file_ext == "tsv") {
      "read_tsv"
    } else if (cust_file_ext %in% c("xls", "xlsx", "xlsm")) {
      "read_excel"
    }

    read_cust_text <- sprintf("# check the code below correctly locates the file\n# (may require setting working directory)\ncustom_df <- %s(\"%s\") %%>%%\n\trename(string = \"%s\") %%>%%\n\trename_at(vars(-\"string\"), ~ sprintf(\"custom.%%s\", .))", read_cust_fun, input$cust_opts_inputfile$name, input$cust_opts_column)
    read_cust_package <- if (read_cust_fun == "read_excel") "readxl" else "readr"

    out <- sprintf("library(dplyr)\nlibrary(%s)\n\n%s\n\n%s %%>%%\n\tfull_join(custom_df, by = \"string\")", read_cust_package, read_cust_text, out)
  }

  if (gen_filterby_boxes_N() > 0) {
    for (i in 1:gen_filterby_boxes_N()) {
      if (is.numeric(lexops_react()[[filter_opts[[i]]$var]])) {
        out <- sprintf("%s %%>%%\n\tsubset(%s >= %s & %s <= %s)", out, codify_var(filter_opts[[i]]$var), formatC(filter_opts[[i]]$selection[1], format = "f", drop0trailing = TRUE), codify_var(filter_opts[[i]]$var), formatC(filter_opts[[i]]$selection[2], format = "f", drop0trailing = TRUE))
      } else {
        out <- sprintf("%s %%>%%\n\tsubset(%s %%in%% c(%s))", out, codify_var(filter_opts[[i]]$var), paste(sprintf("\"%s\"", filter_opts[[i]]$selection), collapse = ", "))
      }
    }
  }

  if (gen_splitby_boxes_N() > 0) {
    for (i in 1:gen_splitby_boxes_N()) {
      if (split_opts[[i]]$var == "Random") {
        out <- sprintf("%s %%>%%\n\tsplit_random(%i%s)", out, codify_var(split_opts[[i]]$n_levels), gen_seed_arg)
      } else {
        if (is.numeric(lexops_react()[[split_opts[[i]]$var]])) {
          split_sel_code <- paste(sapply(split_opts[[i]]$selection, function(l) sprintf("%g:%g", l[1], l[2])), collapse = " ~ ")
          out <- sprintf("%s %%>%%\n\tsplit_by(%s, %s)", out, codify_var(split_opts[[i]]$var), split_sel_code)
        } else {
          split_sel_quoted <- lapply(split_opts[[i]]$selection, function(l) {
            if (length(l) == 1) {
              sprintf("\"%s\"", l)
            } else {
              l
            }
          })
          split_sel_code <- paste(sprintf("%s", split_sel_quoted), collapse = " ~ ")
          out <- sprintf("%s %%>%%\n\tsplit_by(%s, %s)", out, codify_var(split_opts[[i]]$var), split_sel_code)
        }
      }
    }
  }

  if (gen_controlfor_boxes_N() > 0) {
    for (i in 1:gen_controlfor_boxes_N()) {
      if (is.numeric(lexops_react()[[control_opts[[i]]$var]])) {
        out <- sprintf("%s %%>%%\n\tcontrol_for(%s, %s)", out, codify_var(control_opts[[i]]$var), sprintf("%g:%g", control_opts[[i]]$selection[1], control_opts[[i]]$selection[2]))
      } else {
        out <- sprintf("%s %%>%%\n\tcontrol_for(%s)", out, codify_var(control_opts[[i]]$var))
      }
    }
  }

  if (gen_splitby_boxes_N() > 0 & gen_controlfor_boxes_N() > 0) {
    n <- if (input$gen_stim_n_all) "\"all\"" else input$gen_stim_n
    match_null <- gen_match_null()
    out <- sprintf("%s %%>%%\n\tgenerate(%s, \"%s\"%s)", out, n, match_null, gen_seed_arg)
  }

  out
})
