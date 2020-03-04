#' Generate stimuli
#'
#' Generates the stimuli from the data frame after it has been passed through `split_by()`, and optionally, `control_for()`. Will generate `n` items per condition. If <`n` items can be generated, will generate as many as possible given the experiment's design. Can be reproducible with `set.seed()`.
#'
#' @param df A data frame that is the result from `control_for()` or `split_by()`.
#' @param n The number of strings per condition (default = 20). Set to `"all"` to generate as many as possible.
#' @param match_null The condition words should be matched to. Should be a string indicating condition (e.g. `"A1_B2_C1"`), or a string indicating one of the following options: "first" for the lowest condition (e.g. `"A1"` or `"A1_B1_C1_D1"`, etc.), "random" for randomly selected null condition each iteration, "balanced" for randomly ordered null conditions with (as close to as possible) equal number of selections for each condition.
#' @param seed An integer specifying the random seed, allowing reproduction of exact stimuli lists. If `NA`, will not set the seed. Default is `NA`.
#' @param silent Logical: should output to the console (via `cat()`) be suppressed? Default is FALSE.
#' @param is_shiny Allows printing in a shiny context with `shinyjs::html()`. Outputs from the cat() function are stored in the div with id "gen_console". Default is FALSE.
#'
#' @return Returns the generated stimuli.
#' @examples
#'
#' # Generate 20 words per condition, for design with 3 levels of syllables, controlled for frequency
#' lexops %>%
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   generate(n = 20)
#'
#' # Generate 2 levels of bigram probability, controlling for frequency and length
#' # (Note that the matching null is balanced across all stimuli)
#' # (Also note that the data is filtered by proportion known to be >75%)
#' lexops %>%
#'   dplyr::filter(PK.Brysbaert >= .75) %>%
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   control_for(Length) %>%
#'   generate(n = 1000, match_null = "balanced")
#'
#' # Generate stimuli for a concreteness x valence (2 x 3) design
#' # (Note that abstract, neutral is set as the matching null)
#' lexops %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
#'   control_for(Length) %>%
#'   generate(n = 30, match_null = "A2_B2")
#'
#' # As above but with inclusive tolerance
#' # (all words are within the specified tolerances relative to each other)
#' lexops %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
#'   control_for(Length) %>%
#'   generate(n = 30, match_null = "inclusive")
#'
#' # Bypass non-standard evaluation
#' lexops %>%
#'  split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) %>%
#'  control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) %>%
#'  generate(n = 20)
#'
#' # Create two levels of arousal, controlling for orthographic similarity
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 6:9) %>%
#'  control_for_map(levenshtein.distance, string, 0:4) %>%
#'  generate(20)
#'
#' # Create two levels of arousal, controlling for phonological similarity
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 6:9) %>%
#'  control_for_map(levenshtein.distance, eSpeak.br_1letter, 0:2) %>%
#'  generate(20)
#'
#' # Create two levels of arousal, controlling for phonological similarity, and rhyme
#' library(vwr)
#' lexops %>%
#'  split_by(AROU.Warriner, 1:3 ~ 6:9) %>%
#'  control_for(Rhyme.eSpeak.br) %>%
#'  control_for_map(levenshtein.distance, eSpeak.br_1letter, 0:2) %>%
#'  generate(20)
#'
#' # A similar design to that above, but with 3 levels of valence, and inclusive matching
#' # Note that this will result in exactly the same result as above.
#' # A function that calculates something like Semantic Similarity will produce very different results.
#' library(vwr)
#' lexops %>%
#'  split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
#'  control_for(Rhyme.eSpeak.br) %>%
#'  control_for_map(levenshtein.distance, eSpeak.br_1letter, 0:2) %>%
#'  generate(20, match_null = "inclusive")
#'
#' @export

generate <- function(df, n=20, match_null = "balanced", seed = NA, silent = FALSE, is_shiny = FALSE) {
  if (is_shiny) {
    # if in a shiny context, replace the base cat() function with one which captures the console output
    cat <- function(str) shinyjs::html("gen_console", sprintf("%s", str))
  }

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # get options from attributes
  if (!is.null(LexOPS_attrs$options)) {
    id_col <- LexOPS_attrs$options$id_col
    cond_col <- LexOPS_attrs$options$cond_col
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
  } else {
    id_col <- "string"
    cond_col <- "LexOPS_splitCond"
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
  }

  # check for problems with arguments
  generate.check(df, n, match_null, seed, id_col, cond_col, is_shiny, LexOPS_attrs)

  # get the columns containing the split data
  LexOPS_splitCols <- colnames(df)[grepl(cond_col_regex, colnames(df))]

  df <- df %>%
    # create new column, which will give the cell of the design that each string belongs to
    tidyr::unite(!!dplyr::sym(cond_col), LexOPS_splitCols, sep = "_") %>%
    # remove strings that are members of no condition (i.e. if filter=FALSE in previous functions)
    dplyr::filter(!is.na(!!dplyr::sym(cond_col)))

  all_conds <- sort(unique(df[[cond_col]]))

  # check match_null is an expected value
  if (!match_null %in% c(all_conds, "inclusive", "balanced", "random", "first")) stop('Unknown match_null; expected "inclusive", "random", "balanced", "first", or a specific condition (e.g. "A2_B1_C1")')

  # set the seed if specified
  if (!is.na(seed)) set.seed(seed)

  # if no controls, just return the df with the condition variable, otherwise generate matches
  if (!is.null(LexOPS_attrs$controls) | !is.null(LexOPS_attrs$control_functions)) {

    # get the absolute smallest number of possible match rows (a count of the least frequent condition)
    min_nr_of_match_rows <- df %>%
      dplyr::group_by(!!dplyr::sym(cond_col)) %>%
      dplyr::count() %>%
      dplyr::pull(n) %>%
      min()
    # if n is "all", set to the largest possible number of match rows
    if (n == "all") {
      n <- min_nr_of_match_rows
      n_all <- TRUE
    } else {
      n_all <- FALSE
    }
    # give warning if min_nr_of_match_rows < n, as this means n is definitely not achievable
    if (min_nr_of_match_rows < n) {
      warning(sprintf("n is too large; requested n of %i, but the condition with the fewest members has %i entries. Ensure n < %i. Will generate as many stimuli as possible.", n, min_nr_of_match_rows, min_nr_of_match_rows))
    }

    # specify exactly how many null conditions are needed
    null_cond_n <- if (n < min_nr_of_match_rows) n else min_nr_of_match_rows

    # specify the null conditions
    null_conds <- if (match_null == "first") {
      rep(sort(all_conds)[1], null_cond_n)
    } else if (match_null == "random") {
      sample(all_conds, null_cond_n, replace = TRUE)
    } else if (match_null == "balanced") {
      if (n_all) warning('`match_null="balanced"` may not work when `n="all"`. Check distributions of match_null in the output.')
      # As close to equal number of each condition as possible, randomly ordered.
      # If doesn't perfectly divide, the condition(s) which are over-represented are also randomly chosen.
      all_conds %>%
        sample(length(all_conds)) %>%
        rep_len(null_cond_n) %>%
        sample(null_cond_n)
    } else if (match_null == "inclusive") {
      # same as balanced for starting match null
      all_conds %>%
        sample(length(all_conds)) %>%
        rep_len(null_cond_n) %>%
        sample(null_cond_n)
    } else if (match_null %in% all_conds) {
      rep(match_null, null_cond_n)
    }

    # iterate over all items in the null_condition, and for each, generate row of matched stimuli if possible
    n_tried <- 0
    n_tried_this_n_generated <- 0  # increases by 1 each iter but resets to 0 each time a row of matches is successfully generated
    n_generated <- 0
    words_tried_this_generated <- c()
    out <- matrix(ncol=length(all_conds)+1, nrow=n)
    printing_points <- round(seq(0, n, n/10))
    successful_iterations <- c()
    control_for_map_values <- dplyr::tibble()

    while(n_generated < n) {
      n_tried <- n_tried + 1
      n_tried_this_n_generated <- n_tried_this_n_generated + 1

      this_match_null <- null_conds[n_generated+1]
      null_word_bank <- df[[id_col]][!df[[id_col]] %in% out & df[[cond_col]] %in% this_match_null & !df[[id_col]] %in% words_tried_this_generated]

      if (length(null_word_bank) == 0) {
        if (n_all) {
          if (!silent) cat(sprintf("\nGenerated a total of %i stimuli per condition (%i total iterations)\n", n_generated, n_tried))
        } else {
          warning_text <- sprintf("\nFailed to generate any new matches for matched row %i (all %i candidate null words were tried)", n_generated + 1, n_tried_this_n_generated)
          if (is_shiny) {
            cat(warning_text)
          } else {
            warning(warning_text)
          }
        }
        break
      }

      this_word <- sample(null_word_bank, 1)
      words_tried_this_generated <- c(words_tried_this_generated, this_word)

      matches <- sapply(all_conds[all_conds != this_match_null], function(cnd) {
        m <- df[(!df[[id_col]] %in% out & df[[cond_col]] == cnd) | df[[id_col]]==this_word, ] %>%
          generate.find_matches(
            target = this_word,
            vars = LexOPS_attrs$controls,
            matchCond = this_match_null,
            id_col = id_col,
            cond_col = cond_col
          ) %>%
          generate.find_fun_matches(
            target = this_word,
            vars_pre_calc = LexOPS_attrs$control_functions,
            matchCond = this_match_null,
            id_col = id_col,
            cond_col = cond_col
          )
        # remove the target word
        m <- m[m[[id_col]]!=this_word, ]

        if (nrow(m)==0) {
          item_out <- NA
          } else {
            # pick a match randomly
            item_out <- m %>%
              dplyr::pull(id_col) %>%
              sample(1)
            # store any control_for_map values
            if (length(LexOPS_attrs$control_functions) > 0) {
              out_cont_map_val <- sapply(LexOPS_attrs$control_functions, function(cont) {
                # get the representations of the words in the given column
                this_word_rep <- df[[ cont[[3]] ]][df[[id_col]]==this_word]
                out_rep <- df[[ cont[[3]] ]][df[[id_col]]==item_out]
                # get the value from the function
                unname(cont[[2]](out_rep, this_word_rep))
              })
              names(out_cont_map_val) <- sapply(LexOPS_attrs$control_functions, function(cont) cont[[1]] )
              out_val <- item_out
              names(out_val) <- id_col
              control_for_map_values <<- dplyr::bind_rows(control_for_map_values, c(out_val, out_cont_map_val))

              if (!this_word %in% control_for_map_values[[id_col]]) {
                this_word_map_val <- sapply(LexOPS_attrs$control_functions, function(cont) {
                  # get the representation in the given column
                  this_word_rep <- df[[ cont[[3]] ]][df[[id_col]]==this_word]
                  # get the value from the function
                  unname(cont[[2]](this_word, this_word))
                })
                names(this_word_map_val) <- sapply(LexOPS_attrs$control_functions, function(cont) cont[[1]] )
                this_word_val <- this_word
                names(this_word_val) <- id_col
                control_for_map_values <<- dplyr::bind_rows(control_for_map_values, c(this_word_val, this_word_map_val))
              }

            }
          }

        item_out
      })
      # add the target word
      matches[this_match_null] <- this_word
      # ensure ordered correctly (e.g. A1, A2, A3)
      matches <- matches[order(factor(names(matches)))]

      # check the matches are inclusive if match_null = "inclusive"
      if (match_null == "inclusive") {
        matches <- generate.are_matches_inclusive(
          df = df[!df[[id_col]] %in% out | df[[id_col]]==this_word, ],
          matches,
          vars=LexOPS_attrs$controls, vars_pre_calc = LexOPS_attrs$control_functions,
          matchCond=this_match_null, id_col, cond_col
        )
      }

      # if n=="all", print progress every 250 iterations
      if (!silent) {
        if (n_tried%%250==0 & n_all) {
          if (all(!is.na(matches))) {
            cat(sprintf("Generated %i (%i iterations, %.2f success rate)\n", n_generated+1, n_tried, (n_generated+1)/n_tried))
          } else {
            cat(sprintf("Generated %i (%i iterations, %.2f success rate)\n", n_generated, n_tried, n_generated/n_tried))
          }
        }
      }

      if (all(!is.na(matches))) {
        this_match_null_out <- if (match_null=="inclusive") NA else this_match_null  # if match_null is inclusive, don't store initial match_nullvalue
        out[n_generated + 1, ] <- c(matches, this_match_null_out)
        n_generated <- n_generated + 1
        successful_iterations <- c(successful_iterations, n_tried)
        n_tried_this_n_generated <- 0
        words_tried_this_generated <- c()
        if (!silent) {
          if (n_generated %in% printing_points & !n_all) {
            cat(sprintf("Generated %i/%i (%i%%). %i total iterations, %.2f success rate.\n", n_generated, n, round(n_generated/n*100), n_tried, n_generated/n_tried))
          }
        }
      }

    }
    cat(sprintf("\n"))

    # create meta_df
    meta_df <- df

    # add control_for_map() values if any
    if (length(LexOPS_attrs$control_functions) > 0) {
      meta_df <- dplyr::left_join(meta_df, control_for_map_values, by=id_col)
    }

    df <- as.data.frame(out, stringsAsFactors = FALSE)
    colnames(df) <- c(all_conds, "match_null")
    df <- df %>%
      tidyr::drop_na(-match_null) %>%  # na.omit() but allowing NAs in match_null column
      dplyr::mutate(item_nr = dplyr::row_number()) %>%  # add the item number as item_nr
      dplyr::select(item_nr, dplyr::everything())
    # add the original df to the attributes
    LexOPS_attrs$meta_df <- meta_df
    # add the success rate to the attributes
    LexOPS_attrs$success_rate <- n_generated/n_tried
    # add a vector of the iteratons that were successful
    LexOPS_attrs$successful_iterations <- successful_iterations

  }

  # add a marker to the attributes that df has gone through the generate function
  LexOPS_attrs$generated <- TRUE
  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  df
}

# function to check supplied arguments makes sense
generate.check <- function(df, n, match_null, seed, id_col, cond_col, is_shiny, LexOPS_attrs) {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check id_col is a string
  if (!is.character(id_col)) stop(sprintf("Expected id_col to be of class string, not %s", class(id_col)))

  # check n is a number or expected string
  if (!is.numeric(n) & n != "all") stop(sprintf('n must be numeric or a string of value "all"'))

  # check that the conditions are present in the attributes
  if (is.null(cond_col)) {
    # if the column containing the condition info is missing and not defined manually, throw error
    stop("Could not identify split conditions column! Make sure you run split_by() before generate().")
  }
  # if control_for() has been run (can tell from attributes), check the specified columns exist
  if (!is.null(LexOPS_attrs$controls)) {
    controls_exist <- sapply(LexOPS_attrs$controls, function(cont) {cont[[1]] %in% colnames(df)})
    if (!(all(controls_exist))) stop(sprintf("%i unknown control columns. Check columns specified in control_for() are in df.", length(controls_exist[!controls_exist])))
  }
  # if control_for_fun() has been run, check the column to be fed to the function exists in df
  if (!is.null(LexOPS_attrs$control_functions)) {
    controls_exist <- sapply(LexOPS_attrs$control_functions, function(cont) {cont[[3]] %in% colnames(df)})
    if (!(all(controls_exist))) stop(sprintf("%i unknown control columns. Check columns specified in control_for() are in df.", length(controls_exist[!controls_exist])))
  }
}

# function to filter exactly for categories, and with tolerances for numeric
generate.filter_tol <- function(tol, df_matches) {
  if(is.numeric(df_matches[[tol[[1]]]]) & length(tol)>=3) {
    dplyr::filter(df_matches, dplyr::between(!!dplyr::sym(tol[[1]]),
                                             tol[[3]]+tol[[2]][1],
                                             tol[[3]]+tol[[2]][2]))
  } else {
    dplyr::filter(df_matches, !!dplyr::sym(tol[[1]]) == tol[[2]])
  }
}

# function to find matches for a particular word (better than current match_word() function?)
generate.find_matches <- function(df, target, vars, matchCond, id_col, cond_col) {
  # if no controls, return the df unchanged
  if (length(vars)==0) return(df)
  # get a copy of df excluding the null condition, but keep the target word
  df_matches <- df[df[[cond_col]] != matchCond | df[[id_col]] == target, ]
  # add a 2nd (for categorical) or 3rd (for numeric) item to each control's list, indicating the value for the string being matched to
  vars <- lapply(vars, function(cont) {
    cont_val <- if (is.factor(df[[cont[[1]]]])) {
      as.character(df[[cont[[1]]]][df[[id_col]]==target])
    } else {
      df[[cont[[1]]]][df[[id_col]]==target]
    }
    if (is.list(cont)) append(cont, cont_val) else list(cont, cont_val)
  })
  # for each control, filter out non-suitable matches for this word
  df_matches_filt <- vars %>%
    purrr::map(~ generate.filter_tol(., df_matches = df_matches)) %>%
    purrr::reduce(dplyr::inner_join, by = colnames(df_matches))
  df_matches_filt
}

# function to check whether the matches are inclusive (neceessary if match_null = "inclusive")
# This treats each possible condition for the current item as the match null for one iteration, and tests that all other words are suitable matches
# if TRUE, will return the matches unchanged, else will return same vector with all values replaced by NAs
generate.are_matches_inclusive <- function(df, matches, vars, vars_pre_calc, matchCond, id_col, cond_col) {
  # if no controls, return the df unchanged
  if (length(vars)==0 | any(is.na(matches))) return(df)
  # check words are matched inclusively
  are_inclusive <- lapply(matches, function(this_word) {
    # get this word's condition
    matchCond_this_word <- names(matches)[matches==this_word]
    # get list of suitable matches based on controls
    df_matches_this_word <- generate.find_matches(df, this_word, vars, matchCond_this_word, id_col, cond_col)
    # get a similar list, but mapping any specified functions
    df_matches_this_word_funs <- generate.find_fun_matches(df, this_word, vars_pre_calc, matchCond_this_word, id_col, cond_col)
    # check the other items are in there, and return this value
    all(matches[matches != this_word] %in% df_matches_this_word[[id_col]]) & all(matches[matches != this_word] %in% df_matches_this_word_funs[[id_col]])
  })
  # leave unchanged if inclusive, otherwise return NAs
  if (all(unlist(are_inclusive))) {
    matches
  } else {
    rep(NA, length(matches)) %>%
      magrittr::set_names(names(matches))
  }
}

# function to find matches for a particular word using functions defined by `control_for_fun()`
generate.find_fun_matches <- function(df, target, vars_pre_calc, matchCond, id_col, cond_col) {
  # if no control functions, return the df unchanged
  if (length(vars_pre_calc)==0) return(df)

  # if df has a 0 rows (e.g. if generate.find_matches() found no matches), return df unchanged
  if (nrow(df)==0) return(df)

  # get the new columns' values for the target word
  target_vals <- sapply(vars_pre_calc, function(x) {
    fun <- x[[2]]
    var <- x[[3]]
    tol <- x[[4]]
    target_input <- df[[var]][df[[id_col]]==target]
    fun(target_input, target_input)
  })

  # get a copy of df excluding the null condition and the target word
  df_matches <- df[df[[cond_col]] != matchCond | df[[id_col]] == target, ]

  # calculate the new columns based on the supplied functions and arguments
  # (each item of var should have a structure of `list(name, fun, var, tol)`)
  func_col_names <- sapply(vars_pre_calc, function(x) x[[1]])  # get the names (set after `purrr::map()`)
  df_matches <- vars_pre_calc %>%
    purrr::map( ~ .x[[2]](df_matches[[ .x[[3]] ]], df[[ .x[[3]] ]][df[[id_col]]==target] ) ) %>%
    purrr::set_names(func_col_names) %>%
    dplyr::bind_cols(df_matches, .)

  # create vars list which will be used for matching purposes
  vars <- lapply(1:length(vars_pre_calc), function(cont_nr) {
    cont <- vars_pre_calc[[cont_nr]]
    col_name <- func_col_names[[cont_nr]]
    if (length(cont) >= 4) {
      list(col_name, cont[[4]])
    } else {
      list(col_name)
    }
  })

  # add a 2nd (for categorical) or 3rd (for numeric) item to each control's list, indicating the value for the string being matched to
  vars <- lapply(1:length(vars), function(cont_nr) {
    cont <- vars[[cont_nr]]
    fun <- vars_pre_calc[[cont_nr]][[2]]
    var <- vars_pre_calc[[cont_nr]][[3]]
    cont_val <- target_vals[[cont_nr]]
    if (is.list(cont)) append(cont, cont_val) else list(cont, cont_val)
  })

  # for each control, filter out non-suitable matches for this word
  df_matches_filt <- vars %>%
    purrr::map(~ generate.filter_tol(., df_matches = df_matches)) %>%
    purrr::reduce(dplyr::inner_join, by = colnames(df_matches))

  df_matches_filt
}
