#' Generate stimuli
#'
#' Generates the stimuli from the data frame after it has been passed through `split_by()`, and optionally, `control_for()`. Will generate `n` items per condition. If <`n` items can be generated, will generate as many as possible given the experiment's design. Can be reproducible with `set.seed()`.
#'
#' @param df A data frame that is the result from `control_for()` or `split_by()`.
#' @param n The number of strings per condition (default = 20). Set to `"all"` to generate as many as possible.
#' @param match_null The condition words should be matched to. Should be a string indicating condition (e.g. `"A1_B2_C1"`), or a string indicating one of the following options: "first" for the lowest condition (e.g. `"A1"` or `"A1_B1_C1_D1"`, etc.), "random" for randomly selected null condition each iteration, "balanced" for randomly ordered null conditions with (as close to as possible) equal number of selections for each condition.
#' @param string_col The column containing the strings (default = `"string"`).
#' @param cond_col Prefix with which the columns detailing the splits were labelled by `split_by()`. This is rarely needed (default = `NA`), as by default the function gets this information from `df`'s attributes.
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
#' lexops %>%
#'   dplyr::filter(PK.Brysbaert >= .75) %>%
#'   split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   control_for(Length) %>%
#'   generate(n = 1000, match_null = "balanced")
#'
#' # Generate stimuli for a concreteness x valence (2 x 3) design
#' # (Note that abstract, neutral is set as the matching null)
#' # (Also note that the data is filtered by proportion known to be >75%)
#' lexops %>%
#'   split_by(CNC.Brysbaert, 1:2 ~ 4:5) %>%
#'   split_by(VAL.Warriner, 1:3 ~ 4.5:5.5 ~ 7:9) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.25:0.25) %>%
#'   control_for(Length) %>%
#'   generate(n = 30, match_null = "A2_B2")
#'
#' # Bypass non-standard evaluation
#' lexops %>%
#'  split_by("Syllables.CMU", list(c(1, 3), c(4, 6), c(7, 20)), standard_eval = TRUE) %>%
#'  control_for("Zipf.SUBTLEX_UK", c(-0.2, 0.2), standard_eval = TRUE) %>%
#'  generate(n = 20)
#'
#' @export

generate <- function(df, n=20, match_null = "first", string_col = "string", cond_col = NA) {
  # check the df is a dataframe
  if (!is.data.frame(df)) stop(sprintf("Expected df to be of class data frame, not %s", class(df)))
  # check string_col is a string
  if (!is.character(string_col)) stop(sprintf("Expected string_col to be of class string, not %s", class(string_col)))

  # check n is a number or expected string
  if (!is.numeric(n) & n != "all") stop(sprintf('n must be numeric or a string of value "all"'))

  # get attributes
  LexOPS_attrs <- if (is.null(attr(df, "LexOPS_attrs"))) list() else attr(df, "LexOPS_attrs")

  # check that the conditions are present in the attributes
  if (is.null(LexOPS_attrs$splitCol) & is.na(cond_col)) {
    # if the column containing the condition info is missing and not defined manually, throw error
    stop("Could not identify split conditions column! Make sure you run split_by() before generate().")
  } else if (!is.na(cond_col)) {
    # if the column containing condition info is defined manually, check exists
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
    if (length(colnames(df)[grepl(cond_col_regex, colnames(df))]) == 0) stop(sprintf("No columns found for the manually defined cond_col '%s'.", cond_col))
  }
  # if control_for() has been run (can tell from attributes), check the specified columns exist
  if (!is.null(LexOPS_attrs$controls)) {
    controls_exist <- sapply(LexOPS_attrs$controls, function(cont) {cont[[1]] %in% colnames(df)})
    if (!(all(controls_exist))) stop(sprintf("%i unknown control columns. Check columns specified in control_for() are in df.", length(controls_exist[!controls_exist])))
  }

  # get cond_col from the attributes if not manually defined
  if (is.na(cond_col)) {
    cond_col <- LexOPS_attrs$splitCol
    cond_col_regex <- sprintf("^%s_[A-Z]$", cond_col)
  }

  # get the columns containing the split data
  LexOPS_splitCols <- colnames(df)[grepl(cond_col_regex, colnames(df))]

  df <- df %>%
    # create new column, which will give the cell of the design that each string belongs to
    tidyr::unite(LexOPS_cond, LexOPS_splitCols, sep = "_") %>%
    # remove strings that are members of no condition (i.e. if filter=FALSE in previous functions)
    dplyr::filter(!is.na(LexOPS_cond))
  all_conds <- sort(unique(df$LexOPS_cond))

  # check match_null is an expected value
  if (!match_null %in% c(all_conds, "balanced", "random", "first")) stop('Unknown match_null; expected "random", "balanced", "first", or a specific condition (e.g. "A2_B1_C1")')

  # if no controls, just return the df with the condition variable, otherwise generate matches
  if (!is.null(LexOPS_attrs$controls)) {

    # function to find matches for a particular word (better than current match_word() function?)
    find_matches <- function(df, target, vars, matchCond, string_col) {
      # get a copy of df excluding the null condition and the target word
      df_matches <- df[df$LexOPS_cond != matchCond & df[[string_col]] != target, ]
      # add a 2nd (for non-numeric) or 3rd (for numeric) item to each control's list, indicating the value for the string being matched to
      vars <- lapply(vars, function(cont) {
        cont_val <- df[[cont[[1]]]][df[[string_col]]==target]
        #if (is.factor(cont_val)) cont_val <- as.character(cont_val)
        if (is.list(cont)) append(cont, cont_val) else list(cont, cont_val)
      })
      # function to filter exactly for non-numeric, and with tolerances for numeric
      filter_tol <- function(tol, df_matches) {
        if(is.numeric(df_matches[[tol[[1]]]]) & length(tol)>=3) {
          dplyr::filter(df_matches, dplyr::between(!!dplyr::sym(tol[[1]]),
                                           tol[[3]]+tol[[2]][1],
                                           tol[[3]]+tol[[2]][2]))
        } else {
          dplyr::filter(df_matches, !!dplyr::sym(tol[[1]]) == tol[[2]])
        }
      }
      # for each control, filter out non-suitable matches for this word
      df_matches_filt <- vars %>%
        purrr::map(~ filter_tol(., df_matches = df_matches)) %>%
        purrr::reduce(dplyr::inner_join, by = colnames(df_matches))
      df_matches_filt
    }

    # get the absolute smallest number of possible match rows (a count of the least frequent condition)
    min_nr_of_match_rows <- df %>%
      dplyr::group_by(LexOPS_cond) %>%
      dplyr::count() %>%
      dplyr::arrange(n) %>%
      dplyr::pull(n) %>%
      dplyr::first()
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
    while(n_generated < n) {
      n_tried <- n_tried + 1
      n_tried_this_n_generated <- n_tried_this_n_generated + 1

      this_match_null <- null_conds[n_generated+1]
      null_word_bank <- df[[string_col]][!df[[string_col]] %in% out & df$LexOPS_cond == this_match_null & !df[[string_col]] %in% words_tried_this_generated]

      if (length(null_word_bank) == 0) {
        if (n_all) {
          cat(sprintf("\nGenerated a total of %i stimuli per condition (%i total iterations)\n", n_generated, n_tried))
        } else {
          warning(sprintf("\nFailed to generate any new matches for matched row %i null condition %s (all %i candidate null words were tried)", n_generated + 1, this_match_null, n_tried_this_n_generated))
        }
        break
      }

      this_word <- sample(null_word_bank, 1)
      words_tried_this_generated <- c(words_tried_this_generated, this_word)

      matches <- sapply(all_conds[all_conds != this_match_null], function(c) {
        m <- find_matches(
          df = df[(!df[[string_col]] %in% out & df$LexOPS_cond == c) | df[[string_col]]==this_word, ],
          target = this_word,
          vars = LexOPS_attrs$controls,
          matchCond = this_match_null,
          string_col = string_col
          )
        if (nrow(m)==0) NA else{
          m %>%
            dplyr::pull(string_col) %>%
            sample(1)
        }
      })
      # add the target word
      matches[this_match_null] <- this_word
      # ensure ordered correctly (e.g. A1, A2, A3)
      matches <- matches[order(factor(names(matches)))]

      # if n=="all", print progress every 250 iterations
      if (n_tried%%250==0 & n_all) {
        if (all(!is.na(matches))) {
          cat(sprintf("Generated %i (%i iterations, %.2f success rate)\n", n_generated+1, n_tried, (n_generated+1)/n_tried))
        } else {
          cat(sprintf("Generated %i (%i iterations, %.2f success rate)\n", n_generated, n_tried, n_generated/n_tried))
        }
      }

      if (all(!is.na(matches))) {
        out[n_generated + 1, ] <- c(matches, this_match_null)
        n_generated <- n_generated + 1
        n_tried_this_n_generated <- 0
        words_tried_this_generated <- c()
        if (n_generated %in% printing_points & !n_all) {
          cat(sprintf("Generated %i/%i (%i%%). %i total iterations, %.2f success rate.\n", n_generated, n, round(n_generated/n*100), n_tried, n_generated/n_tried))
        }
      }

    }
    sprintf("\n")

    meta_df <- df[df[[string_col]] %in% out, ]
    df <- as.data.frame(out, stringsAsFactors = FALSE) %>%
      stats::na.omit()
    colnames(df) <- c(all_conds, "match_null")
    # add the item number as item_nr
    df <- dplyr::mutate(df, item_nr = dplyr::row_number()) %>%
      dplyr::select(item_nr, dplyr::everything())
    # add the original df to the attributes
    LexOPS_attrs$meta_df <- meta_df

  }

  # add a marker to the attributes that df has gone through the generate function
  LexOPS_attrs$generated <- TRUE
  # add the attributes to the output object
  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  df
}

# # should throw errors
#
# lexops %>%
#   control_for(list(Zipf.SUBTLEX_UK, c(-0.2, 0.2))
#
# lexops %>%
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) %>%
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2), cond_col = "splitID")
#
# # should not be enough data (all words of 7-20 syllables are nouns), so should only generate as many conditions as possible
# lexops %>%
#    split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) %>%
#    split_by(PoS.SUBTLEX_UK, "noun" ~ "verb")) %>%
#    control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2)) %>%
#    generate()

# should work
# lexops %>%
#   dplyr::filter(PK.Brysbaert >= .75) %>%
#   split_by(list("Length", c(1, 3), c(4, 6), c(7, 20))) %>%
#   control_for(list("Zipf.SUBTLEX_UK", c(-0.2, 0.2))) %>%
#   control_for("PoS.SUBTLEX_UK") %>%
#   generate(n = 50)
#
# lexops %>%
#   dplyr::filter(PK.Brysbaert >= .75) %>%
#   split_by(BG.SUBTLEX_UK, c(0.001, 0.003) ~ c(0.009, 0.011)) %>%
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2)) %>%
#   control_for(Length) %>%
#   generate(n = 1000, match_null = "balanced")
#
# lexops %>%
#   split_by(Syllables.CMU, c(1, 3) ~ c(4, 6) ~ c(7, 20)) %>%
#   control_for(Zipf.SUBTLEX_UK, c(-0.2, 0.2)) %>%
#   generate(n = 20)
#
# lexops %>%
#   split_by(CNC.Brysbaert, c(1, 2) ~ c(4, 5)) %>%
#   split_by(VAL.Warriner, c(1, 3) ~ c(4.5, 5.5) ~ c(7, 9)) %>%
#   control_for(Zipf.SUBTLEX_UK, c(-0.25, 0.25)) %>%
#   control_for(Length) %>%
#   generate(30, "A2_B2")
