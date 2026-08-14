context("cite_functions")

# cite_design ----
testthat::test_that("cite_design", {
  # if run on a non-lexops dataframe, should all be custom measures
  testthat::expect_true({
    carstim <- mtcars |>
      tibble::as_tibble(rownames = "car_id") |>
      set_options(id_col = "car_id") |>
      split_by(am, 0:0 ~ 1:1) |>
      control_for(qsec, -5:5) |>
      control_for(carb, 0:0) |>
      generate(5, silent=TRUE)

    cite_df <- cite_design(carstim)

    all(cite_df$measure=="Custom Measure") &
      all(cite_df$source=="Custom Source") &
      all(is.na(cite_df$url))
  })
  # if run on an object that is not the result of a LexOPS pipeline, return an informative error
  testthat::expect_error(
      cite_design(c("not", "even", "a", "dataframe!")),
      regexp = "Must run `generate\\(\\)` on `df` before using `cite_design\\(\\)`"
  )

  # only run the remaining tests if the lexopsdata package is installed
  testthat::skip_if_not_installed("lexopsdata")
  # cite_design returns a dataframe
  testthat::expect_true({
    stim <- lexops |>
      dplyr::filter(PK.Brysbaert >= .75) |>
      split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
      split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
      control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
      control_for(Length, 0:0) |>
      generate(n = 5, match_null = "balanced", silent=TRUE)

    is.data.frame( cite_design(stim) )
  })
  # relevant variables are present and in expected order
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
        # (also checks that random split is missing)
        split_random(nlevels = 2, equal_size = TRUE) |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        control_for(Length, 0:0) |>
        generate(n = 1, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim)

      cite_df$var
    },
    c("BG.SUBTLEX_UK", "CNC.Brysbaert", "Zipf.SUBTLEX_UK", "Length")
  )
  # requesting design variables gives both splits and controls, in expected order
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
        control_for(Length, 0:0) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        generate(n = 2, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim, include="design")

      cite_df$var
    },
    c("CNC.Brysbaert", "BG.SUBTLEX_UK", "Length", "Zipf.SUBTLEX_UK")
  )
  # requesting split variables gives only splits, in expected order
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
        control_for(Length, 0:0) |>
        generate(n = 2, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim, include="splits")

      cite_df$var
    },
    c("CNC.Brysbaert", "BG.SUBTLEX_UK")
  )
  # requesting split variables gives only splits, in expected order
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
        control_for(Length, 0:0) |>
        generate(n = 2, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim, include="controls")

      cite_df$var
    },
    c("Zipf.SUBTLEX_UK", "Length")
  )
  # requesting specific variables gives only those variables, regardless of whether present
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        split_by(BG.SUBTLEX_UK, 0.001:0.003 ~ 0.009:0.011) |>
        control_for(Length, 0:0) |>
        generate(n = 2, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim, include=c("BG.SUBTLEX_UK", "AROU.Warriner"))

      cite_df$var
    },
    c("BG.SUBTLEX_UK", "AROU.Warriner")
  )
  # permits custom id_cols
  testthat::expect_equal(
    {
      stim <- lexops |>
        dplyr::rename(lemma = string) |>
        dplyr::filter(PK.Brysbaert >= .75) |>
        set_options(id_col = "lemma") |>
        split_by(CNC.Brysbaert, 1:2 ~ 4:5) |>
        control_for(Zipf.SUBTLEX_UK, -0.2:0.2) |>
        generate(n = 2, match_null = "balanced", silent = TRUE)

      cite_df <- cite_design(stim)

      cite_df$var
    },
    c("CNC.Brysbaert", "Zipf.SUBTLEX_UK")
  )
})

# citation_table ----
testthat::test_that("citation_table", {
  # variables with DOI sources return patterns fitting DOIs
  testthat::expect_true({
    doi_vars <- c(
      "Zipf.SUBTLEX_UK", "Zipf.SUBTLEX_US", "AROU.Glasgow_Norms",
      "IMAG.Clark_and_Paivio", "AoA.Kuperman", "AoA.BrysbaertBiemiller",
      "CNC.Brysbaert", "AROU.Warriner", "HUM.EngelthalerHills",
      "PREV.Brysbaert", "RT.ELP", "RT.BLP"
    )

    cite_df <- citation_table(doi_vars)

    all( grepl("^https\\://doi.org/10\\.\\d{4,}/[-._;()/:A-Za-z0-9]+$", cite_df$url) )
  })
})

# var_to_measure ----
testthat::test_that("var_to_measure", {
  # standard and non-standard evaluation produce same results
  testthat::expect_equal(
    var_to_measure("Zipf.SUBTLEX_UK", standard_eval=TRUE),
    var_to_measure(Zipf.SUBTLEX_UK, standard_eval=FALSE)
  )
  # for the "first citation", give full name for variables that are later abbreviated
  testthat::expect_equal(
    unlist( lapply(c("Zipf.SUBTLEX_UK", "fpmw.SUBTLEX_UK", "ON.OLD20", "PN.PLD20.eSpeak.br", "PN.Colthearts_N.CMU", "PN.Log_Colthearts_N.eSpeak.br", "PN.Log_Colthearts_N.CMU", "RT.BLP", "Accuracy.ELP"), var_to_measure, standard_eval=TRUE, first_cite=TRUE) ),
    c("frequency in Zipf (Zipf=log10(frequency per million)+3)",
      "frequency per million words", "orthographic levenshtein distance 20 (OLD20)",
      "phonological levenshtein distance 20 (pld20)", "phonological Coltheart's N",
      "log phonological Coltheart's N", "log phonological Coltheart's N",
      "lexical decision response time (RT)", "lexical decision accuracy"
    )
  )
  # for the "non-first citation", give abbreviated names
  testthat::expect_equal(
    unlist( lapply(c("Zipf.SUBTLEX_UK", "fpmw.SUBTLEX_UK", "ON.OLD20", "PN.PLD20.eSpeak.br", "PN.Colthearts_N.CMU", "PN.Log_Colthearts_N.eSpeak.br", "PN.Log_Colthearts_N.CMU", "RT.BLP", "Accuracy.ELP"), var_to_measure, standard_eval=TRUE, first_cite=FALSE) ),
    c("frequency in Zipf", "frequency per million words", "OLD20",
      "pld20", "phonological Coltheart's N", "log phonological Coltheart's N",
      "log phonological Coltheart's N", "lexical decision RT", "lexical decision accuracy"
    )
  )
  # title capitalisation
  testthat::expect_equal(
    var_to_measure("eSpeak.br_IPA", standard_eval=TRUE, title_caps = TRUE),
    "International Phonetic Alphabet (IPA) Representations of Words' British Pronunciations"
  )
})

# var_to_source ----
testthat::test_that("var_to_source", {
  # standard and non-standard evaluation produce same results
  testthat::expect_equal(
    var_to_source("Zipf.SUBTLEX_UK", standard_eval=TRUE),
    var_to_source(Zipf.SUBTLEX_UK, standard_eval=FALSE)
  )
  # for the "non-first" citation, get shorter (abbreviated) output
  testthat::expect_lt(
    nchar(var_to_source("fpmw.BNC.All", standard_eval=TRUE, first_cite=FALSE)),
    nchar(var_to_source("fpmw.BNC.All", standard_eval=TRUE, first_cite=TRUE))
  )
})

# var_to_url ----
testthat::test_that("var_to_source", {
  # standard and non-standard evaluation produce same results
  testthat::expect_equal(
    var_to_url("Zipf.SUBTLEX_UK", standard_eval=TRUE),
    var_to_url(Zipf.SUBTLEX_UK, standard_eval=FALSE)
  )
})
