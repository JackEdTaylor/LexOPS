#' Set options for generating stimuli.
#'
#' When the first function called in a generate pipeline, allows the user to set options that will be passed to all subsequent functions in the pipeline.
#'
#' @param df The dataframe that will be used in the generate pipeline.
#' @param id_col A character vector specifying the column identifying unique observations (e.g. in `LexOPS::lexops`, the `id_col` is `"string"`).
#' @param cond_col Prefix with which to name the columns where conditions will be stored (default = "LexOPS_splitCond"). Each time `split_by()` is run on a dataframe, a new cond_col is added to the data frame, e.g., the first time will add splitCond_A, the second time will add split_cond_B, etc. The default is usually sufficient.
#'
#' @return Returns `df`, with the options stored in the attributes.
#'
#' @examples
#'
#' # give a df with "word" as the identifying column
#' lexops %>%
#'   dplyr::rename(word = string) %>%
#'   # tell LexOPS "word" is the identifying column
#'   set_options(id_col = "word") %>%
#'   split_by(Syllables.CMU, 1:3 ~ 4:6 ~ 7:20) %>%
#'   control_for(Zipf.SUBTLEX_UK, -0.2:0.2) %>%
#'   generate(n = 20)
#'
#' @export

set_options <- function(df, id_col = "string", cond_col = "LexOPS_splitCond"){

  # check no LexOPS attributes present
  if (!is.null(attr(df, "LexOPS_attrs"))) {
    stop("`set_options()` must be the first function run in a generate pipeline")
  }

  # check id_col is a character vector
  if (!is.character(id_col)) {
    stop("`id_col` must be a character vector")
  }

  # check cond_col is a character vector
  if (!is.character(cond_col)) {
    stop("`cond_col` must be a character vector")
  }

  # add the attributes
  LexOPS_attrs <- list()

  LexOPS_attrs$options <- list(
    id_col = id_col,
    cond_col = cond_col
  )

  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  # return df
  df

}
