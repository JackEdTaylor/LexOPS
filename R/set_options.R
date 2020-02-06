#' Set options for generating stimuli.
#'
#' When the first function called in a generate pipeline, allows the user to set options that will be passed to all subsequent functions in the pipeline.
#'
#' @param df The dataframe that will be used in the generate pipeline.
#' @param id_col A character vector specifying the column identifying unique observations (e.g. in `LexOPS::lexops`, the `id_col` is `"string"`).
#'
#' @return Returns `df`, with the options stored in the attributes.
#'
#' @export

set_options <- function(df, id_col = "string"){

  # check no LexOPS attributes present
  if (!is.null(attr(df, "LexOPS_attrs"))) {
    stop("`set_options()` must be the first function run in a generate pipeline")
  }

  # check id_col is a character vector
  if (!is.character(id_col)) {
    stop("`id_col` must be a character vector")
  }

  # add the attributes
  LexOPS_attrs <- list()

  LexOPS_attrs$options <- list(
    id_col = id_col
  )

  attr(df, "LexOPS_attrs") <- LexOPS_attrs

  # return df
  df

}
