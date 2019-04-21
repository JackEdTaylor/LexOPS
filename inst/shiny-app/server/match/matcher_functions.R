get_differences <- function(df=matched, str_in) {
  get_diff <- function(df1=df, str_in1=str_in, column="Length") {
    if (is.numeric(df1[[column]])) {
      str_in_x <- df1[[column]][df1$string==str_in1]
      df1[[column]] - str_in_x
    } else {
      df1[[column]]
    }
  }
  out <- df
  for (cname in names(out)) {
    out[[cname]] <- get_diff(column=cname)
  }
  out
}

get_distances <- function(df=matched_differences) {
  # for speed, this function is run on matched_differences
  get_dist <- function(coldata) {
    if (is.numeric(coldata)) {
      abs(coldata)
    } else {
      coldata
    }
  }
  out <- df
  for (cname in names(out)) {
    out[[cname]] <- get_dist(out[[cname]])
  }
  out
}

