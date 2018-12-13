# These functions serve to match by selected variables, and calculate the distances and differences from that associated with the target string

matcher <- function(df, rawdf, column="Length", sl, checkbox, str_in, manual_str_in_x=NA, colmeans_name=NA, pron_nr=NA, scale.cols=F, do.filter=T) {
  if (checkbox & !(all(is.null(column)) | all(is.na(column)))) {
    
    if (scale.cols) {
      rawdf[column] <- lapply(rawdf[column], scale)
    }
    
    if (length(column)>1) {
      # create new column, which will be the average of the variables selected
      rawdf[[colmeans_name]] <- rowMeans(select(rawdf, one_of(column)), dims=1, na.rm=T)
      column <- colmeans_name
    }
    
    # add column to the df being built
    df <- inner_join(df, select(rawdf, "string", column), by="string")
    
    if (is.na(pron_nr)) {
      column_str_in_x <- column
    } else {
      column_str_in_x <- sprintf("%s%i", substr(column, 0, nchar(column)-1), pron_nr)
    }
    # get column's value for this string
    if (is.na(manual_str_in_x)) {
      str_in_x <- rawdf[[column_str_in_x]][rawdf$string==str_in]
    } else {
      str_in_x <- manual_str_in_x
    }
    if (is.numeric(rawdf[[column]])) {
      # match numeric
      limits <- sl + str_in_x
      rawdf <- filter(rawdf, between(!!sym(column), limits[1], limits[2]) | string==str_in)
    } else {
      # match categorical
      rawdf <- filter(rawdf, !!sym(column) == str_in_x | string==str_in)
    }
    
    if (do.filter) {
      df <- df %>% filter(string %in% rawdf$string)
    }
    
    df
    
  } else {
    
    df
    
  }
}

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

