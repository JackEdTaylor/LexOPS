# These functions serve to match by selected variables, and calculate the distances and differences from that associated with the target string

matcher <- function(df, rawdf, column="Length", sl, checkbox, str_in, manual_str_in_x=NA, colmeans_name=NA, pron_nr=NA, scale.cols=F, do.filter=T) {
  if (checkbox & !(is.null(column)|is.na(column))) {
    
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

corpus_recode <- function(inputopts = c("bnc_w", "bnc_s"), prefix=NA) {
  if (!is.null(inputopts)) {
    prefix_dot <- if(is.na(prefix)) {""} else {sprintf("%s.", prefix)}
    recoded <- recode(inputopts,
                      "bnc_w" = "BNC.Written",
                      "bnc.wbg" = "BNC.Written",
                      "bnc_s" = "BNC.Spoken",
                      "bnc.sbg" = "BNC.Spoken",
                      "suk" = "SUBTLEX_UK",
                      "subtlex_uk.bg" = "SUBTLEX_UK",
                      "sus" = "SUBTLEX_US",
                      "subtlex_us.bg" = "SUBTLEX_US",
                      "elp" = "ELP",
                      "blp" = "BLP",
                      "mp" = "Moby",
                      "cmu" = "CMU.pr1",
                      "gn" = "Glasgow_Norms",
                      "cp" = "Clark_and_Paivio",
                      "kuperman" = "Kuperman",
                      "bb" = "BrysbaertBiemiller",
                      "brysbaert" = "Brysbaert",
                      "warriner" = "Warriner",
                      "eh" = "EngelthalerHills"
    )
    sprintf("%s%s", prefix_dot, recoded)
  } else {
    NA
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

