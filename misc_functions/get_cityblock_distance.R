# Get a vector of the City-Block distance of each word from the target word.

# Calculated using all numeric columns in the "columns" vector.
# If "columns"==NA, all numeric columns in df will be used.

# "weights" can be a named vector with the desired weights for calculating ED.
# The default (if variable not named, or "weights" is NA, is 1)

get_cityblock_distance <- function(df, str_in, columns=NA, weights=NA) {
  rawdf <- df
  if (all(is.na(columns)) | length(columns)==0) {
    res <- rep(NA, nrow(df))
  } else {
    df <- df[, c(columns)]
    
    df[columns] <- lapply(df[columns], scale)
    get_dist <- function(df1=df, str_in1=str_in, column="Length", rawdf1=rawdf) {
      if (is.numeric(df1[[column]])) {
        str_in_x <- df1[[column]][rawdf1$string==str_in1]
        abs(df1[[column]] - str_in_x)
      } else {
        df1[[column]]
      }
    }
    dist <- df
    if ("CityBlock.Distance" %in% names(dist)) {
      dist <- select(dist, -CityBlock.Distance)
    }
    for (cname in names(dist)) {
      dist[[cname]] <- get_dist(column=cname)
      if (cname %in% names(weights)) {
        dist[[cname]] <- dist[[cname]] * weights[[cname]]
      }
    }
    
    res <- sqrt(rowSums(dist[, c(columns)], na.rm=T))
  }
  
  res
}

#lexops$CityBlock.Distance <- get_cityblock_distance(lexops, "thicket", c("Length", "Zipf.SUBTLEX_UK"))
