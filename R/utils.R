re.escape <- function(strings){
  vals <- c("\\\\", "\\[", "\\]", "\\(", "\\)", 
            "\\{", "\\}", "\\^", "\\$","\\*", 
            "\\+", "\\?", "\\.", "\\|")
  replace.vals <- paste0("\\\\", vals)
  for(i in seq_along(vals)){
    strings <- gsub(vals[i], replace.vals[i], strings)
  }
  strings
}

# Return TRUE is an object is a list or a named vector--------------------------
.is_list_or_named_vector <- function(l) {
  res <- FALSE
  if (is.list(l)) res <- TRUE
  if (is.vector(l)) {
    if (!is.null(names(l))) {
      res <- TRUE
    }
  }
  return(res)
}

# transform values in dataframe to numeric -------------------------------------
.transformToNumeric <- function(df) {
  regexp_pattern <- "(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)"
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      isnumeric <- all(grepl(pattern = regexp_pattern, x = df[[col]]))
      if (isnumeric & is.character(df[[col]])) {
        df[[col]] <- sapply(df[[col]], as.numeric)
      }
    }
  }
  return(df)
}
