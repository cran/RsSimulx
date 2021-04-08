# Round dataframe --------------------------------------------------------------
.roundDataframe <- function(df, nbDigits) {
  for (n in names(df)) {
    df[n] <- sapply(
      df[[n]],
      function(x, nbDigits) {
        if (suppressWarnings(!is.na(as.numeric(x)))) x <- round(as.numeric(x), nbDigits)
        return(x)
      },
      nbDigits
    )
  }
  return(df)
}

# Get the extension of a file --------------------------------------------------
.getFileExt <- function(filename) {
  ext <- utils::tail(strsplit(filename, "\\.")[[1]], n=1)
  if (ext == filename) ext <- NULL
  return(ext)
}

# Get the separator of the data set --------------------------------------------
.getDelimiter <- function(fileName, sep = NULL){
  L <- suppressMessages(suppressWarnings(readLines(fileName, n = 1)))
  sepToCheck = c(' ', '\t', ',', ';')
  if (!is.null(sep)) {
    sepToCheck <- unique(c(sep, sepToCheck))
  }
  nSepToCheck <- length(sepToCheck)
  numfields <- vector(length = nSepToCheck)
  for(index in 1:nSepToCheck){
    numfields[index] <- utils::count.fields(textConnection(L), sep = sepToCheck[index])
  }
  if (all(numfields == 0)) {
    sep <- NULL
  } else {
    sep <- sepToCheck[min(which.max(numfields))]
  }
  return(sep)
}

# Rename dataframe column ------------------------------------------------------
.renameColumns <- function(df, oldName, newName){
  if (length(oldName) != length(newName)) {
    message("[ERROR] vector of old names and new names must match in size")
    return(df)
  }
  for (i in seq_along(oldName)) {
    old <- oldName[i]
    new <- newName[i]
    if (old %in% names(df)) {
      names(df)[names(df) == old] <- new
    }
  }
  return(df)
}

# Read dataset file ------------------------------------------------------------
.readDataset <- function(filename, sep = NULL, ...) {
  filename <- .check_file(filename)
  sep <- .getDelimiter(filename, sep = sep)
  if (is.null(sep))
    stop(paste0("Cannot read file ", filename, "."))
  df <- utils::read.table(filename, sep = sep, ...)
  return(df)
}

#' Read Lixoft@ files
#' 
#' Utility function to read Lixoft@ formated input/output files
#' 
#' @param file file path of the file to read
#' @param sep separator
#' @param \dots see \code{\link{read.table}}
#' @return  a dataframe object
#' @importFrom utils read.table
#' @export
lixoft.read.table <- function(file, sep = "", ...){
  if (sep == "") sep <- NULL
  df <- .readDataset(filename = file, sep = sep, ...)
  return(df)
}

#' Reads  a table into a vector 
#' @param f : path to table file 
#' @param header : bool, use the header or not
#' @param sep : the separator
#' @param quote : the quote character
#' @return  the vector
#' @export
#' 
read.vector <- function(f, header=FALSE, sep="", quote = "\"'") 
{
  t <- .readDataset(filename = f, header = header, sep = sep, quote = quote)
  v <- t[,2]
  names(v) <- t[,1]
  return(v)
}


# Save a dataframe in a temporary file -----------------------------------------
.addDataFrameTemp <- function(df){
  tempFile <- paste0(tempfile(),'.txt')
  tempFile <- gsub(x = tempFile, pattern = '\\\\', replacement = "/")
  utils::write.table(x = df, file = tempFile, quote = F,
                     sep = ';', eol = '\n', row.names = F, col.names = T)
  return(tempFile)
}
