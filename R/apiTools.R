.getOS <- function(){
  
  if (.Platform$OS.type == "windows") { 
    return("Windows")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("Apple") 
  } else if (.Platform$OS.type == "unix") { 
    return("Unix")
  } else {
    stop("Unknown OS")
  }
  
}

.checkLixoftConnectorsAvailibility <- function(){
  
  if (!isNamespaceLoaded("lixoftConnectors")){
    stop("lixoftConnectors package is not loaded.", call. = F)
    return(invisible(FALSE))
  }
  
  return(invisible(TRUE))
  
}

################################################################################
# Set lixoft connectors options
################################################################################
set_options <- function(errors=NULL, warnings=NULL, info=NULL) {
  options_list <- list(errors=errors, warnings=warnings, info=info)
  options_list <- options_list[!unlist(lapply(options_list, is.null))]
  op <- getOption("lixoft_notificationOptions")
  for (i in seq_along(options_list)) {
    oname <- names(options_list)[[i]]
    oval <- options_list[[i]]
    if (!is.null(oval)) {
      if (!oval) {
        op[[oname]] <- 1
      } else {
        op[[oname]] <- 0
      }
    }
  }
  options(lixoft_notificationOptions=op)
  return(invisible(TRUE))
}

get_lixoft_options <- function() {
  op <- getOption("lixoft_notificationOptions")
  errors <- ifelse(op$errors == 1, FALSE, TRUE)
  warnings <- ifelse(op$warnings == 1, FALSE, TRUE)
  info <- ifelse(op$info == 1, FALSE, TRUE)
  return(list(errors=errors, warnings=warnings, info=info))
}

.lixoftCall <- function(fun, args = list(), messageMatch = NULL) {
  argsString <- ""
  if (length(args) > 0) {
    argsString <- paste0("args[[", seq_along(args), "]]")
    if (!is.null(names(args))) {
      argsString <- paste(names(args), argsString, sep = " = ")
    }
    argsString <- paste(argsString, collapse = ", ")
  }
  command <- paste0("r <- lixoftConnectors::", fun, "(", argsString, ")")
  
  # res <- eval(parse(text=command))
  res <- withCallingHandlers(
    eval(parse(text=command)),
    message = function(m) {
      message <- conditionMessage(m)
      for (match in names(messageMatch)) {
       message <- gsub(re.escape(match), re.escape(messageMatch[[match]]), message)
      }

      if (grepl("\\[ERROR\\]", message)) {
        message <- gsub("\\[ERROR\\] ", "", message)
        stop(message, call. = FALSE)
      } else if (grepl("\\[WARNING\\]", message)) {
        message <- gsub("\\[WARNING\\] ", "", message)
        warning(message, call. = FALSE)
        invokeRestart("muffleMessage")
      } else {
        message(message)
        invokeRestart("muffleMessage")
      }
    })
  return(res)
}