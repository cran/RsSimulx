smlxEnvironment <- new.env()

# assign lixoft environment variables:
assign("LIXOFT_DIRECTORY", "", envir = smlxEnvironment)
assign("ORIGINAL_OPTIONS", options(), envir = smlxEnvironment)

# store original environment variables:
myOS = .getOS()
if (myOS == "Unix") {
  assign("ORIGINAL_SYS_PATH", Sys.getenv("LD_LIBRARY_PATH"), envir = smlxEnvironment)
} else if (myOS == "Apple") {
  # path is set during installation
} else if (myOS == "Windows") {
  assign("ORIGINAL_SYS_PATH", Sys.getenv("PATH"), envir = smlxEnvironment)
}
remove(myOS)

#' Initialize RsSimulx library
#' 
#' Initialize RsSimulx library
#' @param path (\emph{character}) (\emph{optional}) Path to installation directory of the Lixoft suite.
#' If RsSimulx library is not already loaded and no path is given, the directory written in the lixoft.ini file is used for initialization.
#' @param ... (\emph{optional}) Extra arguments passed to lixoftConnectors package when RsSimulx is used with a version of Lixoft(/@) software suite.
#' \itemize{
#' \item \code{force} (\emph{bool}) (\emph{optional}) Should RsSimulx initialization overpass lixoftConnectors software switch security or not. Equals FALSE by default.
#' }
#' @return A list:
#' \itemize{
#'   \item \code{software}: the software that is used (should be monolix with Rsmlx)
#'   \item \code{path}: the path to MonolixSuite
#'   \item \code{version}: the version of MonolixSuite that is used
#'   \item \code{status}: boolean equaling TRUE if the initialization has been successful.
#' }
#' @examples
#' \dontrun{
#' initRsSimulx(path = "/path/to/lixoftRuntime/")
#' }
#' @export
initRsSimulx <- function(path = NULL, ...){
  # check if RsSimulx needs to be (re-)initialized:
  currentPath <- get("LIXOFT_DIRECTORY", envir = smlxEnvironment)
  
  if (!is.null(path)) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }

  if (currentPath != "" && !is.null(path) && path != currentPath) {
    warning("RsSimulx package has already been initialized with \"", currentPath,
            "\". R session must be restarted to use a different Lixoft installation directory.",
            call. = F)
    return(invisible(list(status=FALSE, path=path)))   
  }
  
  if (isNamespaceLoaded("lixoftConnectors")){
    
    lixoftConnectorsState <- NULL
    lixoftConnectorsState <- .lixoftCall("getLixoftConnectorsState", list(quietly = TRUE))
    
    
    if (!is.null(lixoftConnectorsState)){
      
      if (is.null(path)){

        if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
          
          assign("LIXOFT_DIRECTORY", lixoftConnectorsState$path, envir = smlxEnvironment)
          lixoftConnectorsState$status <- TRUE
          return(lixoftConnectorsState)
          
        }
        path <- lixoftConnectorsState$path
        message("lixoftConnectors package has already been initialized using the Lixoft installation directory \"",
                lixoftConnectorsState$path, "\". This directory will be used to run \"simulx\".")
        
      } else if (lixoftConnectorsState$path != path){
        warning("lixoftConnectors package has already been initialized using an other Lixoft installation directory (\"",
                lixoftConnectorsState$path, "\"). R session must be restarted to use a different Lixoft installation directory.",
                call. = F)
        return(invisible(list(status=FALSE)))
      }
      
    }
    
  }

  .checkLixoftVersion(path)

  # lixoft core library initialization:
  lixoft.status <- .initLixoftConnectorsLibrary(path, ...)
  lixoftConnectorsState <- NULL
  lixoftConnectorsState <- .lixoftCall("getLixoftConnectorsState", list(quietly = TRUE))
  lixoftConnectorsState$status <- lixoft.status
  set_options(info=FALSE)

  return(lixoftConnectorsState) 
}

.initLixoftConnectorsLibrary <- function(path, ...){
  if (is.null(path)) path <- ""
  status <- FALSE
  status = .lixoftCall("initializeLixoftConnectors", list(software = "simulx", path = path, ...))
  
  if (status) {
    assign("LIXOFT_DIRECTORY", path, envir = smlxEnvironment)
  }
  
  return(invisible(status))
  
}

.findLixoftDirectory <- function(){
  
  # check Lixoft softwares suite is installed:
  myOS <- .getOS()
  lixoftRootDirectory <- {
    if (myOS == "Windows"){
      file.path(Sys.getenv("USERPROFILE"),"lixoft")
    } else {
      file.path(Sys.getenv("HOME"),"lixoft")
    }
  }
  if (!file.exists(lixoftRootDirectory)){
    stop("Lixoft softwares suite has probably not been installed. You can install it from http://download.lixoft.com",
         call. = F)
  }
  
  
  # find lixoft.ini:
  lixoftIniPath <- file.path(lixoftRootDirectory, "lixoft.ini")
  if (! file.exists(lixoftIniPath)) {
    stop("Either provide a lixoft.ini file in the <home>/lixoft folder or ",
         "call initRsSimulx(path=) to indicate the location of the MonolixSuite ",
         "installation folder.", call. = F)
  }
  
  
  # read lixoft runtime path:
  readDirectory <- function(lines, key){
    
    if (length(lines) > 0){
      for (i in 1:length(lines)){
        res = regexpr(paste0("^", key, "="), lines[[i]])
        if (res != -1)
          return(substring(lines[[i]], res + attr(res, "match.length")))
      } 
    }
    return("")
    
  }
  
  lines <- readLines(lixoftIniPath) # is blocking lixoft.ini for other threads, and lixoft.ini can be modified by mlxComputeR
  
  
  lixoftRuntimeDirectory <- readDirectory(lines, "monolixSuite")
  return(lixoftRuntimeDirectory)
  
}

.checkLixoftDirectory <- function(path){
  
  res = list(status = FALSE, reliesOnLixoftConnectors = NULL)
  
  if (path == "" || !file.exists(file.path(path, 'lib')))
    return(res)
  
  
  myOS = .getOS()
  lixoftConnectorsLibPath <- file.path(path, 'lib', if (myOS == "Windows") 'lixoftConnectors.dll' else 'liblixoftConnectors.so')
  
  if (file.exists(lixoftConnectorsLibPath)){
    
    options(lixoft_lixoftConnectors_loadOptions = list(quietLoad = TRUE, initializePkg = FALSE))
    
    isLixoftConnectorsPkgAvailable <- FALSE
    command <- 'requireNamespace("lixoftConnectors", quietly = TRUE)'
    isLixoftConnectorsPkgAvailable <- eval.parent(parse(text = command))

    if (!isLixoftConnectorsPkgAvailable) {
      stop("RsSimulx depends on \"lixoftConnectors\" (@Lixoft) package. Please install it using the following command:\n",
           "> install.packages(\"", file.path(path, "connectors", "lixoftConnectors.tar.gz"), "\", repos = NULL)",
           call. = F)
    } else {
      res$status = TRUE
    }
    
    res$reliesOnLixoftConnectors = TRUE
    
    options(lixoft_lixoftConnectors_loadOptions = NULL)
    
  }
  return(invisible(res))
  
}

.checkLixoftVersion <- function(path = NULL) {
  # check lixoft version
  if (is.null(path)) path <- .findLixoftDirectory()
  # check that connectors are installed
  .checkLixoftDirectory(path)
  if (length(grep("MonolixSuite[0-9]*", path))) {
    # check connectors version
    version <- regmatches(path, regexpr("MonolixSuite[0-9]*", path))
    version <- gsub("MonolixSuite", "", version)
    if (length(version)) {
      if (as.numeric(version) < 2020) {
        stop("You need to upgrade lixoftConnectors package with a version >= 2020 to use RsSimulx.", call. = F)
      }
    }
  }
  return(invisible(TRUE))
}
