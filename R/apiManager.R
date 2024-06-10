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
#'   \item \code{software}: the software that is used (should be simulx)
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
  
  # track status: NA = unknown/uninitialized, TRUE = success, FALSE = fail
  status <- NA
  lixoftConnectorsState <- list()
  
  .checkConnectorVersion()
  
  if (!is.null(path)) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  }
  
  # check if RsSimulx needs to be (re-)initialized:
  currentPath <- get("LIXOFT_DIRECTORY", envir = smlxEnvironment)

  if (currentPath != "" && !is.null(path) && path != currentPath) {
    warning("The RsSimulx package has already been initialized with \"", currentPath,
            "\". The R session must be restarted to use a different MonolixSuite installation directory.",
            call. = FALSE)
    status <- FALSE
  }
  
  if (isNamespaceLoaded("lixoftConnectors")){
    
    lixoftConnectorsState <- .lixoftCall("getLixoftConnectorsState", list(quietly = TRUE))
    
    if (!is.null(lixoftConnectorsState)){
      
      if (is.null(path)){

        if (lixoftConnectorsState$software == "simulx"){ # => nothing to be done
          
          assign("LIXOFT_DIRECTORY", lixoftConnectorsState$path, envir = smlxEnvironment)
          status <- TRUE
        }
        
        path <- lixoftConnectorsState$path

      } else if (lixoftConnectorsState$path != path){
        warning("The lixoftConnectors package has already been initialized using a different MonolixSuite installation directory (\"",
                lixoftConnectorsState$path, "\"). The R session must be restarted to use a different MonolixSuite installation directory.",
                call. = F)
        status <- FALSE
      }
    }
  }

  if (is.na(status)) {
    # connectors need initialization:
    status <- .initLixoftConnectorsLibrary(path, ...)
    lixoftConnectorsState <- .lixoftCall("getLixoftConnectorsState", list(quietly = TRUE))
  }
  
  if (status != FALSE)
  {
    .checkLixoftVersion(lixoftConnectorsState)
  }
  
  set_options(info=FALSE)
  
  lixoftConnectorsState$status = status

  return(lixoftConnectorsState) 
}

.initLixoftConnectorsLibrary <- function(path, ...){
  if (is.null(path)) path <- ""
  status <- FALSE
  # suppress warning since will error later for version mismatch
  status = .suppressWarnings(.lixoftCall("initializeLixoftConnectors", list(software = "simulx", path = path, ...)), 
                             "The lixoftConnectors package is not the same version as the software")
  
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
    stop("MonolixSuite does not appear to have been installed. You can install it from http://download.lixoft.com",
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

.checkLixoftVersion <- function(connectorState) {
  if (is.null(connectorState)) stop("Problem initializing lixoftConnectors.")
  
  ver_RsSimulx <- packageVersion("RsSimulx")
  ver_lixoft <- connectorState$version
  ver_lixoft_major <- as.numeric(sub("([0-9]+).*$", "\\1", ver_lixoft)) # extract numbers from beginning
  
  if (ver_RsSimulx$major != ver_lixoft_major) {
    stop(paste0("The major version number for MonolixSuite and the RsSimulx package must be the same:\nMonolixSuite version -> ",
                ver_lixoft, "\nRsSimulx package version -> ", ver_RsSimulx), call. = FALSE)
  }
  return(invisible(TRUE))
}

.checkConnectorVersion <- function() {
  tryCatch(ver_Connectors <- packageVersion("lixoftConnectors"),
           error = function(e) stop("RsSimulx depends on \"lixoftConnectors\" (@Lixoft) package. Please install it using the following command:\n",
                                    "> install.packages(\"", file.path(.findLixoftDirectory(), "connectors", "lixoftConnectors.tar.gz"), "\", repos = NULL)",
                                    call. = FALSE)
  )
  ver_RsSimulx <- packageVersion("RsSimulx")
  
  if (ver_RsSimulx$major != ver_Connectors$major) {
    stop(paste0("The major version number for the lixoftConnectors package and the RsSimulx package must be the same:\nlixoftConnectors package version -> ",
                 ver_Connectors, "\nRsSimulx package version -> ", ver_RsSimulx), call. = FALSE)
  }
  return(invisible(TRUE))
}

# suppress specific warnings 
# .expr expression to evaluate that may throw error
# .text  a string to match the warning via regex 
.suppressWarnings <- function(.expr, .text, ...) {
  eval.parent(substitute(
    withCallingHandlers( .expr, warning = function(w) {
      cm <- conditionMessage(w)
      cond <- grepl(.text, cm)
      if (cond) {
        invokeRestart("muffleWarning")
      }
    })
  ))
}

