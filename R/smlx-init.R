#' Initialize Rssimulx library
#' @param software (\emph{optional}) in {"simulx", "monolix"} The software used by R connectors (default 'simulx')
#' @param errors (\emph{optional}) (\emph{boolean}) Display/Hide lixoft connectors errors (default TRUE)
#' @param warnings (\emph{optional}) (\emph{boolean}) Display/Hide lixoft connectors warnings (default TRUE)
#' @param info (\emph{optional}) (\emph{boolean}) Display/Hide lixoft connectors info (default FALSE)
#' @return A boolean equaling TRUE if the initialization has been successful and FALSE if not.
#' @examples
#' \dontrun{
#' initRssimulx()
#' }
#' @export
initRssimulx <- function(software = "simulx", errors = TRUE, warnings = TRUE, info = FALSE){
  if (! length(find.package("lixoftConnectors", quiet = TRUE)))
    stop("You need to install the lixoftConnectors package in order to use Rssimulx", call. = FALSE)
  
  set_options(errors = errors, warnings = warnings, info = info)

  lixoftConnectorsState <- smlx.getLixoftConnectorsState(quietly = TRUE)
  
  if (!is.null(lixoftConnectorsState)){
    
    if (lixoftConnectorsState$software == software) {
      status=TRUE
    } else {
      status = smlx.initializeLixoftConnectors(software = software)
    }
    
  } else {
    status = smlx.initializeLixoftConnectors(software = software)
  }
  return(invisible(status))
}

################################################################################
# Set lixoft connectors options
################################################################################
set_options <- function(errors = NULL, warnings = NULL, info = NULL) {
  options_list <- list(errors = errors, warnings = warnings, info = info)
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
  options(lixoft_notificationOptions = op)
  return(invisible(TRUE))
}

get_lixoft_options <- function() {
  op <- getOption("lixoft_notificationOptions")
  errors <- ifelse(op$errors == 1, FALSE, TRUE)
  warnings <- ifelse(op$warnings == 1, FALSE, TRUE)
  info <- ifelse(op$info == 1, FALSE, TRUE)
  return(list(errors = errors, warnings = warnings, info = info))
}

################################################################################
# Check project, initialize connectors, load project
################################################################################
.loadProject <- function(project, software = "simulx", ...) {
  # initialize library
  if (!initRssimulx(software = software, ...)){
    return()
  }
  
  # demo
  if (project == "rssimulxDemo.smlx") {
    rssimulxDemo.project <- rssimulxDemo.model <- NULL
    rm(rssimulxDemo.project, rssimulxDemo.model)
    eval(parse(text="data(RsSimulxDemo)"))
    tmp.dir <- tempdir()
    write(rssimulxDemo.project, file=file.path(tmp.dir,"rssimulxDemo.smlx"))
    modeldir <- file.path(tmp.dir, "rssimulxDemo", "ModelFile")
    if (!dir.exists(modeldir)) dir.create(modeldir, recursive = TRUE)
    write(rssimulxDemo.model, file=file.path(modeldir, "model.txt"))
    project <- file.path(tmp.dir, project)
    demo <- TRUE
  } else {
    demo <- FALSE
  }
  
  # check extension & add extension if needed
  ext <- .getFileExt(project)
  if (is.null(ext)) {
    if (software == "simulx") {
      project <- paste0(project, ".smlx")
    } else if (software == "monolix") {
      project <- paste0(project, ".mlxtran")
    } else {
      return()
    }
  } else {
    if (software == "simulx" & ext != "smlx")
      stop("You need to load a simulx project, with a .smlx extension.")
    if (software == "monolix" & ext != "mlxtran")
      stop("You need to load a monolix project, with a .mlxtran extension.")
  }
  
  # check if file exists
  .check_file(project, fileType = "Project")
  
  # load project
  lp <- smlx.loadProject(project) 
  if (!lp) 
    stop("Could not load project '", project, "'", call.=FALSE)
  return(project)
}
