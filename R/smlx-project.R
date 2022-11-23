################################################################################
# Check project, initialize connectors, load project
################################################################################
.loadProject <- function(project, software="simulx", ...) {
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
  project <- .checkProjectExtension(project, software)
  
  # if monolix project --> reinit connectors
  lixoftConnectorsState <- NULL
  lixoftConnectorsState <- .lixoftCall("getLixoftConnectorsState", list(quietly=TRUE))
  if (software != lixoftConnectorsState$software) {
    status = .lixoftCall("initializeLixoftConnectors",
                         list(software=software, force=TRUE, path=lixoftConnectorsState$path))
  }
  
  # check if file exists
  .check_file(project, fileType = "Project")
  
  # load project
  lp <- .lixoftCall("loadProject", list(projectFile = project))
  if (!lp) {
    stop("Could not load project '", project, "'", call. = FALSE)
  }
  return(project)
}

# Check is a project is loaded -------------------------------------------------
.isProjectLoaded <- function() {
  isloaded <- !is.null(suppressMessages(.lixoftCall("getProjectSettings")))
  return(isloaded)
}

