#' Write Simulx Dataset
#'
#' Format outputs of simulx simulations and write datasets in monolix
#' and pkanalix project format.
#' 
#' WARNING: `writeData` function is not implemented for simulx project with regressors in MonolixSuite version 2020R1
#' 
#' @param project (\emph{string}) a simulx project. 
#' If no project specified, the function will run on the project that is already loaded.
#' @param filename (\emph{string}) (\emph{optional}) file path to dataset.
#' (default "simulated_dataset.csv")
#' In case of multiple replicates, the function creates one dataset per replicate with name $filename_repi
#' If filename contains an extension, it must be "csv" or "txt". If it does not, extension is defined by `ext` argument.
#' @param sep (\emph{string}) (\emph{optional}) Separator used to write dataset file. (default ",")
#' It must be one of {"\\t", " ", ";", ","}
#' @param ext (\emph{bool}) (\emph{optional}) Extension used to write dataset file. (default "csv")
#' It must be one of {"csv", "txt"}
#' To defined only if filename with no extension
#' @param nbdigits (\emph{integer}) (\emph{optional}) number of decimal digits in output file.
#' (default = 5)
#'
#' @return a dataframe if one single simulation, a list of dataframe if multiple replicates.
#' @export
#' @examples
#' \dontrun{
#'   # rssimulxDemo.smlx is a Simulx project. This demo simulates three groups
#'   # with different dose levels: low, medium and high.
#'   # Groups have the same number of individuals, population parameters,
#'   # distribution of covariates and outputs.
#'   
#'   # In this example we write data in the home directory with the name demo_simulx
#'   # and a txt extension
#'   writeData("rssimulxDemo.smlx", filename = "demo_simulx", ext = "txt")
#'   
#' } 
writeData <- function(project = NULL, filename = "simulated_dataset.csv",
                      sep = ",", ext = "csv", nbdigits = 5) {
  # connectors
  if (!initRsSimulx()$status) {
    return()
  }

  if (is.null(filename)) {
    filename = "simulated_dataset.csv"
  }
  if (! is.null(.getFileExt(filename))) {
    ext <- .getFileExt(filename)
  }

  .checkDelimiter(sep, "sep")
  .checkExtension(ext, "ext")
  .check_pos_integer(nbdigits, "nbdigits")
  
  # check input smlx project and load it
  if (!is.null(project)) {
    .loadProject(project, software = "simulx")
  } else if (!.isProjectLoaded()) {
    stop("No simulx project loaded", call. = FALSE)
  }

  # run simulation if needed
  if (is.null(.lixoftCall("getSimulationResults"))) {
    .lixoftCall("runSimulation")
  }

  filename <- paste0(tools::file_path_sans_ext(filename), ".", ext)

  # get Simulx data
  groups <- .lixoftCall("getGroups")
  treatment <- .getTreatment()
  regressor <- .getRegressor()
  covariates <- .lixoftCall("getCovariateElements")
  simulation <- .lixoftCall("getSimulationResults")
  
  # check if results have been well outputted
  if (is.null(simulation$res)) {
    stop("No result found for the simulation.", call. = F)
  }
  
  # Write data for a simulx project with Regressors not implemented
  version <- .lixoftCall("getLixoftConnectorsState")$version
  if (!is.null(regressor) & (version == "2020R1")) {
    stop("'writeData' function is not implemented for simulx project with regressors in MonolixSuite version 2020R1", call. = F)
  }
  
  nbRep <- .lixoftCall("getNbReplicates")

  # create a dataset for each replicate
  res <- list()
  for (r in seq_len(nbRep)) {
    simrep <- .filterReplicate(simulation, r)
    if (nbRep == 1) {
      treatrep <- treatment
      regrep <- regressor
    } else {
      treatrep <- NULL
      regrep <- NULL
      if (!is.null(treatment)) {
        treatrep <- subset(treatment, rep == r)
      }
      if (!is.null(regressor)) {
        regrep <- subset(regressor, rep == r)
      }
      
    }
    rdata <- .compileData(simrep, treatrep, regrep, covariates, groups)
    # remove rep column
    rdata <- rdata[names(rdata) != "rep"]
      
    res[[paste0("rep", r)]] <- rdata
  }
  
  # write datasets
  if (nbRep == 1) {
    res <- res$rep1
    # cut digits
    res <- .roundDataframe(res, nbdigits)
    utils::write.table(
      x = res, file = filename,
      row.names = FALSE, sep = sep,
      quote = FALSE
    )
  } else {
    for (r in seq_along(res)) {
      rfile <- paste0(
        tools::file_path_sans_ext(filename),
        "_rep", r, ".", .getFileExt(filename)
      )
      # cut digits
      res[[paste0("rep", r)]] <- .roundDataframe(res[[paste0("rep", r)]], nbdigits)

      # write file
      utils::write.table(
        x = res[[paste0("rep", r)]], file = rfile,
        row.names = FALSE, sep = sep,
        quote = FALSE
      )
    }
  }
  return(res)
}

.compileData <- function(simulation, treatment, regressor, covariates, groups) {
  # Create simulation dataset
  simData <- .convertSimulations(simulation)
  
  if (length(groups) == 1) {
    simData$group <- groups[[1]]$name
  }

  # Create covariates dataset
  covData <- .convertCovariates(simulation, covariates, groups)

  # create dose dataset
  treatmentData <- .convertTreatment(treatment)
  
  # create regressor dataset
  sos <- unique(subset(simData, select = .getSubjocc()))
  regressor <- .convertRegressor(regressor, sos)

  # Create compiled dataset
  if (!is.null(treatmentData)) {
    obsNames <- .getObservationNames()
    obsheader <- ifelse (length(obsNames) == 1, obsNames, "y")
    treatmentData[obsheader] <- "."
    if (is.element("evid", names(treatmentData)))
      simData$evid <- 0
    simData <- merge(simData, treatmentData, all = TRUE)
  }
  if (!is.null(regressor)) {
    simData <- merge(simData, regressor, all = TRUE)
  }
  if (!is.null(covData)) {
    simData <- merge(simData, covData)
  }
  # fill NAN by "."
  for (h in names(simData)) {
    simData[h][is.na(simData[h]),] <- "."
  }
  
  # sort dataframe
  tosort <- intersect(names(simData), c(.getSubjocc(), "time"))
  simData <- simData[do.call(order, simData[,tosort]),]
  
  if (length(unique(simData$group)) == 1) {
    group <- NULL
    simData <- subset(simData, select = - group)
  }
  return(simData)
}

.getSubjocc <- function() {
  subjocc <- c("id")
  occ <- .lixoftCall("getOccasionElements")$names
  if (length(occ) > 0) {
    subjocc <- c("id", occ)
  }
  return(subjocc)
}

.filterReplicate <- function(simulationList, r) {
  nbRep <- .lixoftCall("getNbReplicates")
  if (nbRep == 1) return(simulationList)
  res <- simulationList
  for (n in names(simulationList)) {
    for (m in names(simulationList[[n]])) {
      res[[n]][[m]] <- subset(simulationList[[n]][[m]], rep == r)
      res[[n]][[m]] <- subset(res[[n]][[m]], select = -rep)
    }
  }
  return(res)
}

.getObservationNames <- function() {
  obsnames <- names(.lixoftCall("getSimulationResults")$res)
  return(obsnames)
}

# Create dataset with simulations associated to each group
.convertSimulations <- function(simulation) { 
  
  obsnames <- .getObservationNames()
  simData <- simulation$res[[obsnames[1]]]
  
  if (length(obsnames) > 1) {
    simData$ytype <- 1
    simData <- .renameColumns(simData, obsnames[1], "y")
  
    for (i in seq(2, length(obsnames))) {
      dataobs <- simulation$res[[obsnames[i]]]
      dataobs$ytype <- i
      dataobs <- .renameColumns(dataobs, obsnames[i], "y")
      simData <- rbind(simData, dataobs)
    }
  }
  
  return(simData)
  
}

# Create dataset with covariates associated to each group
.convertCovariates <- function(simulation, covariates, groups) {

  covData <- NULL
  subjocc <- .getSubjocc()

  if (!length(covariates)) return(covData)

  for (g in seq_along(groups)) {
    gname <- groups[[g]]$name
    gcov <- groups[[g]]$covariate
    if (is.null(gcov)) next
    cov <- covariates[[gcov]]$data
    if (is.element("name", names(cov))) {
      gcovnames <- unique(cov$name)
    } else {
      gcovnames <- setdiff(names(cov), subjocc)
    }
    gsim <- simulation$IndividualParameters[[gname]]
    gsim <- subset(gsim, select = c(subjocc, gcovnames))
    gsim$group <- gname
    covData <- rbind(covData, gsim)
  }
  return(covData)
}

# Create dataset with treatments associated to each group
.convertTreatment <- function(treatment) {

  if (is.null(treatment)) return(treatment)

  if (!all(treatment$washout == FALSE))
    treatment$evid <- sapply(treatment$washout, function(x) ifelse(x, 3, 1))
  # remove unused columns
  washout <- tinf <- admtype <- NULL
  treatment <- subset(treatment, select = - washout)
  if (all(is.na(treatment$tinf)))
    treatment <- subset(treatment, select = - tinf)
  if (all(treatment$admtype == 1))
    treatment <- subset(treatment, select = - admtype)
  return(treatment)
}

# Temporary treatment of regressor - when only 1 id in regressor file, spread value of the id to others
.convertRegressor <- function(regressor, sos) {

  if (is.null(regressor))
    return(regressor)
  
  if (all(regressor$id == 1)) {
    ntimes <- nrow(regressor)
    regressor <- do.call("rbind", replicate(nrow(sos), regressor, simplify = FALSE))
    regressor[names(sos)] <- as.data.frame(lapply(sos, rep, each = ntimes))
  }
  return(regressor)
}

.getTreatment <- function() {
  filename <- file.path(.lixoftCall("getProjectSettings")$directory, "Simulation", "doses.txt")
  if (file.exists(filename)) {
    treatment <- utils::read.csv(filename)
    if (nrow(treatment) == 0) treatment <- NULL
  } else {
    treatment <- NULL
  }
  return(treatment)
}

.getRegressor <- function() {
  filename <- file.path(.lixoftCall("getProjectSettings")$directory, "Simulation", "regressors.txt")
  if (file.exists(filename)) {
    regressor <- utils::read.csv(filename)
    if (nrow(regressor) == 0) regressor <- NULL
  } else {
    regressor <- NULL
  }
  return(regressor)
}