#' Simulation of mixed effects models and longitudinal data
#'
#' Compute predictions and sample data from \code{Mlxtran} and \code{R} models
#' 
#' simulx takes advantage of the modularity of hierarchical models for simulating 
#' different components of a model: models for population parameters, individual 
#' covariates, individual parameters and longitudinal data.
#' 
#' Furthermore, \code{simulx} allows to draw different types of longitudinal data, 
#' including continuous, count, categorical, and time-to-event data.
#' 
#' The models are encoded using either the model coding language \samp{Mlxtran}. 
#' \samp{Mlxtran} models are automatically converted into C++ codes, 
#' compiled on the fly and linked to R using the \samp{RJSONIO} package. 
#' That allows one to implement very easily complex models and to take advantage 
#' of the numerical sovers used by the C++ \samp{mlxLibrary}.
#' 
#' See http://simulx.lixoft.com for more details.      
#' @param model a \code{Mlxtran} model used for the simulation. It can be a text file or an outut of the inLine function.
#' You can now create a new simulx project providing a model using `newProject` lixoft simulx connector.
#' @param parameter a vector of parameters with their names and values
#' You can now define a new individual element using `defineIndividualElement` lixoft simulx connector and 
#' a new population element with 'definePopulationElement' lixoft simulx connector.
#' @param output a list (or list of lists) with fields: 
#' \itemize{
#'   \item \code{name}: a vector of output names
#'   \item \code{time}: a vector of times (only for the longitudinal outputs)
#'   \item \code{lloq}: lower limit of quantification (only for the longitudinal outputs)
#'   \item \code{uloq}: upper limit of quantification (only for the longitudinal outputs)
#'   \item \code{limit}: lower bound of the censoring interval (only for the longitudinal outputs)
#' }
#' You don't need to add individual parameters in output anymore since there are now automatically outputed.
#' You can now define a new output element using `defineOutputElement` lixoft simulx connector.
#' @param treatment a list with fields
#' \itemize{
#'   \item \code{time} : a vector of input times,
#'   \item \code{amount} : a scalar or a vector of amounts,
#'   \item \code{rate} : a scalar or a vector of infusion rates (default=\code{Inf}),
#'   \item \code{tinf} : a scalar or a vector of infusion times (default=0),
#'   \item \code{type} : the type of input (default=1).
#' }
#' "target" field is not supported anymore in RsSimulx.
#' You can now define a new treatment element using `defineTreatmentElement` lixoft simulx connector.
#' @param regressor a list, or a list of lists, with fields
#' \itemize{
#'   \item \code{name} : a vector of regressor names,
#'   \item \code{time} : a vector of times,
#'   \item \code{value} : a vector of values.
#' }
#' You can now define a new regressor element using `defineOccasionElement` lixoft simulx connector.
#' @param varlevel a list (or a dataframe) with fields: 
#' \itemize{
#'   \item \code{name} : name of the variable which defines the occasions,
#'   \item \code{time} : a vector of times (beginnings of occasions)
#' }
#' You can now define occasion levels using `defineOccasionElement` lixoft simulx connector.
#' @param group a list, or a list of lists, with fields: 
#' \itemize{
#'   \item \code{size} : size of the group (default=1),
#'   \item \code{parameter} : if different parameters per group are defined,
#'   \item \code{output} : if different outputs per group are defined,
#'   \item \code{treatment} : if different treatments per group are defined,
#'   \item \code{regressor} : if different regression variables per group are defined.
#' }
#' "level" field is not supported anymore in RsSimulx.
#' You can now define a new simulation group using `addGroup` lixoft simulx connector and 
#' use connectors `setGroupElement`, `setGroupSize`, to define groups attributes.
#' @param addlines a list with fields: 
#' \itemize{
#'   \item \code{formula}: string, or vector of strings, to be inserted .
#' }
#' "section", "block" field are not supported anymore in RsSimulx.
#' You only need to specify a formula. The additional lines will be added in a new section EQUATION.
#' You can now add lines to the model file using `setAddLines` lixoft simulx connector.
#' @param data a list (output of simulx when settings$data.in==TRUE)
#' `data` argument is deprecated
#' @param project the name of a Monolix project
#' You can now import all elements coming from a Monolix project to perform simulations 
#' using `importMonolixProject` lixoft simulx connector.
#' @param nrep number of replicates
#' You can now define the number of replicates if the simulation using `setNbReplicates` lixoft simulx connector.
#' @param npop number of population parameters to draw randomly 
#' @param fim a string with the Fisher Information Matrix to be used 
#' @param result.folder the name of the folder where the outputs of simulx should be stored
#' `result.folder` argument is deprecated
#' @param result.file the name of the single file where the outputs of simulx should be saved
#' @param stat.f a R function for computing some summary (mean, quantiles, survival,...) of the simulated data. Default = "statmlx".
#' `stat.f` argument is deprecated
#' @param settings a list of optional settings
#' \itemize{
#'   \item \code{seed} : initialization of the random number generator (integer) (by default a random seed will be generated)
#'   \item \code{id.out}  : add (TRUE) / remove (FALSE) columns id and group when only one element (N = 1 or group = 1) (default=FALSE)
#'   \item \code{kw.max} : maximum number of trials for generating a positive definite covariance matrix (default = 100) 
#'   \item \code{sep} : the field separator character (default = ",") 
#'   \item \code{digits} : number of decimal digits in output files (default = 5) 
#'   \item \code{replacement} : TRUE/FALSE (default = FALSE) sample id's with/without replacement
#'   \item \code{out.trt} : TRUE/FALSE (default = TRUE) output of simulx includes treatment
#' }       
#' 
#' @return A list of data frames. Each data frame is an output of simulx
#' 
#' @examples
#' \dontrun{
#' myModel <- inlineModel("
#' [LONGITUDINAL]
#' input = {A, k, c, a}
#' EQUATION:
#' t0    = 0 
#' f_0   = A
#' ddt_f = -k*f/(c+f)
#' DEFINITION:
#' y = {distribution=normal, prediction=f, sd=a}
#' [INDIVIDUAL]
#' input = {k_pop, omega}
#' DEFINITION:
#' k = {distribution=lognormal, prediction=k_pop, sd=omega}
#' ")
#' f <- list(name='f', time=seq(0, 30, by=0.1))
#' y <- list(name='y', time=seq(0, 30, by=2))
#' res <- simulx(model     = myModel, 
#'               parameter = c(A=100, k_pop=6, omega=0.3, c=10, a=2), 
#'               output    = list(f,y),
#'               group     = list(size=4))
#' 
#' plot(ggplotmlx() + geom_line(data=res$f, aes(x=time, y=f, colour=id)) +
#'      geom_point(data=res$y, aes(x=time, y=y, colour=id)))
#' print(res$parameter)
#' }
#' 
#' @importFrom stats runif
#' @importFrom utils read.table
#' @export

simulx <- function(model = NULL, parameter = NULL, output = NULL, treatment = NULL, 
                   regressor = NULL, varlevel = NULL, group = NULL, 
                   project = NULL, nrep = 1, npop = NULL, fim = NULL, 
                   result.folder = NULL, result.file = NULL, stat.f = "statmlx",
                   addlines = NULL, settings = NULL, data = NULL) {
  
  params <- as.list(match.call(expand.dots = TRUE))[-1]
  
  initRssimulx(software = "simulx")

  #=============================================================================
  # Raise message for not supported arguments compared to mlxR function
  #=============================================================================
  # Message for not implemented settings
  notImplementedSettings <- c("load.design", "data.in", "disp.iter")
  if (any(is.element(notImplementedSettings, names(settings)))) {
    message("[INFO] '", paste(notImplementedSettings, collapse = "', '"),
            "' information are not anymore supported for `settings` argument. They will be ignored.")
  }
  
  # Message for not implemented data
  if (is.element("data", names(params))) {
    message("[INFO] 'data' argument is not supported by RsSimulx. It will be ignored")
  }
  
  # Message for not implemented stat.f
  if (is.element("stat.f", names(params))) {
    message("[INFO] 'stat.f' argument is not supported by RsSimulx. It will be ignored")
  }
  
  # Message for not implemented result.folder
  if (is.element("result.folder", names(params))) {
    message(
      "[INFO] Saving several files in a folder is not implemented. ",
      "'result.folder' argument will be ignored. ",
      "Check 'result.file' argument if you want to save the result of the simulation"
    )
  }
  
  #=============================================================================
  # Check the inputs of Simulx
  #=============================================================================
  #-----------------------------------------------------------------------------
  # Model file
  nbID <- 1
  if(!is.null(model)){
    .checkModel(fileName = model)
  }
  set_options(warnings = FALSE)
  
  # Settings
  settings <- .initsimulxSettings(settings)
  
  #-----------------------------------------------------------------------------
  # Project file
  if(!is.null(project)){
    .check_file(filename = project, fileType = "Project")
  }
  
  #-----------------------------------------------------------------------------
  # Project file and model file
  if(is.null(model) & is.null(project))
    stop("You must define either the model or the Monolix project file.", call. = FALSE)
  if (!is.null(model) & !is.null(project))
    stop("You must define either the model or the Monolix project file, not both of them.", call. = FALSE)
  
  #-----------------------------------------------------------------------------
  # Parameter
  # merge parameters
  parameter <- .transformParameter(parameter)
  # check parameter format
  parameter <- .checkParameter(parameter)
  # Write the data set as a .txt file
  if (is.data.frame(parameter)) {
    indexID <- which(names(parameter)=='id')
    if (length(indexID) > 0) {
      nbID <- max(nbID,length(unique(parameter[,indexID[1]])))
    }
    indexPop <- which(names(parameter)=='pop')
    if (length(indexPop) > 0) {
      nbID <- max(nbID,length(unique(parameter[,indexPop[1]])))
    }
    parameter <- .addDataFrameTemp(df = parameter)
  }
  #-----------------------------------------------------------------------------
  # Output
  # check output format
  output <- .checkOutput(output)
  # if dataframe, save it in a txt file
  for (iout in seq_along(output)) {
    outputValue <- output[[iout]]
    if (is.data.frame(outputValue)) {
      print("output is dataframe !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
    if (is.data.frame(outputValue$time)) {
      dfout <- outputValue$time
      # Write the data set as a .txt file
      indexID <- which(names(dfout) == 'id')
      if (length(indexID) > 0) {
        nbID <- max(nbID,length(unique(dfout[,indexID[1]])))
      }
      output[[iout]]$time <- .addDataFrameTemp(df = dfout)
    }
  }
  #-----------------------------------------------------------------------------
  # Treatment
  # check treatment format
  treatment <- .checkTreatment(treatment)
  treatment <- .splitTreatment(treatment)
  # if dataframe, save it in a txt file
  for (itreat in seq_along(treatment)) {
    treatementValue <- treatment[[itreat]]
    if (is.data.frame(treatementValue)) {
      # Write the data set as a .txt file
      indexID <- which(names(treatementValue) == 'id')
      if (length(indexID) > 0) {
        nbID <- max(nbID,length(unique(treatementValue[,indexID[1]])))
      }
      treatment[[itreat]] <-.addDataFrameTemp(df = treatementValue)
    }
  }
  
  #-----------------------------------------------------------------------------
  # Regressor
  # check regressor format
  regressor <- .checkRegressor(regressor)
  # merge regressors
  regressor <- .transformRegressor(regressor)
  # When regressor is defined as dataframe, write the data set as a .txt file
  if (is.data.frame(regressor)) {
    indexID <- which(names(regressor) == 'id')
    if (length(indexID) > 0) {
      nbID <- max(nbID,length(unique(regressor[,indexID[1]])))
    }
    regressor <- .addDataFrameTemp(df = regressor)
  }

  #-----------------------------------------------------------------------------
  # Group
  allowedNames <- c("size", "parameter", "output", "treatment", "regressor", "level")
  if (any(is.element(allowedNames, names(group))) | !all(sapply(group, .is_list_or_named_vector))) {
    group <- list(group)
  }
  groupsWithLevel <- group[sapply(group, function(g) is.element("level", names(g)))]
  for (g in groupsWithLevel) {
    if (any(g[["level"]] != "individual")) {
      stop("'level' information in 'group' argument is not supported by RsSimulx. \n",
           "Simulations are only run at the individual level. If you have variability at other levels, check the online documentation.", call. = FALSE)
    }
  }

  group <- .checkGroup(group)

  #=============================================================================
  # Initialize the simulation
  #=============================================================================
  if (!is.null(model)) {
    # The simulation is initialized by a model
    
    # Check that the output is not null
    isOutputInGroupHere <- T
    if (is.null(output)) {
      if (!is.list(group[[1]])) {
        for (indexGrp in seq_along(group)) {
          if (!is.element("output", names(group[[indexGrp]]))) {
            isOutputInGroupHere <- F
          }
        }
      }
    }
    if (!isOutputInGroupHere) {
      stop("If only a model is defined, an output element is mandatory to run (either in output or in group definition).", call. = F)
    } else{
      bIsOutput <- .checkModelOutputSection(model)
      if (!bIsOutput) {
        outputName <- .getOutputNames(output, group)
        model <- .addOutputToFile(model, outputName[1])
      }
      # The simulation is made through a model file
      bNewProject <- smlx.newProject(modelFile = model)
      smlx.setGroupSize(smlx.getGroups()[[1]]$name, nbID)
    }
  } else {
    # The simulation is initialized by a Monolix project
    if (!is.null(npop)) {
      popParam <- simpopmlx(n = npop, project = project, fim = fim, kw.max = settings[["kw.max"]])
      if (is.element("pop", names(popParam))) popParam <- popParam[names(popParam) != "pop"]
    }
    bNewProject <- smlx.importMonolixProject(projectFile = project)
    if (is.element("mlx_PopInit", names(smlx.getPopulationElements()))) {
      stop("Population parameter estimation has not been launched in monolix project.", call. = FALSE)
    }
    if (!is.null(npop)) {
      smlx.definePopulationElement(name = "popSimpopMlx", element = .addDataFrameTemp(df = popParam))
      smlx.setGroupElement(group=smlx.getGroups()[[1]]$name, elements = "popSimpopMlx")  
    }
    if (nbID > 1) {
      smlx.setGroupSize(smlx.getGroups()[[1]]$name, nbID)
    }
  }
  if (!bNewProject) {return(invisible(FALSE))}
  
  if (!is.null(npop)) {
    nrep <- npop * nrep
  }

  # Occasions
  if (!is.null(varlevel)) {
    if (is.data.frame(varlevel)) {
      # Write the data set as a .txt file
      indexID <- which(names(varlevel) == 'id')
      if (length(indexID) > 0) {
        nbID <- max(nbID,length(unique(varlevel[,indexID[1]])))
        smlx.setGroupSize(smlx.getGroups()[[1]]$name, nbID)
      }
      names(varlevel)[indexID] <- "ID"
      varlevel <-.addDataFrameTemp(df = varlevel)
      smlx.defineOccasionElement(element = varlevel)
    } else {
      smlx.defineOccasionElement(element = data.frame(time = varlevel$time, occ = 1:length(varlevel$time)))
    }
  }

  # Add additional lines in the model ------------------------------------------
  if (is.element("formula", names(addlines))) {
    addlines <- list(addlines)
  }
  if (!is.null(addlines)) {
    for (newline in addlines) {
      if (is.element("section", names(newline))) {
        if (! grepl("LONGITUDINAL", newline$section)) 
          stop("'addlines' argument only support 'LONGITUDINAL' section and 'EQUATION' block.", call. = FALSE)
      }
      if (is.element("block", names(newline))) {
        if (grepl("EQUATION", newline))
          stop("'addlines' argument only support 'LONGITUDINAL' section and 'EQUATION' block.", call. = FALSE)
      }
      smlx.setAddLines(lines = newline$formula)
    }
  }
  #=============================================================================
  # Design the Shared group
  #=============================================================================
  #-----------------------------------------------------------------------------
  # Add output
  .addOutput(output)
  #-----------------------------------------------------------------------------
  # Add parameter
  if (is.list(parameter) & !is.null(names(parameter))) {
    if (names(parameter)[1] == "name") {
      param <- as.vector(parameter$value)
      names(param) <- parameter$name
      parameter <- param
    }
  }

  if (!is.null(parameter)) {
    
    if ((parameter[1] == "mode") | (parameter[1] == "mean") | (parameter[1] =="pop")) {
      if (parameter[1] == "mode") {
        if (! is.element("mlx_EBEs", names(smlx.getIndividualElements()))) {
          stop("Invalid parameter mode. EBEs parameters were not found in monolix project.", call. = FALSE)
        }
        smlx.setGroupElement(group=smlx.getGroups()[[1]]$name, elements = "mlx_EBEs")
      } else if(parameter[1] == "mean") {
        if (! is.element("mlx_CondMean", names(smlx.getIndividualElements()))) {
          stop("Invalid parameter mean. Conditional mean parameters were not found in monolix project.", call. = FALSE)
        }
        smlx.setGroupElement(group=smlx.getGroups()[[1]]$name, elements = "mlx_CondMean")
      } else {
        if (! is.element("mlx_CondMean", names(smlx.getIndividualElements()))) {
          stop("Invalid parameter pop. Individual population parameters were not found in monolix project.", call. = FALSE)
        }
        smlx.setGroupElement(group=smlx.getGroups()[[1]]$name, elements = "mlx_PopIndiv")
      }
      remaining <- smlx.getGroupRemaining(group = smlx.getGroups()[[1]]$name)
      popData <- smlx.getPopulationElements()$mlx_Pop$data
      namesPopData <- names(popData)
      for (indexParam in seq_along(remaining)) {
        remainingName <- names(remaining)[indexParam]
        remaining[indexParam] <- popData[which(namesPopData==remainingName)]
      }
      smlx.setGroupRemaining(group = smlx.getGroups()[[1]]$name, remaining)
    } else {
      .addParameter(parameter)
    }
  }
  
  #-----------------------------------------------------------------------------
  # Add treatment
  .addTreatment(treatment)
  
  #-----------------------------------------------------------------------------
  # Add regressor
  .addRegressor(regressor)
  
  groupOut <- NULL
  
  #=============================================================================
  # Design the groups
  #=============================================================================
  if (!is.null(group)) {
    nbGroup <- length(group)
    if (nbGroup == 1) {
      .addGroup(group[[1]])
    } else if (nbGroup > 1) {
      for (indexGroup in 2:nbGroup) {
        smlx.addGroup(paste0("simulationGroup",indexGroup))
      }
      for (indexGroup in seq_len(nbGroup)) {
        .addGroup(group = group[[indexGroup]], groupName = paste0("simulationGroup", indexGroup))
        groupOut[[indexGroup]] <- list(size = smlx.getGroups()[[indexGroup]]$size, level = 'longitudinal')
      }
    }
  }
  #=============================================================================
  # Manage the replicates
  #=============================================================================
  if (nrep > 1) {
    smlx.setNbReplicates(nrep)
  }
  
  #=============================================================================
  # Manage the settings
  #=============================================================================
  smlx.setProjectSettings(seed = settings[['seed']])
  bOutTrt <- settings[['out.trt']]
  if (settings[['replacement']]) {
    smlx.setSamplingMethod( method = "withReplacement" )
  }
  
  for (index in seq_along(smlx.getGroups())) {
    smlx.renameGroup(paste0('simulationGroup', index), paste0(index))
  }
  #=============================================================================
  # Run the simulation
  #=============================================================================
  smlx.runSimulation()

  simOut <- smlx.getSimulationResults()$res
  
  for (index in seq_along(simOut)) {
    outName <- names(simOut)[index]
    out <- simOut[[index]]

    # censor output
    out <- .censorOutput(out, outName, output, group)
    
    # rearrange rep and pop columns
    out <- .rearrangeRepPop(out, npop)
      
    # factor rep id group cens pop & remove unused columns
    out <- .postProcessDataFrames(out, force = settings[["id.out"]])

    # atttributes
    i_Val <- which(names(out) == outName)
    out[,ncol(out) + 1] <- out[,i_Val]
    names(out)[ncol(out)] <- outName
    out <- out[,-i_Val]
    attributes(out) <- c(attributes(out), name = outName)

    simOut[[index]] <- out
  }
  if(!is.null(groupOut)){
    simOut$group <- groupOut
  }
  
  ##############################################################################
  # Manage parameter output
  ##############################################################################
  xx <- smlx.getSimulationResults()$IndividualParameters
  if (length(xx) == 1) {
    paramOut <- xx[[1]]
  } else {
    for (g in names(xx)) {
      xx[[g]] <- cbind(xx[[g]], group = g)
    }
    paramOut <- do.call(rbind, xx)
    row.names(paramOut) <- seq_len(nrow(paramOut))
  }
  
  if (ncol(paramOut) > 1) {
    for (indexCol in 2:ncol(paramOut)) {
      notDouble <- suppressWarnings(sum(is.na(as.double(paramOut[,indexCol]) == paramOut[,indexCol])))
      if (notDouble == 0) {
        paramOut[,indexCol] <- as.double(paramOut[,indexCol])
      }            
    }
  }

  # rearrange rep and pop columns
  paramOut <- .rearrangeRepPop(paramOut, npop)
  
  # factor rep id group cens pop & remove unused columns
  paramOut <- .postProcessDataFrames(paramOut, force = settings[["id.out"]])

  # if only one set of parameter, return parameter as named vector
  if (nrow(unique(paramOut[! names(paramOut) %in% c("rep", "id", "group")])) == 1) {
    paramOut <- utils::head(paramOut[! names(paramOut) %in% c("rep", "id", "group")], n = 1)
  }
  if (nrow(paramOut) == 1) {
    paramOut <- unlist(paramOut)
  }
  
  simOut$parameter <- paramOut

  ##############################################################################
  # Manage treatment output
  ##############################################################################
  if (bOutTrt) {
    trtFile <- paste0(smlx.getProjectSettings()$directory, '/Simulation/doses.txt')
    if (file.exists(trtFile)) {
      treat <- read.table(file = trtFile, header = T, sep = .getDelimiter(trtFile))
      
      # rename column amt to amount
      treat <- .renameColumns(treat, "amt", "amount")

      if (sum(is.na(treat$tinf)) == nrow(treat)) {
        treat <- treat[, names(treat) != "tinf"]
      }
      if (is.element("tinf", names(treat))) {
        treat$tinf[!is.na(treat$tinf)] <- treat$amount[!is.na(treat$tinf)] / treat$tinf[!is.na(treat$tinf)]
        treat <- .renameColumns(treat, "tinf", "rate")
      }
      
      # rearrange rep and pop columns
      treat <- .rearrangeRepPop(treat, npop)
      
      # factor rep id group cens pop & remove unused columns
      treat <- .postProcessDataFrames(treat, force = settings[["id.out"]])
      
      simOut$treatment <- treat
    }
  }
  
  ##############################################################################
  # Manage population output
  ##############################################################################
  if (!is.null(nrep)) {
    popFile <- paste0(smlx.getProjectSettings()$directory,'/Simulation/populationParameters.txt')
    if (file.exists(popFile)) {
      popData <- read.table(file = popFile, header = T, sep = .getDelimiter(popFile))

      # rearrange rep and pop columns
      popData <- .rearrangeRepPop(popData, npop)
      
      # factor rep id group cens pop & remove unused columns
      popData <- .postProcessDataFrames(popData, force = settings[["id.out"]])
      
      # if only one set of parameter, return only the first row
      if (nrow(unique(popData[! names(popData) %in% c("rep", "id", "group")])) == 1) {
        popData <- popData[! names(popData) %in% c("rep", "id", "group")][1,]
      }
      # if one row: return a named vector
      if (nrow(popData) == 1) {
        popData <- unlist(popData)
      }

      simOut$population <- popData
    }
  }
  
  ##############################################################################
  # Write data
  ##############################################################################
  if (!is.null(result.file)) {
    writeData(filename = result.file, sep = settings$sep, nbdigits = settings$digits)
  }
  
  return(simOut)
}

.initsimulxSettings <- function(settings) {
  if (is.null(settings)) settings <- list()
  if (!is.element("seed", names(settings))) settings$seed <- sample.int(1e6, 1)
  if (!is.element("kw.max", names(settings))) settings[["kw.max"]] <- 100
  if (!is.element("sep", names(settings))) settings$sep <- ","
  if (!is.element("digits", names(settings))) settings$digits <- 5
  if (!is.element("replacement", names(settings))) settings[['replacement']] <- FALSE
  if (!is.element("out.trt", names(settings))) settings[['out.trt']] <- TRUE
  if (!is.element("id.out", names(settings))) settings[['id.out']] <- FALSE
  return(settings)
}

.rearrangeRepPop <- function(df, npop) {
  if (! "rep" %in% names(df)) return(df)
  if (is.null(npop)) npop <- 1
  if (npop == 1) return(df)
  nrep <- length(unique(df$rep)) / npop
  count <- table(df$rep)[[1]]
  pop <- rep(rep(seq_len(npop), each = count), nrep)
  rep <- rep(seq_len(nrep), each = count * npop)
  df$rep <- rep
  df$pop <- pop
  # reorder
  df <- df[, c("rep", "pop", names(df)[! names(df) %in% c("rep", "pop")])]
  return(df)
}

.postProcessDataFrames <- function(df, force = FALSE) {
  cols <- intersect(c("cens", "pop", "rep", "id", "group"), names(df))
  for (col in cols) {
    df[[col]] <- as.factor(df[[col]])
  }
  
  if (is.element("rep", names(df))) {
    if (length(unique(df$rep)) == 1) df <- df[names(df) != "rep"]
  }

  if (is.element("pop", names(df))) {
    if (length(unique(df$pop)) == 1) df <- df[names(df) != "pop"]
  }
  
  if (is.element("id", names(df))) {
    if (length(unique(df$id)) == 1 & ! force) df <- df[names(df) != "id"]
  }
  
  if (is.element("group", names(df))) {
    if (length(unique(df$group)) == 1 & ! force) df <- df[names(df) != "group"]
  }
  return(df)
}
