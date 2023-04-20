##############################################################################"
# Check the model file
##############################################################################"
.checkModel <- function(fileName){
  filename <- .check_file(fileName)

  # check if a distribution is defined in [COVARIATE] section
  covsection <- .getModelSection(filename, section = "COVARIATE", block = "DEFINITION")
  if (!is.null(covsection)) {
    if (any(grepl("distribution", covsection))) {
      stop(
        "Invalid model file. Definition of distributions for covariates in the [COVARIATE] block is not supported anymore. \n",
        "Instead, generate the covariates in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See 'http://simulx.lixoft.com/definition/model/' for model definition.",
        call. = F
      )
    }
    if (any(grepl("P\\(([a-zA-Z0-9]|\\s|=)*\\)", covsection))) {
      stop(
        "Invalid model file. Definition of distributions for covariates in the [COVARIATE] block is not supported anymore. \n",
        "Instead, generate the covariates in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See 'http://simulx.lixoft.com/definition/model/' for model definition.",
        call. = F
      )
    }
  }
  
  # check if a distribution is defined in [POPULATION] section
  popsection <- .getModelSection(filename, section = "POPULATION", block = "DEFINITION")
  if (!is.null(popsection)) {
    if (any(grepl("distribution", popsection))) {
      stop(
        "Definition of distributions for population parameters in the [POPULATION] block is not supported anymore. \n",
        "Instead, generate the population parameters in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See 'http://simulx.lixoft.com/definition/model/' for model definition.",
        call. = F
      )
    }
    if (any(grepl("P\\(([a-zA-Z0-9]|\\s|=)*\\)", popsection))) {
      stop(
        "Definition of distributions for population parameters in the [POPULATION] block is not supported anymore. \n",
        "Instead, generate the population parameters in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See 'http://simulx.lixoft.com/definition/model/' for model definition.",
        call. = F
      )
    }
  }
  
  # check if there is an [OUTPUT] section
  
  # check how the error model is defined in [LONGITUDINAL] DEFINITION
  # longsection <- .getModelSection(filename, section = "LONGITUDINAL", block = "DEFINITION")
  # if (length(grep("sd", longsection))) {
  #   stop(
  #     "Invalid model file. Use `errorModel=` instead of`sd=` to define the error model.",
  #     call. = F
  #   )
  # }
  return(invisible(TRUE))
}

##############################################################################"
# Check if the model file contains an OUTPUT: section
##############################################################################"
.checkModelOutputSection <- function(fileName){
  if (grepl("^lib:", fileName)) {
    lines <- .lixoftCall("getLibraryModelContent", args = list(filename = fileName, print = FALSE))
  } else {
    lines <- suppressWarnings(readLines(con = fileName, n = -1))
  }
  bIsOUT = F
  for(index in 1: length(lines)){
    bIsOUT <- bIsOUT|grepl(x =lines[index], pattern = 'OUTPUT:')
  }
  return(bIsOUT)
}

################################################################################
# Check the parameters
################################################################################
.checkParameter <- function(parameter, mlxProject=F){
  if(!is.null(parameter)){
    # Monolix parameters
    if (is.string(parameter)) {
      if (parameter %in% c("mode", "mean", "pop")) {
        message("[INFO] 'mode', 'mean', 'pop' are deprecated parameters. Parameters imported from",
                " monolix must be in {'", paste0(.getAllowedMlxParameterNames(), collapse="', '"),
                "'}.")
        if (parameter == "mode") {
          parameter <- "mlx_EBEs"
        } else if (parameter == "mean") {
          parameter <- "mlx_CondMean"
        }
      }
      # parameter path to dataframe
      if (file.exists(parameter)) {
        .checkExtension(.getFileExt(parameter), "parameter file extension")
      } else if (parameter %in% .getAllowedMlxParameterNames()) {
        if (! mlxProject) {
          stop("Invalid parameter. Monolix parameters can only be used when a project is imported from monolix.", call. = F)
        }
      } else {
        stop("Invalid parameter. Parameters defined as string must be either a path to a csv or txt file or ",
              "a parameter imported from monolix ('",
              paste0(.getAllowedMlxParameterNames(), collapse="', '"), "').",
             call.=F)
      }
    } else {
      if (!(is.vector(parameter) || (is.data.frame(parameter)))) {
        stop("Invalid paramerer. It must be a vector or a data.frame.", call. = F)
      }
    }
  }
  return(parameter)
}

.checkCovariate <- function(covariate, mlxProject=F){
  if(!is.null(covariate)){
    # Monolix covariates
    if (is.string(covariate)) {
      # parameter path to dataframe
      if (file.exists(covariate)) {
        .checkExtension(.getFileExt(covariate), "covariate file extension")

      } else if (covariate %in% .getAllowedMlxCovariateNames()) {
        if (! mlxProject) {
          stop("Invalid covariate. Monolix covariates can only be used when a project is imported from monolix.", call. = F)
        }
      } else {
        stop("Invalid covariate. Covariates defined as string must be either a path to a csv or txt file or ",
             "a covariate imported from monolix ('",
             paste0(.getAllowedMlxCovariateNames(), collapse="', '"), "').",
             call.=F)
      }
    } else {
      if(!(is.vector(covariate)||(is.data.frame(covariate)))) {
        stop("Invalid covariate It must be a vector or a data.frame.", call. = F)
      }
    }
  }
  return(covariate)
}

.checkMissingParameters <- function(parameter, expectedParameters, frommlx = FALSE) {
  diff <- setdiff(expectedParameters, parameter)
  ismissing <- FALSE
  if (length(diff)) {
    ismissing <- TRUE
    if (length(diff) == 1) {
      message <- paste0(" '", diff, "' has not been specified. It ")
    } else {
      message <- paste0(" '", paste(diff, collapse = "', '"), "' have not been specified. They ")
    }
    
    if (frommlx) {
      message <- paste0(message, "will be set to the value estimated by Monolix.")
    } else {
      message <- paste0(message, "will be set to 1.")
    }
    warning(message, call. = F)
  }
  return(ismissing)
}

.checkExtraParameters <- function(parameter, expectedParameters) {
  diff <- setdiff(parameter, expectedParameters)
  isextra <- FALSE
  if (length(diff)) {
    isextra <- TRUE
    if (length(diff) == 1) {
      warning("Found extra parameters. '", diff, "' is not in the model.", call. = F)
    } else {
      warning("Found extra parameters. '", paste(diff, collapse = "', '"), "' are not in the model.", call. = F)
    }
  }
  return(isextra)
}

#*******************************************************************************
# INPUT LIST MANAGEMENT
#*******************************************************************************

################################################################################
# Check if the inputList
# - is a list of vector
# - all the mandatoryNames are defined
# - all the named used in the inputList are in the mandatoryNames or defaultNames
################################################################################
.checkUnitaryList <- function(inputList, mandatoryNames=c(), defaultNames=c(), listName="list"){
  if (is.null(inputList)) {
    return(inputList)
  }

  # check that inputList is a list
  if (!(is.data.frame(inputList) | is.vector(inputList))) {
    stop("Invalid ", listName, ". It must be a vector with at least the following fields: ", paste(mandatoryNames, collapse = ', '), ".", call. = F)
  }

  # Check that all the defined elements are vectors
  namesList <- names(inputList)
  for (indexName in seq_along(namesList)) {
    if (! (is.list(inputList[[indexName]]) || is.vector(inputList[[indexName]]) || is.factor(inputList[[indexName]]))) {
      stop("Invalid field ", namesList[indexName], " in '", listName, "'. It must be a vector.", call. = F)
    }
  }

  if (!is.null(mandatoryNames)) {
    # Check that all the elements in the mandatory name are defined
    missingName <- setdiff(mandatoryNames, namesList)
    if (length(missingName) > 0) {
      message <- paste0("Mandatory fields are missing in '", listName, "', ")
      if (length(missingName) == 1) {
        message <- paste0(message, "'", missingName,"' is not defined. \n")
      }else{
        message <- paste0(message, "('", paste(missingName, collapse = "', '"), "') are not defined. \n")
      }
      message <- paste0(message, "Skip '", listName, "'. ")
      if (grepl("output", listName) & any(missingName %in% c("name", "time"))) {
        message <- paste0(message, "If it is a parameter, note that simulx function now returns all parameters by default.")
      }
      warning(message, call. = F)
      inputList <- NULL
    }
  }

  # Check that all the elements in the list are either in the mandatory or default list
  extraName <- setdiff(namesList, union(mandatoryNames,defaultNames))
  if (!is.null(inputList) && length(extraName) > 0) {
    message <- paste0("Invalid fields will be ignored in '", listName, "'.")
    if( length(extraName) == 1){
      message <- paste0(message, " ", extraName," will be ignored.")
    }else{
      message <- paste0(message, " (", paste(extraName, collapse = ','),") will be ignored.")
    }
    warning(message, call. = F)
    inputList <- inputList[! namesList %in% extraName]
  }

  return(inputList)
}

#*****************************************************************************************************************************************
# TRT MANAGEMENT
#*****************************************************************************************************************************************
###################################################################################
# Check if the treatment
# - if the type is a integer equals 1 by default
# - the amount is a vector or a scalar with the same number of values as time
# - everything is defined as infusion time
# - infusion time is removed if the value is 0
# - infusion time are positive
###################################################################################
.checkUnitaryTreatment<- function(treatment, listName = "treatment"){
  mandatoryNames <- c("time", "amount")
  defaultNames <- c("tinf", "rate", "type", "repeats", "probaMissDose", "washout")
  
  # replace adm name by type
  names(treatment)[names(treatment) == "adm"] <- "type"
  
  # replace amt by amount
  names(treatment)[names(treatment) == "amt"] <- "amount"
  
  if (is.data.frame(treatment)) {
    # add id as mandatory name if data is a dataframe
    indexID <- which(names(treatment) == 'id')
    if (length(indexID)) mandatoryNames <- c(mandatoryNames, names(treatment)[indexID])
    # remove repeats and probaMissDose as default name if data is a dataframe
    # defaultNames <- setdiff(defaultNames, c("repeats"))
    defaultNames <- names(treatment)
  }
  
  # error if target in treatment
  if (is.element("target", names(treatment))) {
    stop("Invalid ", listName, " target. You must use 'adm=' instead.", call. = FALSE)
  }

  # check names in treatement
  treatment <- .checkUnitaryList(
    treatment,
    mandatoryNames = mandatoryNames,
    defaultNames = defaultNames,
    listName = listName
  )

  if (!is.null(treatment)) {
    # check if the type is an integer
    if (is.element("type", names(treatment))) {
      .check_strict_pos_integer(treatment$type, paste(listName, "type"))
    } else{
      treatment$type = 1
    }
    
    # check if treatment times are double
    .check_vector_of_double(treatment$time, paste(listName, "time"))
    nbTimePoint = length(treatment$time)
    
    # check if treatment amounts are double
    .check_vector_of_double(treatment$amount, paste(listName, "amount"))
  
    # check that treatment amount vector and treatment times vector have the same length
    # (when length of treatment amount is different from 1)
    if (length(treatment$amount) > 1) {
      .check_vectors_length(treatment$amount, treatment$time, paste(listName, "amount"), paste(listName, "time"))
    } else {
      treatment$amount <- rep(treatment$amount, nbTimePoint)
    }

    # check that treatment type vector and treatment times vector have the same length
    # (when length of treatment type is different from 1)
    if (length(treatment$type) > 1) {
      .check_vectors_length(treatment$type, treatment$time, paste(listName, "type"), paste(listName, "time"))
    }
    
    # check that when it exists, treatment rate is a vector of strictly positive double
    if (is.element("rate", names(treatment))) {
      .check_vector_of_pos_double(treatment$rate, paste(listName, "rate"))
  
      if (length(treatment$rate) > 1) {
        .check_vectors_length(treatment$rate, treatment$time, paste(listName, "rate"), paste(listName, "time"))
      } else {
        treatment$rate <- rep(treatment$rate, nbTimePoint)
      }
    }
    
    # check that when it exists, treatment washout is a vector of strictly positive double
    if (is.element("washout", names(treatment))) {
      .check_vector_of_bool(treatment$washout, paste(listName, "washout"))
      
      if (length(treatment$washout) > 1) {
        .check_vectors_length(treatment$washout, treatment$time, paste(listName, "washout"), paste(listName, "time"))
      } else {
        treatment$washout <- rep(treatment$washout, nbTimePoint)
      }
      
      if (is.data.frame(treatment)) {
        treatment$washout <- as.integer(treatment$washout)
      } else {
        treatment$washout <- as.logical(treatment$washout)
      }
    }
    
    # check that when it exists, treatment infusion duration is a vector of positive double
    if (is.element("tinf", names(treatment))) {
      .check_vector_of_pos_double(treatment$tinf, paste(listName, "infusion duration"))
  
      if (length(treatment$tinf) > 1) {
        .check_vectors_length(treatment$tinf, treatment$time, paste(listName, "infusion duration"), paste(listName, "time"))
      } else{
        treatment$tinf <- rep(treatment$tinf, nbTimePoint)
      }
    }
    
    # check that when it exists, probaMissDose is a double in [0, 1]
    if (is.element("probaMissDose", names(treatment))) {
      .check_double(treatment$probaMissDose, paste(listName, "probaMissDose"))
      .check_in_range(treatment$probaMissDose, paste(listName, "probaMissDose"), 0, 1)
    }
    
    # check that when it exists, repeats is a vector with cycleDuration and NumberOfRepetitions
    if (is.element("repeats", names(treatment)) && ! is.data.frame(treatment)) {
      if (! all(c("cycleDuration", "NumberOfRepetitions") %in% names(treatment$repeats))) {
        stop("Invalid ", paste(listName, "repeats"), ". It should be a vector specifiying fields cycleDuration and NumberOfRepetition.", call.=F)
      }
      .check_pos_integer(treatment$repeats[["NumberOfRepetitions"]], paste(listName, "repeats, NumberOfRepetitions"))
      .check_pos_double(treatment$repeats[["cycleDuration"]], paste(listName, "repeats, cycleDuration"))
      
    }
  } else {
    treatment <- as.list(treatment)
  }
  return(treatment)
}

################################################################################
# Check if the treatment
# - if the type is a integer
# - the amount is a vector or a scalar with the same number of values as time
# - everything is defined as infusion time
# - infusion time is removed if the value is 0
# - infusion time are positive
################################################################################
.checkTreatment <- function(treatment, mlxProject=F){
  # When only one treatment --> create a list of treatment
  if (is.string(treatment) | is.element("time", names(treatment))) {
    treatment <- list(treatment)
  }
  for (itreat in seq_along(treatment)) {
    treatementValue <- treatment[[itreat]]

    if (length(treatment) > 1) {
      treatName <- paste0("treatment ", itreat)
    } else {
      treatName <- "treatment"
    }

    if (is.string(treatementValue)) {
      # parameter path to dataframe
      if (file.exists(treatementValue)) {
        .checkExtension(.getFileExt(treatementValue), paste(treatName, "file extension"))
        treatementValue <- utils::read.table(file=treatementValue, header=T, sep=.getDelimiter(treatementValue))

      } else if (grepl("^mlx_Adm[0-9]", treatementValue)) {
        if (! mlxProject) {
          stop("Invalid ", treatName, ". Monolix treatment can only be used when a project is imported from monolix.", call. = F)
        }
      } else {
        stop("Invalid ", treatName, ". Treatments defined as string must be either a path to a csv or txt file or ",
             "a treatment imported from monolix with the form 'mlx_Adm'.",
             call.=F)
      }
    }
    
    if (! is.string(treatementValue)) {
      treatment[[itreat]] <- .checkUnitaryTreatment(treatementValue, listName=treatName)
    }
  }
  treatment <- treatment[sapply(treatment, function(e) length(e) > 0)]
  return(treatment)
}

#*******************************************************************************
# OCCASION MANAGEMENT
#*******************************************************************************
.checkOccasion <- function(occasion, occName="occasion"){
  if (is.null(occasion)) return(occasion)

  if (is.string(occasion)) {
    if (occasion == "none") return(occasion)

    # occasion path to dataframe
    if (file.exists(occasion)) {
      .checkExtension(.getFileExt(occasion), paste(occName, "file extension"))
      occasion <- utils::read.table(file=occasion, header=T, sep=.getDelimiter(occasion))
    } else {
      stop("Invalid ", occName, ". Occasions defined as string must be valid path to a csv or txt file.",
           call.=F)
    }
  }
  if (is.data.frame(occasion)) {
    if (! is.element("time", names(occasion))) {
      stop("When occasion is a dataframe, it must contain a time column.", call. = FALSE)
    }
    .check_vector_of_double(occasion$time, paste(occName, "time"))
  } else {
    occasion <- .checkUnitaryList(
      occasion,
      mandatoryNames=c("name", "time"),
      listName=occName
    )
    if (is.null(occasion)) return(occasion)

    occname <- occasion$name
    .check_char(occname, paste(occName, "name"))
    if (length(occname) > 1) {
      stop("Invalid occasion. If you want to define several occasions, use a dataframe.", call.=F)
    }
    .check_vector_of_double(occasion$time, paste(occName, "time"))
    occasion <- data.frame(time=occasion$time)
    occasion[occname] <- 1:length(occasion$time)
  }
  return(occasion)
}

#*****************************************************************************************************************************************
# OUTPUT MANAGEMENT
#*****************************************************************************************************************************************

################################################################################
# Check if the output
# - have string for names
# - have double for times
################################################################################
.checkUnitaryOutput<- function(output, listName = "output"){
  mandatoryNames <- c("name", "time")
  defaultNames <- c("lloq", "uloq", "limit")
  if (!is.null(output)) {
    output <- .checkUnitaryList(
      output,
      mandatoryNames = mandatoryNames,
      defaultNames = defaultNames,
      listName = listName
    )
    if (is.null(output)) return(as.list(output))

    # check that output names are string
    .check_vector_of_char(output$name, paste(listName, "name"))

    # check that output times are a string, a dataframe, or a double
    outputTime <- output$time
    if (is.string(outputTime)) {
      # parameter path to dataframe
      if (file.exists(outputTime)) {
        .checkExtension(.getFileExt(outputTime), paste(listName, "file extension"))
        outputTime <- utils::read.table(file=outputTime, header=T, sep=.getDelimiter(outputTime))
      } else {
        stop("Invalid ", listName, ". Outputs defined as string must be valid path to a csv or txt file.",
             call.=F)
      }
    }
    if (is.data.frame(outputTime)) {
      if (! is.element("time", names(outputTime))) {
        stop("When output time is a dataframe, it must contain at least a time column.", call. = FALSE)
      }
      .check_vector_of_double(outputTime$time, paste(listName, "time"))
    } else {
      if (length(outputTime) > 1) {
        .check_vector_of_double(outputTime, paste(listName, "time"))
      } else {
        .check_vector_of_double(outputTime, paste(listName, "time"))
      }
    }
    output$time <- outputTime
  }
  if (is.null(output)) {
    output <- as.list(output)
  }
  return(output)
}

################################################################################
# Check if the output
# - have string for names
# - have double for times
################################################################################
.checkOutput <- function(output, mlxProject=F){
  # When only one output --> create a list of output
  if (is.string(output) || is.element("name", names(output))) {
    output <- list(output)
  }
  
  for (iout in seq_along(output)) {
    outputValue <- output[[iout]]
    if (length(output) > 1) {
      outName <- paste0("output ", iout)
    } else {
      outName <- "output"
    }
    
    if (is.string(outputValue)) {
      if (grepl("^mlx_", outputValue)) {
        if (! mlxProject) {
          stop("Invalid ", outName, ". Monolix output can only be used when a project is imported from monolix.", call. = F)
        }
      } else {
        stop("Invalid ", outName, ". Outputs defined as string must be ",
             "an output imported from monolix with the form 'mlx_'.", call.=F)
      }
    } else {
      output[[iout]] <- .checkUnitaryOutput(outputValue, listName = outName)
    }
  }
  output <- output[sapply(output, function(e) length(e) > 0)]
  if (!length(output)) output <- NULL
  return(output)
}

################################################################################
# Check if the output filename
# - does not exist
# - has a valid extension
################################################################################
.checkOutputDirectory <- function(directory) {
  if(!is.null(directory)){
    if(!dir.exists(directory))
      stop("Directory '", directory, "' does not exist.", call. = FALSE)
  }
  return(invisible(TRUE))
}

################################################################################
# Check if the delimiter is valid
# Valid delimiters are "\t", " ", ";", ","
################################################################################
.checkDelimiter <- function(delimiter, argname) {
  if (is.null(delimiter)) return(invisible(TRUE))
  if (! is.element(delimiter, c("\t", " ", ";", ",")))
    stop("'", argname, "' must be one of: {\"",
         paste(c("\\t", " ", ";", ","), collapse="\", \""), "\"}.",
         call. = FALSE)
  return(invisible(TRUE))
}

###################################################################################
# Check if the extension is valid
# Valid extensions are "csv", "txt"
###################################################################################
.checkExtension <- function(ext, argname) {
  if (is.null(ext)) return(invisible(TRUE))
  if (! is.element(ext, c("csv", "txt")))
    stop("'", argname, "' must be one of: {\"",
         paste(c("csv", "txt"), collapse="\", \""), "\"}.",
         call. = FALSE)
  return(invisible(TRUE))
}

#*****************************************************************************************************************************************
# REGRESSOR MANAGEMENT
#*****************************************************************************************************************************************

################################################################################
# Check if the regressor
# - have string for name, and only one name
# - have double for time
# - have double for value
################################################################################
.checkUnitaryRegressor<- function(regressor, listName = "regressor") {
  mandatoryNames <- c('name', 'time', 'value')
  defaultNames <- NULL
  if (is.data.frame(regressor)) {
    # add id as mandatory name if data is a dataframe
    indexID <- which(names(regressor) == 'id')
    mandatoryNames <- c("time")
    if (length(indexID)) defaultNames <- c(names(regressor)[indexID], names(regressor))
  }

  regressor <- .checkUnitaryList(
    regressor,
    mandatoryNames = mandatoryNames,
    defaultNames = defaultNames,
    listName = listName
  )
  if (is.null(regressor)) return(as.list(regressor))
  
  # check that regressor name is a string
  if (is.element("name", names(regressor))) {
    if (length(regressor$name) > 1)
      stop("The regressor name must have only one value.", call. = F)
    .check_char(regressor$name, paste(listName, "name"))
  }

  # check that regressor values are double
  if(is.element("value", names(regressor))) {
    .check_vector_of_double(regressor$value, paste(listName, "value"))
  }

  # check that regressor times are double
  if (is.element("time", names(regressor))) {
    .check_vector_of_double(regressor$time, paste(listName, "time"))
  }
  
  if (all(c("time", "value") %in% names(regressor))) {
    .check_vectors_length(regressor$time, regressor$value, paste(listName, "time"), paste(listName, "value"))
  }

  if (is.null(regressor)) regressor <- as.list(regressor)
  return(regressor)
}

################################################################################
# check if the regressor
# - have string for name, and only one name
# - have double for time
# - have double for value
################################################################################
.checkRegressor <- function(regressor){
  # When only one regressor --> create a list of regressor
  if (is.string(regressor) | is.data.frame(regressor) | is.element("name", names(regressor))) {
    regressor <- list(regressor)
  }
  for (ireg in seq_along(regressor)) {
    regValue <- regressor[[ireg]]
    if (length(regressor) > 1) {
      listName <- paste0("regressor ", ireg)
    } else {
      listName <- "regressor"
    }

    if (is.string(regValue)) {
      # regressor path to dataframe
      if (file.exists(regValue)) {
        .checkExtension(.getFileExt(regValue), paste(listName, "file extension"))
        regValue <- utils::read.table(file=regValue, header=T, sep=.getDelimiter(regValue))
      } else {
        stop("Invalid ", listName, ". Regressors defined as string must be valid path to a csv or txt file.",
             call.=F)
      }
    }
    regressor[[ireg]] <- .checkUnitaryRegressor(regValue, listName=listName)
  }
  regressor <- regressor[sapply(regressor, function(e) length(e) > 0)]
  return(regressor)
}

#*****************************************************************************************************************************************
# GROUP MANAGEMENT
#*****************************************************************************************************************************************

################################################################################
# Check if the group
# - has a size. Else wise the size is 1
# - parameter is a valid parameter,
# - output is a valid output,
# - treatment is a valid treatment,
# - regressor is a valid regressor
################################################################################
.checkUnitaryGroup <- function(group, listName, mlxProject=F){
  nbID <- 1

  # check if the size is an integer
  if (is.element("size", names(group))) {
    if (length(group$size) > 1) {
      stop("group size must a strictly positive integer of size 1", call. = F)
    }
    .check_strict_pos_integer(group$size, "treatment size")
  }
  
  # check that parameter is a good parameter
  if (is.element("parameter", names(group))) {
    group$parameter <- .transformParameter(group$parameter)
    group$parameter <- .checkParameter(parameter=group$parameter, mlxProject=mlxProject)
    # Write the data set as a .txt file
    if (is.data.frame(group$parameter)) {
      group$parameter <- .addDataFrameTemp(df=group$parameter)
    }
    # update the size of the default group
    if (is.string(group$parameter) && file.exists(group$parameter)) {
      df <- utils::read.table(file=group$parameter, header=T, sep=.getDelimiter(group$parameter))
      nbID <- max(nbID, .getNbIds(df))
    }
  }

  # check that covariate is a good covariate
  if (is.element("covariate", names(group))) {
    group$covariate <- .transformParameter(group$covariate)
    group$covariate <- .checkCovariate(covariate=group$covariate, mlxProject=mlxProject)
    # Write the data set as a .txt file
    if (is.data.frame(group$covariate)) {
      group$covariate <- .addDataFrameTemp(df=group$covariate)
    }
    # update the size of the default group
    if (is.string(group$covariate) && file.exists(group$covariate)) {
      df <- utils::read.table(file=group$covariate, header=T, sep=.getDelimiter(group$covariate))
      nbID <- max(nbID, .getNbIds(df))
    }
  }

  # check that output is a good output
  if (is.element("output", names(group))) {
    group$output <- .checkOutput(output = group$output, mlxProject=mlxProject)
    for (iout in seq_along(group$output)) {
      outputValue <- group$output[[iout]]
      if (is.data.frame(outputValue$time)) {
        dfout <- outputValue$time
        nbID <- max(nbID, .getNbIds(dfout))
        # Write the data set as a .txt file
        group$output[[iout]]$time <- .addDataFrameTemp(df = dfout)
      }
    }
  }

  # check that treatment is a good treatment
  if (is.element("treatment", names(group))) {
    group$treatment <- .checkTreatment(treatment=group$treatment, mlxProject=mlxProject)
    group$treatment <- .splitTreatment(group$treatment)

    # if dataframe, save it in a txt file
    for (itreat in seq_along(group$treatment)) {
      treatementValue <- group$treatment[[itreat]]
      if (is.data.frame(treatementValue)) {
        # Write the data set as a .txt file
        nbID <- max(nbID, .getNbIds(treatementValue))
        group$treatment[[itreat]] <-.addDataFrameTemp(df = treatementValue)
      }
    }
  }
  
  # check that regressor is a good regressor
  if (is.element("regressor", names(group))) {
    group$regressor <- .checkRegressor(regressor = group$regressor)
    group$regressor <- .transformRegressor(group$regressor)
    # When regressor is defined as dataframe, write the data set as a .txt file
    if (is.data.frame(group$regressor)) {
      nbID <- max(nbID, .getNbIds(group$regressor))
      group$regressor <- .addDataFrameTemp(df = group$regressor)
    }
  }
  
  group$nbID <- nbID
  if (is.null(group$size) && nbID > 1) {
    group$size <- nbID
  }
  return(group)
}

################################################################################
# Check if the group
# - has a size. Else wise the size is 1
# - parameter is a valid parameter,
# - output is a valid output,
# - treatment is a valid treatment,
# - regressor is a valid regressor
################################################################################
.checkGroup <- function(group, defaultArguments, mlxProject=F){
  # When only one group --> create a list of group
  allowedNames <- c("size", "parameter", "covariate", "output", "treatment", "regressor", "level")
  if (any(is.element(allowedNames, names(group))) | !all(sapply(group, .is_list_or_named_vector))) {
    group <- list(group)
  }
  
  # Check if elements defined in main and groups
  for (element in c("parameter", "covariate", "output", "treatment", "regressor")) {
    elementInGroup <- any(sapply(group, function(g) element %in% names(g)))
    
    if (elementInGroup & defaultArguments[[element]]) {
      warning(element, " has been defined in both shared group and groups. ",
              element, " defined in shared group will be erased.", call.=F)
    }
  }
  
  for (igroup in seq_along(group)) {
    gValue <- group[[igroup]]
    if (length(group) > 1) {
      listName <- paste0("group ", igroup)
    } else {
      listName <- "group"
    }
    group[[igroup]] <- .checkUnitaryGroup(gValue, listName=listName, mlxProject=mlxProject)
  }
  return(group)
}

################################################################################
# Check simpop inputs
################################################################################
# Check simpop parameter argument ----------------------------------------------
.checkSimpopParameter <- function(parameter) {
  if (is.null(parameter)) return(parameter)
  if (!is.data.frame(parameter)) stop("parameter must be a dataframe object.", call. = FALSE)
  # check parameter values
  .check_in_vector(names(parameter), "'parameter names'", c("pop.param", "sd", "trans", "lim.a", "lim.b"))
  if (! all(is.element(c("pop.param", "sd"), names(parameter))))
    stop("You must specified at least 'pop.param' and 'sd' in 'parameter' argument")
  
  paramName <- row.names(parameter)

  if (is.null(parameter$trans)) {
    parameter$trans <- "N"
    i.omega <- c(grep("^omega_", paramName), grep("^omega2_", paramName))
    i.corr <- unique(c(grep("^r_", paramName), grep("^corr_", paramName)))
    parameter$trans[i.omega] <- "L"
    parameter$trans[i.corr] <- "R"
  }
  .check_in_vector(parameter$trans, "'parameter trans'", c("N", "L", "G", "P", "R"))
  
  # lim.a & lim.b
  if (is.null(parameter$lim.a)) {
    parameter$lim.a <- NA
    parameter$lim.a[parameter$trans == "G"] <- 0
  }
  if (is.null(parameter$lim.b)) {
    parameter$lim.b <- NA
    parameter$lim.b[parameter$trans == "G"] <- 1
  }
  if (! all(is.na(parameter[parameter$trans != "G", c("lim.a", "lim.b")])))
    stop("lim.a and lim.b must be specified for logit transformations only (trans = G). For other transformations, set lim.a and lim.b to NaN.", call. = FALSE)
  lima <- parameter[parameter$trans == "G",]$lim.a
  limb <- parameter[parameter$trans == "G",]$lim.b
  # check lim.a < lim.b
  if (!all(lima < limb))
    stop("lim.a must be strictly inferior to lim.b.", call. = FALSE)
  
  # check pop.param values
  # normal distribution
  parameter$lim.a[parameter$trans == "N"] <- - Inf
  parameter$lim.b[parameter$trans == "N"] <- Inf
  # lognormal distribution: pop.param > 0
  .check_in_range(parameter$pop.param[parameter$trans == "L"], "pop.param of lognormal distribution",
                  lowerbound = 0, includeBound = FALSE)
  parameter$lim.a[parameter$trans == "L"] <- 0
  parameter$lim.b[parameter$trans == "L"] <- Inf
  # logit distribution: pop.param in ]lim.a, lim.b[
  .check_in_range(parameter$pop.param[parameter$trans == "G"], "pop.param of logit distribution",
                  lowerbound = parameter$lim.a[parameter$trans == "G"],
                  upperbound = parameter$lim.b[parameter$trans == "G"],
                  includeBound = FALSE)
  # r distribution: pop.param in [-1, 1]
  .check_in_range(parameter$pop.param[parameter$trans == "R"], "pop.param of r distribution",
                  lowerbound = -1, upperbound = 1, includeBound = TRUE)
  parameter$lim.a[parameter$trans == "R"] <- -1
  parameter$lim.b[parameter$trans == "R"] <- 1
  # probit distribution: pop.param in [0, 1]
  .check_in_range(parameter$pop.param[parameter$trans == "P"], "pop.param of probit normal distribution",
                  lowerbound = 0, upperbound = 1, includeBound = TRUE)
  parameter$lim.a[parameter$trans == "P"] <- 0
  parameter$lim.b[parameter$trans == "P"] <- 1
  
  return(parameter)
    
}

# Check simpop corr argument ---------------------------------------------------
.checkSimpopCorr <- function(corr, nbParams) {
  # corr matrix size must be (nbParams x nbParams)
  # if (!is.matrix(corr) | !is.data.frame(corr))
  #   stop("`corr` must be a matrix or a dataframe of shape (", nbParams, " x ", nbParams, ").", call. = FALSE)
  if (nrow(corr) != nbParams | ncol(corr) != nbParams)
    stop("'corr' must be a matrix of shape (", nbParams, " x ", nbParams, ").", call. = FALSE)
  # corr elements must be in [-1, 1]
  .check_in_range(corr[! is.na(corr)], "corr elements", lowerbound = -1 - 1e-4, upperbound = 1 + 1e-4, includeBound = TRUE)
  return(corr)
}

# Check sharedIds argument -----------------------------------------------------
.checkSharedIds <- function(sharedIds, sharedElements) {
  for (element in sharedIds) {
    if (element != "output") {
      if (! any(sapply(sharedElements[[element]], is.string))) {
        warning("No id to share in ", element, " element.", call.=F)
        sharedIds <- sharedIds[sharedIds != element]
      }
    } else {
      if (! any(sapply(sharedElements[[element]], function(x) is.string(x$time)))) {
        warning("No id to share in ", element, " element.", call.=F)
        sharedIds <- sharedIds[sharedIds != element]
      }
    }
  }
  if (length(sharedIds) == 0) sharedIds <- NULL
  return(sharedIds)
}

# Check new additional line to add to the model --------------------------------
.checkAdditionalLine <- function(line) {
  if (is.element("section", names(line))) {
    if (! grepl("LONGITUDINAL", line$section))
      stop("'addlines' argument only support 'LONGITUDINAL' section and 'EQUATION' block.", call. = FALSE)
  }
  if (is.element("block", names(line))) {
    if (grepl("EQUATION", line))
      stop("'addlines' argument only support 'LONGITUDINAL' section and 'EQUATION' block.", call. = FALSE)
  }
  return(invisible(TRUE))
}

################################################################################
# Object types check functions
################################################################################
# Check project extension & add extension if needed
.checkProjectExtension <- function(project_file, software) {
  ext <- .getFileExt(project_file)
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
      stop("Invalid extension for a simulx project. smlx extension is expected.", call. = F)
    if (software == "monolix" & ext != "mlxtran")
      stop("Invalid extension for a Monolix project. mlxtran extension is expected.", call. = F)
  }
  return(project_file)
}

# Checks if a file exists and returns an error----------------------------------
.check_file <- function(filename, fileType = "File") {
  if (grepl("^lib:", filename)) return(filename)
  if (!file.exists(filename))
    stop(fileType, " ", filename, " does not exist.", call. = FALSE)
  filename <- normalizePath(filename)
  return(filename)
}

# Checks if an input is a strictly positive integer and returns an error ------
.check_integer <- function(int, argname = NULL) {
  if(!(is.double(int)||is.integer(int)))
    stop("Invalid ", argname, ". It must be an integer.", call. = F)
  if (!all(as.integer(int) == int))
    stop("Invalid ", argname, ". It must be an integer.", call. = F)
  return(int)
}  

# Checks if an input is a strictly positive integer and returns an error -------
.check_strict_pos_integer <- function(int, argname) {
  if(!(is.double(int)||is.integer(int)))
    stop("Invalid ", argname, ". It must be a strictly positive integer.", call. = F)
  if (any(int <= 0) || any(!as.integer(int) == int))
    stop("Invalid ", argname, ". It must be a strictly positive integer.", call. = F)
  return(int)
}

# Checks if an input is a strictly positive integer and returns an error -------
.check_pos_integer <- function(int, argname) {
  if(!(is.double(int)||is.integer(int)))
    stop("Invalid ", argname, ". It must be a positive integer.", call. = F)
  if (any(int < 0) || any(!as.integer(int) == int))
    stop("Invalid ", argname, ". It must be a positive integer.", call. = F)
  return(int)
}

# Checks if an input is a double and returns an error --------------------------
.check_double <- function(d, argname) {
  if(!(is.double(d)||is.integer(d)))
    stop("Invalid ", argname, ". It must be a double.", call. = F)
  return(d)
}

# check if a vector contains only doubles
.check_vector_of_double <- function(v, argname) {
  if(!(is.double(v)||is.integer(v)))
    stop("Invalid ", argname, ". It must be a vector of doubles.", call. = F)
  return(v)
}

# Checks if an input is a positive double and returns an error --------
.check_pos_double <- function(d, argname) {
  if(!(is.double(d)||is.integer(d)) || any(d < 0))
    stop("Invalid ", argname, ". It must be a positive double.", call. = F)
  return(d)
}

# check if a vector contains only positive doubles
.check_vector_of_pos_double <- function(v, argname) {
  if(!(is.double(v)||is.integer(v)) || any(v < 0))
    stop("Invalid ", argname, ". It must be a vector of positive doubles.", call. = F)
  return(v)
}

# Checks if an input is a strictly positive double and returns an error --------
.check_strict_pos_double <- function(d, argname) {
  if(!(is.double(d)||is.integer(d)) || any(d <= 0))
    stop("Invalid ", argname, ". It must be a strictly positive double.", call. = F)
  return(d)
}

# check if a vector contains only strictly positive doubles
.check_vector_of_strict_pos_double <- function(v, argname) {
  if(!(is.double(v)||is.integer(v)) || any(v <= 0))
    stop("Invalid ", argname, ". It must be a vector of strictly positive doubles.", call. = F)
  return(v)
}

# Checks if an input is a string -----------------------------------------------
.check_char <- function(str, argname = NULL) {
  if (!is.character(str)) {
    stop("Invalid ", argname, ". It must be a string", call. = F)
  }
  return(str)
}

# check if a vector contains only char
.check_vector_of_char <- function(v, argname) {
  if (!is.character(v)) {
    stop("Invalid ", argname, ". It must be a vector of strings", call. = F)
  }
  return(v)
}

# Checks if an input is boolean  -----------------------------------------------
.check_bool <- function(bool, argname) {
  if (!is.logical(bool))
    stop("Invalid ", argname, ". It must be logical.", call. = F)
  return(bool)
}

.check_vector_of_bool <- function(v, argname) {
  if (! (is.logical(v) || all(v %in% c(0, 1)))) {
    stop("Invalid ", argname, ". It must be a vector of boolean.", call. = F)
  }
  return(v)
}

# check if 2 vectors have the same size
.check_vectors_length <- function(v1, v2, argname1, argname2) {
  if (length(v1) != length(v2))
    stop(argname1, " vector and ", argname2, " vector must have the same length.", call. = F)
  return(invisible(TRUE))
}

# Check if an element is allowed -----------------------------------------------
.check_in_vector <- function(arg, argname, vector) {
  if (is.null(arg)) return(invisible(TRUE))
  if (! all(is.element(arg, vector))) {
    stop(argname, " must be in {'", paste(vector, collapse="', '"), "'}.", call. = F)
  }
  return(invisible(TRUE))
}

# Check if an element is in a range --------------------------------------------
.check_in_range <- function(arg, argname, lowerbound = -Inf, upperbound = Inf, includeBound = TRUE) {
  if (includeBound) {
    if (! all(arg >= lowerbound) | ! all(arg <= upperbound)) {
      stop(argname, " must be in [", lowerbound, ", ", upperbound, "].", call. = F)
    }
  } else {
    if (! all(arg > lowerbound) | ! all(arg < upperbound)) {
      stop(argname, " must be in ]", lowerbound, ", ", upperbound, "[.", call. = F)
    }
  }
  return(invisible(TRUE))
}

# return TRUE if an object is a string
is.string <- function(str) {
  isStr <- TRUE
  if (!is.null(names(str))) {
    isStr <- FALSE
  } else if (length(str) > 1) {
    isStr <- FALSE
  } else if (! is.character(str)) {
    isStr <- FALSE
  }
  return(isStr)
}
