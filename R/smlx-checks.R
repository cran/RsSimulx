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
        "See `http://simulx.lixoft.com/definition/model/` for model definition.",
        call. = F
      )
    }
    if (any(grepl("P\\(([a-zA-Z0-9]|\\s|=)*\\)", covsection))) {
      stop(
        "Invalid model file. Definition of distributions for covariates in the [COVARIATE] block is not supported anymore. \n",
        "Instead, generate the covariates in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See `http://simulx.lixoft.com/definition/model/` for model definition.",
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
        "See `http://simulx.lixoft.com/definition/model/` for model definition.",
        call. = F
      )
    }
    if (any(grepl("P\\(([a-zA-Z0-9]|\\s|=)*\\)", popsection))) {
      stop(
        "Definition of distributions for population parameters in the [POPULATION] block is not supported anymore. \n",
        "Instead, generate the population parameters in your R script and pass them as a data.frame to the 'parameter' argument of simulx. \n",
        "See `http://simulx.lixoft.com/definition/model/` for model definition.",
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
  lines <- suppressWarnings(readLines(con = fileName, n = -1))
  bIsOUT = F
  for(index in 1: length(lines)){
    bIsOUT <- bIsOUT|grepl(x =lines[index], pattern = 'OUTPUT:')
  }
  return(bIsOUT)
}

################################################################################
# Check the parameters
################################################################################
.checkParameter <- function(parameter){
  if(!is.null(parameter)){
    if(!(is.vector(parameter)||(is.data.frame(parameter))))
      stop("Invalid paramerer. It must be a vector or a data.frame.", call. = F)
  }
  return(parameter)
}

.checkMissingParameters <- function(parameter, expectedParameters, frommlx = FALSE) {
  diff <- setdiff(expectedParameters, parameter)
  ismissing <- FALSE
  if (length(diff)) {
    ismissing <- TRUE
    if (length(diff) == 1) {
      message <- paste0(" '", diff, "` has not been specified. It ")
    } else {
      message <- paste0(" '", paste(diff, collapse = "`, `"), "` have not been specified. They ")
    }
    
    if (frommlx) {
      message <- paste0(message, "will be set to the value estimated by mononlix.")
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
      warning("Found extra parameters. '", diff, "` is not in the model.", call. = F)
    } else {
      warning("Found extra parameters. '", paste(diff, collapse = "`, `"), "` are not in the model.", call. = F)
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
.checkUnitaryList <- function(inputList, mandatoryNames, defaultNames, listName){
  if (!is.null(inputList)) {
    # check that inputList is a list
    if (!(is.data.frame(inputList) | is.vector(inputList))) {
      stop("Invalid ", listName, ". It must be a vector with at least the following fields: ", paste(mandatoryNames, collapse = ', '), ".", call. = F)
    }

    namesList <- names(inputList)
    # Check that all the defined elements are vectors
    for (indexName in seq_along(namesList)) {
      # if (is.list(inputList[[indexName]]) || ! is.vector(inputList[[indexName]]))
      #   stop("Invalid field ", namesList[indexName], " in `", listName, "`. It must be a vector.", call. = F)
      if (! (is.list(inputList[[indexName]]) || is.vector(inputList[[indexName]]) || is.factor(inputList[[indexName]])))
        stop("Invalid field ", namesList[indexName], " in `", listName, "`. It must be a vector.", call. = F)
    }
    if (!is.null(mandatoryNames)) {
      # Check that all the elements in the mandatory name are defined
      missingName <- setdiff(mandatoryNames, namesList)
      if (length(missingName) > 0) {
        message <- paste0("Mandatory fields are missing in `", listName, "`, ")
        if (length(missingName) == 1) {
          message <- paste0(message, "`", missingName,"` is not defined. \n")
        }else{
          message <- paste0(message, "(`", paste(missingName, collapse = "`, `"),"`) are not defined. \n")
        }
        message <- paste0(message, "Skip '", listName, "`. ")
        if (grepl("output", listName) & any(missingName %in% c("name", "time"))) {
          message <- paste0(message, "If it is a parameter, note that simulx function now returns all parameters by default.")
        }
        warning(message, call. = F)
        inputList <- NULL
      }
    }
    # Check that all the elements in the list are either in the mandatory or default list
    extraName <- setdiff(namesList, union(mandatoryNames,defaultNames))
    if (length(extraName)>0) {
      message <- paste0("Invalid fields will be ignored in `", listName, "`.")
      if(length(extraName)==1){
        message <- paste0(message, " ", extraName," will be ignored.")
      }else{
        message <- paste0(message, " (", paste(extraName, collapse = ','),") will be ignored.")
      }
      warning(message, call. = F)
      inputList <- inputList[! namesList %in% extraName]
    }
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
  defaultNames <- c("tinf", "rate", "type")
  if (is.data.frame(treatment)) {
    # add id as mandatory name if data is a dataframe
    indexID <- which(names(treatment) == 'id')
    if (length(indexID)) mandatoryNames <- c(mandatoryNames, names(treatment)[indexID])
  }
  
  # replace adm name by type
  names(treatment)[names(treatment) == "adm"] <- "type"
  
  # replace amt by amount
  names(treatment)[names(treatment) == "amt"] <- "amount"
  
  # error if target in treatment
  if (is.element("target", names(treatment))) {
    stop("Invalid field `target` for `treatment` argument. You must use `adm=` instead.", call. = FALSE)
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
      .check_strict_pos_integer(treatment$type, "treatment type")
    } else{
      treatment$type = 1
    }
    
    # check if treatment times are double
    .check_vector_of_double(treatment$time, "treatment times")
    nbTimePoint = length(treatment$time)
    
    # check if treatment amounts are double
    .check_vector_of_double(treatment$amount, "treatment amount")
  
    # check that treatment amount vector and treatment times vector have the same length
    # (when length of treatment amount is different from 1)
    if (length(treatment$amount) > 1) {
      .check_vectors_length(treatment$amount, treatment$time, "treatment amount", "treatment times")
    } else {
      treatment$amount <- rep(treatment$amount, nbTimePoint)
    }

    # check that treatment type vector and treatment times vector have the same length
    # (when length of treatment type is different from 1)
    if (length(treatment$type) > 1) {
      .check_vectors_length(treatment$type, treatment$time, "treatment type", "treatment times")
    }
    
    # check that when it exists, treatment rate is a vector of strictly positive double
    if (is.element("rate", names(treatment))) {
      .check_vector_of_pos_double(treatment$rate, "treatment rate")
  
      if (length(treatment$rate) > 1) {
        .check_vectors_length(treatment$rate, treatment$time, "treatment rate", "treatment times")
      } else {
        treatment$rate <- rep(treatment$rate, nbTimePoint)
      }
    }
    
    # check that when it exists, treatment infusion duration is a vector of positive double
    if (is.element("tinf", names(treatment))) {
      .check_vector_of_pos_double(treatment$tinf, "treatment infusion duration")
  
      if (length(treatment$tinf) > 1) {
        .check_vectors_length(treatment$tinf, treatment$time, "treatment infusion duration", "treatment times")
      } else{
        treatment$tinf <- rep(treatment$tinf, nbTimePoint)
      }
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
.checkTreatment <- function(treatment){
  # When only one treatment --> create a list of treatment
  if (is.element("time", names(treatment))) {
    treatment <- list(treatment)
  }
  
  for (itreat in seq_along(treatment)) {
    treatementValue <- treatment[[itreat]]
    if (length(treatment) > 1) {
      listName <- paste0("treatment ", itreat)
    } else {
      listName <- "treatment"
    }
    treatment[[itreat]] <- .checkUnitaryTreatment(treatementValue, listName = listName)
  }
  treatment <- treatment[sapply(treatment, function(e) length(e) > 0)]
  return(treatment)
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
    if (is.data.frame(output)) {
      # add id as mandatory name if data is a dataframe
      indexID <- which(names(output) == 'id')
      if (length(indexID)) defaultNames <- c(defaultNames, names(output)[indexID], names(output))
    }
    output <- .checkUnitaryList(
      output,
      mandatoryNames = mandatoryNames,
      defaultNames = defaultNames,
      listName = listName
    )

    # check that output names are string
    if (is.element("name", names(output)))
      .check_vector_of_char(output$name, "output name")

    # check that output times are double
    if (is.element("time", names(output))) {
      if (is.data.frame(output$time)) {
        if (! is.element("time", names(output$time))) {
          stop("When output time is a dataframe, it must contains at least a time column.", call. = FALSE)
        }
      } else {
        if (length(output$time) > 1) {
          .check_vector_of_double(output$time, "output time")
        } else {
          if (output$time != "none") .check_vector_of_double(output$time, "output time") 
        }
      }
    }
    # } else {
    #   output <- NULL
    #   #output$time = 0
    # }
  }
  if (is.null(output)) output <- as.list(output)
  return(output)
}

################################################################################
# Check if the output
# - have string for names
# - have double for times
################################################################################
.checkOutput <- function(output){
  # When only one output --> create a list of output
  if (is.data.frame(output) | is.element("name", names(output))) {
    output <- list(output)
  }
  for (iout in seq_along(output)) {
    outputValue <- output[[iout]]
    if (length(output) > 1) {
      listName <- paste0("output ", iout)
    } else {
      listName <- "output"
    }
    output[[iout]] <- .checkUnitaryOutput(outputValue, listName = listName)
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
    stop("`", argname, "` must be one of: {\"",
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
    stop("`", argname, "` must be one of: {\"",
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
  
  # check that regressor name is a string
  if (is.element("name", names(regressor))) {
    if (length(regressor$name) > 1)
      stop("The regressor name must have only one value.", call. = F)
    .check_char(regressor$name, "regressor name")
  }

  # check that regressor values are double
  if(is.element("value", names(regressor))) {
    .check_vector_of_double(regressor$value, "regressor value")
  }

  # check that regressor times are double
  if (is.element("time", names(regressor))) {
    .check_vector_of_double(regressor$time, "regressor time")
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
  if (is.data.frame(regressor) | is.element("name", names(regressor))) {
    regressor <- list(regressor)
  }
  for (ireg in seq_along(regressor)) {
    regValue <- regressor[[ireg]]
    if (length(regressor) > 1) {
      listName <- paste0("regressor ", ireg)
    } else {
      listName <- "regressor"
    }
    regressor[[ireg]] <- .checkUnitaryRegressor(regValue, listName = listName)
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
.checkUnitaryGroup<- function(group, listName){
  # check if the size is an integer
  if (is.element("size", names(group))) {
    if (length(group$size) > 1) {
      stop("group size must a strictly positive integer of size 1", call. = F)
    }
    .check_strict_pos_integer(group$size, "treatment size")
  } else {
    #group$size = 1
  }
  
  # check that parameter is a good parameter
  if (is.element("parameter", names(group))) {
    group$parameter <- .transformParameter(parameter=group$parameter)
    group$parameter <- .checkParameter(parameter=group$parameter)
  }

  # check that output is a good output
  if (is.element("output", names(group))) {
    group$output <- .checkOutput(output = group$output)
  }

  # check that treatment is a good treatment
  if (is.element("treatment", names(group))) {
    group$treatment <- .checkTreatment(treatment = group$treatment)
    group$treatment <- .splitTreatment(group$treatment)
  }
  
  # check that regressor is a good regressor
  if (is.element("regressor", names(group))) {
    group$regressor <- .checkRegressor(regressor = group$regressor)
    group$regressor <- .transformRegressor(group$regressor)
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
.checkGroup <- function(group){
  # When only one group --> create a list of group
  allowedNames <- c("size", "parameter", "output", "treatment", "regressor", "level")
  if (any(is.element(allowedNames, names(group))) | !all(sapply(group, .is_list_or_named_vector))) {
    group <- list(group)
  }
  for (igroup in seq_along(group)) {
    gValue <- group[[igroup]]
    if (length(group) > 1) {
      listName <- paste0("group ", igroup)
    } else {
      listName <- "group"
    }
    group[[igroup]] <- .checkUnitaryGroup(gValue, listName = listName)
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
  .check_in_vector(names(parameter), "`parameter names`", c("pop.param", "sd", "trans", "lim.a", "lim.b"))
  if (! all(is.element(c("pop.param", "sd"), names(parameter))))
    stop("You must specified at least `pop.param` and `sd` in `parameter` argument")
  
  paramName <- row.names(parameter)

  if (is.null(parameter$trans)) {
    parameter$trans <- "N"
    i.omega <- c(grep("^omega_", paramName), grep("^omega2_", paramName))
    i.corr <- unique(c(grep("^r_", paramName), grep("^corr_", paramName)))
    parameter$trans[i.omega] <- "L"
    parameter$trans[i.corr] <- "R"
  }
  .check_in_vector(parameter$trans, "`parameter trans`", c("N", "L", "G", "P", "R"))
  
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

# Check simpop corr argument ----------------------------------------------
.checkSimpopCorr <- function(corr, nbParams) {
  # corr matrix size must be (nbParams x nbParams)
  # if (!is.matrix(corr) | !is.data.frame(corr))
  #   stop("`corr` must be a matrix or a dataframe of shape (", nbParams, " x ", nbParams, ").", call. = FALSE)
  if (nrow(corr) != nbParams | ncol(corr) != nbParams)
    stop("`corr` must be a matrix of shape (", nbParams, " x ", nbParams, ").", call. = FALSE)
  # corr elements must be in [-1, 1]
  .check_in_range(corr[! is.na(corr)], "corr elements", lowerbound = -1 - 1e-4, upperbound = 1 + 1e-4, includeBound = TRUE)
  return(corr)
}

################################################################################
# Object types check functions
################################################################################
# Checks if a file exists and returns an error----------------------------------
.check_file <- function(filename, fileType = "File") {
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
  if ((int <= 0) || (!as.integer(int) == int))
    stop("Invalid ", argname, ". It must be a strictly positive integer.", call. = F)
  return(int)
}

# Checks if an input is a strictly positive integer and returns an error -------
.check_pos_integer <- function(int, argname) {
  if(!(is.double(int)||is.integer(int)))
    stop("Invalid ", argname, ". It must be a positive integer.", call. = F)
  if ((int < 0) || (!as.integer(int) == int))
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
  if(!(is.double(d)||is.integer(d)) || (d < 0))
    stop("Invalid ", argname, ". It must be a positive double.", call. = F)
  return(d)
}

# check if a vector contains only positive doubles
.check_vector_of_pos_double <- function(v, argname) {
  if(!(is.double(v)||is.integer(v)) || (v < 0))
    stop("Invalid ", argname, ". It must be a vector of positive doubles.", call. = F)
  return(v)
}

# Checks if an input is a strictly positive double and returns an error --------
.check_strict_pos_double <- function(d, argname) {
  if(!(is.double(d)||is.integer(d)) || (d <= 0))
    stop("Invalid ", argname, ". It must be a strictly positive double.", call. = F)
  return(d)
}

# check if a vector contains only strictly positive doubles
.check_vector_of_strict_pos_double <- function(v, argname) {
  if(!(is.double(v)||is.integer(v)) || (v <= 0))
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
    stop("Invalid ", argname, ". It must be logical", call. = F)
  return(bool)
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
    stop(argname, " must be in {`", paste(vector, collapse="`, `"), "`}.", call. = F)
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
