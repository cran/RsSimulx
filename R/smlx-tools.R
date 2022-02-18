#' Inline model
#'
#' Save a string in a temporary file to be used as a model file
#'
#' @param srtIn (\emph{string}) Model in string format,
#' @param filename (\emph{string}) name of the model file  (by default the model is saved in a temporary file)
#' @return Name of the model file
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
#' }
#' @export
inlineModel <- function(srtIn, filename = NULL){
  tempFile <- tempfile(pattern = 'Simulx_',fileext = '.txt')
  tempFile <- gsub(x = tempFile, pattern = '\\\\', replacement = "/")
  fileConn <- file(tempFile)
  writeLines(text = srtIn, con = fileConn, sep ='\n')
  close(fileConn)

  if (!is.null(filename)) {
    str_Filename <- paste0(filename,'.txt')
    file.copy(from = tempFile, to = str_Filename)
    return(str_Filename)
  } else {
    return(tempFile)
  }
}

#' Inline dataframe
#'
#' Convert a string in dataframe and save it in a temporary file
#'
#' @param str (\emph{string}) Dataframe in string format
#' @return dataframe object
#' @examples
#' \dontrun{
#' occ <- inlineDataFrame("
#'  id time occ
#'  1   0    1
#'  1   12   2
#'  1   24   3
#'  2   0    1
#'  2   24   2
#'  3   0    1
#' ")
#' }
#' @export
inlineDataFrame <- function(str){
  tempFile <- paste0(tempfile(),'.txt')
  tempFile <- gsub(x = tempFile, pattern = '\\\\', replacement = "/")

  indexStr = gregexpr(text = str, pattern = '\n')[[1]][1]
  if(indexStr ==1){
    str <- substr(x= str, start = 2, stop = nchar(str))
  }
  indexStr = max(gregexpr(text = str, pattern = '\n')[[1]])
  if(indexStr == nchar(str)){
    str <- substr(x= str, start = 1, stop = nchar(str)-1)
  }
  for(index in 1:10){
    str <- gsub(x= str, pattern = "  ", replacement = " ")
  }
  str <- gsub(x= str, pattern = "\n ", replacement = "\n")
  str <- gsub(x= str, pattern = " \n", replacement = "\n")
  indexStr = gregexpr(text = str, pattern = ' ')[[1]][1]
  if(indexStr ==1){
    str <- substr(x= str, start = 2, stop = nchar(str))
  }
  indexStr = max(gregexpr(text = str, pattern = ' ')[[1]])
  if(indexStr == nchar(str)){
    str <- substr(x= str, start = 1, stop = nchar(str)-1)
  }


  utils::write.table(x = str, file = tempFile, quote = F,
                     sep = ';', eol = '\n', row.names = F, col.names = F)

  df = utils::read.table(file = tempFile, header = T, sep = .getDelimiter(tempFile))
  return(df)
}


# get model file from model library
.getModelFile <- function(model) {
  if (!grepl( "^lib:", model)) return(model)
  lixoftPath <- .lixoftCall("getLixoftConnectorsState", list(quietly = TRUE))$path
  libPath <- file.path(lixoftPath, "factory", "library")
  models <- list.files(
    path = libPath,
    pattern = paste0("*", gsub("^lib:", "", model)),
    recursive = TRUE
  )
  if (length(models) == 0) {
    stop("Invalid model ", model, ". It is not in the model library.", call. = FALSE)
  }
  model <- file.path(libPath, models[1])
  return(model)
}

#*******************************************************************************
# OUTPUT MANAGEMENT
#*******************************************************************************
################################################################################
# .addUnitaryOutputName defines a name for the output
# - based on the group name
# - based on the output name
################################################################################
.addUnitaryOutputName <- function(output, groupName = NULL){
  return(paste0('Out_', groupName, '_', length(.lixoftCall("getOutputElements")) + 1))
}

################################################################################
# .addUnitaryOutput adds an output list to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addUnitaryOutput <- function(output, groupName = NULL){
  outputVect <- output
  outName_full <- NULL
  tt <-  outputVect$time
  
  occInfo <- .lixoftCall("getOccasionElements")
  if (length(occInfo$occasions) > 1) {
    occLevel <- occInfo$occasions
    occLevelName <- occInfo$name
    occLevelTime <- occInfo$time
    occLevelId <- occInfo$id
    
    # output defined as string
    if (is.string(tt)) {
      df_occ <- utils::read.table(file = tt, header = T, sep = .getDelimiter(tt))
      df_occ$time <- as.numeric(df_occ$time)
    } else {
      if (! is.null(occLevelId)) {
        ids <- unique(occLevelId)
        df_occ <- data.frame(id=rep(ids, each=length(tt)),
                             time=rep(tt, length(ids)))
      } else {
        df_occ <- data.frame(time=tt)
      }
    }

    matchOccNames <- unlist(sapply(occLevelName, function(name) grep(paste0("^", name, "$"), names(df_occ), ignore.case=T, value=T)))
    df_occ <- .renameColumns(df_occ, unname(matchOccNames), names(matchOccNames))
    
    # check treatment elements
    extraNames <- setdiff(names(df_occ),
                          c(occLevelName, "id", "time", "lloq", "uloq", "limit"))
    if (length(extraNames)) {
      warning("Invalid occasions found in output: '", paste0(extraNames, collapse="', '"),
              "' will be removed. Available occasions are '",
              paste(occLevelName, collapse="', '"), "'.", call.=F)
      df_occ <- df_occ[! names(df_occ) %in% extraNames]
    }

    for (o in seq_along(occLevelName)) {
      occname <- occLevelName[o]
      if (occname %in% names(df_occ)) next
      if (occname %in% .getOverlapOccasion()) next

      occasions <- sapply(occLevel, function(occ) occ[[o]])

      # levels same for all ids
      if (is.null(occLevelId)) {
        df_occ <- .fill_occasion(df_occ, occasions, occLevelTime, occname)
      } else {
        df = df_occ
        for (id in unique(occLevelId)) {
          if (! id %in% df$id) next
          df_occ_id <- .fill_occasion(df[df$id == id,],
                                      occasions[occLevelId == id],
                                      occLevelTime[occLevelId == id],
                                      occname)
          df_occ[df_occ$id == id, occname] <- df_occ_id[[occname]]
        }
      }
    }
    df_occ <- df_occ[intersect(c("id", occLevelName, "time"), names(df_occ))]
    if ("id" %in% names(df_occ)) {
      data <- .addDataFrameTemp(df_occ)
    } else {
      data <- df_occ
    }
  } else {
    if (is.string(outputVect$time)) {
      data <- outputVect$time
    } else {
      data <- data.frame(time = outputVect$time)
    }
  }
  for (indexName in seq_along(outputVect$name)) {
    outName <- .addUnitaryOutputName(output = outputVect$name[indexName], groupName)
    # catch error if output not defined
    if (is.string(data)) {
      messageMatch <- "output data"
      names(messageMatch) <- paste0("'", data, "'")
    } else {
      messageMatch <- NULL
    }
    .lixoftCall("defineOutputElement",
                  list(name = outName, element = list(data = data, output = outputVect$name[indexName])),
                  messageMatch = messageMatch)
      outName_full <- c(outName_full, outName)
  }
  return(outName_full)
}

################################################################################
# .addOutput adds an output list (or list of list) to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addOutput <- function(output, groupName = NULL){
  if (is.null(output)) return(invisible(TRUE))

  sharedGroupName <- .lixoftCall("getGroups")[[1]]$name
  if(is.null(groupName)) {
    groupName <- sharedGroupName
  }

  outName_full <- NULL

  for (iout in seq_along(output)) {
    outValue <- output[[iout]]
    
    if (is.string(outValue)) {
      if (! outValue %in% .getMlxOutputNames()) {
        stop("Invalid output '", outValue, "' not found in Monolix project. ",
             "Available monolix outputs are ", paste(.getMlxOutputNames(), collapse=", "),
             ".", call.=F)
      }
      
      outName_full <- c(outName_full, outValue)

    } else if (is.element("time", names(outValue))) {
      outName <- .addUnitaryOutput(output = outValue, groupName)
      outName_full <- c(outName_full, outName)
    }
  }

  if (length(outName_full)) {
    .lixoftCall("setGroupElement", list(group = groupName, elements = outName_full))
  }
  return(invisible(TRUE))
}

#*******************************************************************************
# TRT MANAGEMENT
#*******************************************************************************
################################################################################
# .addUnitaryTrtName defines a name for the treatment
# - based on the group name
# - based on the first amount
################################################################################
.addUnitaryTrtName <- function(treatment, groupName = NULL){
  if (is.null(groupName)) {
    trtName <- paste0('TrT_', length(.lixoftCall("getTreatmentElements")) + 1)
  } else {
    trtName <- paste0('TrT_', groupName, '_', length(.lixoftCall("getTreatmentElements")) + 1)
  }
  return(trtName)
}

################################################################################
# .addUnitaryTrt adds an treatment (list) to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addUnitaryTrt <- function(treatment, groupName = NULL) {
  if (is.null(treatment)) {
    return(NULL)
  }

  if (is.string(treatment)) {
    treatment <- utils::read.table(file = treatment, header = T, sep = .getDelimiter(treatment))
    admID <- unique(treatment$type)
    probaMissDose <- unique(treatment$probaMissDose)
    repeats <- NULL
    df_treatment <- treatment[! names(treatment) %in% c("type", "probaMissDose")]
  } else {
    admID <- treatment$type
    probaMissDose <- treatment$probaMissDose
    repeats <- treatment$repeats
    df_treatment <- data.frame(time = treatment$time, amount = treatment$amount)
    
    # Add Tinf if present
    if ("tinf" %in% names(treatment)) {
      df_treatment$tInf <- treatment$tinf
    }
    
    if ("rate" %in% names(treatment)) {
      df_treatment$tInf <- df_treatment$amount/treatment$rate
    }
    
    if ("washout" %in% names(treatment)) {
      df_treatment$washout <- treatment$washout
    }
  }

  occInfo <- .lixoftCall("getOccasionElements")

  if (length(occInfo$occasions) > 1) {
    occLevel <- occInfo$occasions
    occLevelName <- occInfo$name
    occLevelTime <- occInfo$time
    occLevelId <- occInfo$id
    
    df_treatment$time <- as.numeric(df_treatment$time)
    matchOccNames <- unlist(sapply(occLevelName, function(name) grep(paste0("^", name, "$"), names(df_treatment), ignore.case=T, value=T)))
    df_treatment <- .renameColumns(df_treatment, unname(matchOccNames), names(matchOccNames))

    # check treatment elements
    extraNames <- setdiff(names(df_treatment),
                          c(occLevelName, "id", "time", "amount", "tinf", "rate", "washout"))
    if (length(extraNames)) {
      warning("Invalid occasions found in treatement: '", paste0(extraNames, collapse="', '"),
              "' will be removed. Available occasions are '",
              paste(occLevelName, collapse="', '"), "'.", call.=F)
      df_treatment <- df_treatment[! names(df_treatment) %in% extraNames]
    }

    if (! is.null(occLevelId) & ! "id" %in% names(df_treatment)) {
      ids <- unique(occLevelId)
      df_occ <- do.call("rbind", replicate(length(ids), df_treatment, simplify = FALSE))
      df_occ$id <- rep(ids, each=nrow(df_treatment))
    } else {
      df_occ <- df_treatment
    }
    for (o in seq_along(occLevelName)) {
      occname <- occLevelName[o]
      if (occname %in% names(df_occ)) next
      if (occname %in% .getOverlapOccasion()) next
      
      occasions <- sapply(occLevel, function(occ) occ[[o]])
      
      # levels same for all ids
      if (is.null(occLevelId)) {
        df_occ <- .fill_occasion(df_occ, occasions, occLevelTime, occname)
      } else {
        df <- df_occ
        for (id in unique(occLevelId)) {
          if (! id %in% df$id) next
          df_occ_id <- .fill_occasion(df[df$id == id,],
                                      occasions[occLevelId == id],
                                      occLevelTime[occLevelId == id],
                                      occname)
          df_occ[df_occ$id == id, occname] <- df_occ_id[[occname]]
        }
      }
    }
    df_occ <- df_occ[intersect(c("id", occLevelName, setdiff(names(df_treatment), "id")), names(df_occ))]
    df_treatment <- df_occ
  }

  if ("id" %in% names(df_treatment)) {
    data <- .addDataFrameTemp(df_treatment)
  } else {
    data <- df_treatment
  }

  trtName <- .addUnitaryTrtName(treatment = treatment, groupName = groupName)

  if (is.string(data)) {
    messageMatch <- "treatment data"
    names(messageMatch) <- paste0("'", data, "'")
  } else {
    messageMatch <- NULL
  }
  .lixoftCall("defineTreatmentElement",
              list(name = trtName, element = list(admID=admID,
                                                  probaMissDose=probaMissDose,
                                                  repeats=repeats,
                                                  data=data)),
              messageMatch = messageMatch)
  return(trtName)
}

################################################################################
# .addTreatement adds an treatment (or list of list) to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addTreatment <- function(treatment, groupName = NULL){

  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }

  if (is.null(treatment)) {
    return(invisible(TRUE))
  }

  trtName_full <- NULL

  for (indexTreatment in seq_along(treatment)) {

    treat <- treatment[[indexTreatment]]

    if (is.string(treat) && ! file.exists(treat)) {
      if (! treat %in% .getMlxTreatmentNames()) {
        stop("Invalid treatment. '", treat, "' not found in Monolix project. ",
             "Available monolix treatments are ", paste(.getMlxTreatmentNames(), collapse=", "),
             ".", call.=F)
      }
      trtName_full <- c(trtName_full, treat)
    } else {
      trtName <- .addUnitaryTrt(treatment = treat, groupName = groupName)
      trtName_full <- c(trtName_full, trtName)
    }
  }

  .lixoftCall("setGroupElement", list(group = groupName, elements = trtName_full))

  return(invisible(TRUE))
}

.getMlxTreatmentNames <- function() {
  treat <- names(.lixoftCall("getTreatmentElements"))
  if (!is.null(treat)) {
    treatNames <- treat[startsWith(treat, "mlx_Adm")]
  } else {
    treatNames <- c()
  }
  return(treatNames)
}

.getMlxOutputNames <- function() {
  output <- names(.lixoftCall("getOutputElements"))
  
  outputNames <- output[startsWith(output, "mlx_")]
  return(outputNames)
}

#*******************************************************************************
# REGRESSOR MANAGEMENT
#*******************************************************************************
################################################################################
# .addUnitaryRegName defines a name for the treatment
# - based on the group name
# - based on the regressor name
################################################################################
.addUnitaryRegName <- function(regressor, groupName = NULL){
  if (is.null(groupName)) {
    regName <- paste0('Reg_', length(.lixoftCall("getRegressorElements")) + 1)
  } else {
    regName <- paste0('Reg_', groupName, '_', length(.lixoftCall("getRegressorElements")) + 1)
  }
  return(regName)
}

################################################################################
# .addUnitaryRegressor adds a regressor (list) to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addUnitaryRegressor <- function(regressor, groupName = NULL){
  if (is.null(regressor)) {
    return(NULL)
  }

  regName <- .addUnitaryRegName(regressor, groupName)
  df = data.frame(regressor)
  .lixoftCall("defineRegressorElement", list(name = regName, element = df))

  return(regName)
}

################################################################################
# .addRegressor adds a regressor (list or list of a list) to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addRegressor <- function(regressor, groupName = NULL){
  shareGroupName <- .lixoftCall("getGroups")[[1]]$name
  if(is.null(groupName)){
    groupName <- shareGroupName
  }

  if (is.null(regressor)) {
    return(invisible(TRUE))
  }

  if(is.string(regressor)){
    regName <- .addUnitaryRegName(regressor, groupName)
    messageMatch <- "regressor data"
    names(messageMatch) <- paste0("'", regressor, "'")
    .lixoftCall("defineRegressorElement", list(name = regName, element = regressor),
                messageMatch = messageMatch)

  } else {
    regName = .addUnitaryRegressor(regressor = regressor, groupName)
  }

  .lixoftCall("setGroupElement", list(group=groupName, elements = regName))

  return(invisible(TRUE))
}

#*******************************************************************************
# PARAMETER MANAGEMENT
#*******************************************************************************

################################################################################
# .addParameter adds a parameter vector to the group groupName
# It is split between individual and population
# If groupName is NULL, it will be added to the shared group
################################################################################
.addParameter <- function(parameter, groupName = NULL){
  if(is.null(parameter)) {
    return(invisible(TRUE))
  }
  popElements <- .lixoftCall("getPopulationElements")
  indivElements <- .lixoftCall("getIndividualElements")

  # paramtype: individual or population
  if (is.null(groupName)) {
    g <- .lixoftCall("getGroups")[[1]]
    groupName <- .lixoftCall("getGroups")[[1]]$name
  } else {
    g <- .lixoftCall("getGroups")[sapply(.lixoftCall("getGroups"), function(g) g$name == groupName)][[1]]
  }
  paramType <- g$parameter$type

  if (is.string(parameter) ) {
    parameterValues <- utils::read.table(file=parameter, header=T, sep=.getDelimiter(parameter))
    namesParamValues <- names(parameterValues)
    occasion_elements <- .lixoftCall("getOccasionElements")
    nameIntersect <- intersect(namesParamValues,
                               c('id', 'ID', 'pop', 'occ', 'occ1', 'occ2', 'occevid', unlist(occasion_elements$names)))

    if (length(nameIntersect)) {
      indexID_OCC = match(nameIntersect, namesParamValues)
      paramNames <- namesParamValues[-indexID_OCC]
    } else {
      paramNames <- namesParamValues
    }

  } else {
    paramNames <- names(parameter)
  }

  # if ind params & pop params defined together --> raise error
  parameterInd <- .filterParameter(parameter, type="ind")
  parameterPop <- .filterParameter(parameter, type="pop")

  # Add parameters -------------------------------------------------------------
  expectedParams <- c()
  
  if (length(parameterInd) > 0) {
    # Add parameters that describe individuals
    outIndivName <- .addIndParameter(parameter, groupName)
    outName <- outIndivName
    expectedParams <- c(expectedParams, .getParameterNames("indiv"))

    .lixoftCall("setGroupElement", list(group=groupName, elements=outName))
    
    # remaining parameters
    remainingParamNames <- names(.lixoftCall("getGroupRemaining", groupName))
    if (length(intersect(remainingParamNames, names(parameterPop)))) {
      remainingParamNames <- intersect(remainingParamNames, names(parameterPop))
      remainingParams <- parameterPop[remainingParamNames]
      .lixoftCall("setGroupRemaining", list(group=groupName, remaining=remainingParams))
    }

  } else if (length(parameterPop) > 0) {
    # Add parameters that describe populations
    outPopName <- .addPopParameter(parameter, groupName)
    outName <- outPopName
    expectedParams <- c(expectedParams, .getParameterNames("pop"))

    .lixoftCall("setGroupElement", list(group=groupName, elements=outName))
    remainingParamNames <- NULL

  } else {
    remainingParamNames <- NULL
  }
  
  if (length(parameterInd) > 0 & length(parameterPop[setdiff(names(parameterPop), remainingParamNames)]) > 0) {
    stop("Population parameters and individuals parameters cannot be defined together.", call. = FALSE)
  }
  
  # Print Warning in case something is missing ---------------------------------
  # warn in case of missing parameters
  ismlxproject <- any(grepl("mlx", names(.lixoftCall("getOutputElements"))))
  .checkMissingParameters(paramNames, expectedParams, ismlxproject)

  # warn in case of extra parameters
  # RETRO 2020 - Remove covariates from parameters
  remainingParamNames <- names(.lixoftCall("getGroupRemaining", groupName))
  .checkExtraParameters(setdiff(paramNames, c(.getParameterNames("cov"), remainingParamNames)),
                        expectedParams)

  return(invisible(TRUE))
}

# Add Covariates in a group
.addCovariate <- function(covariate, groupName = NULL){
  covElements <- .lixoftCall("getCovariateElements")
  
  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }

  groups <- .lixoftCall("getGroups")
  group <- groups[sapply(groups, function(g) g$name == groupName)][[1]]
  paramType <- group$parameter$type

  if (is.string(covariate)) {
    covariateValues <- utils::read.table(file=covariate, header=T, sep=.getDelimiter(covariate))
    namesCovValues <- names(covariateValues)
    occasion_elements <- .lixoftCall("getOccasionElements")
    nameIntersect <- intersect(namesCovValues,
                               c('id', 'ID', 'pop', 'occ', 'occ1', 'occ2', 'occevid', unlist(occasion_elements$names)))
    
    if (length(nameIntersect)) {
      indexID_OCC = match(nameIntersect, namesCovValues)
      covNames <- namesCovValues[-indexID_OCC]
    } else {
      covNames <- namesCovValues
    }
    
  } else {
    covNames <- names(covariate)
  }

  # Add covariates -------------------------------------------------------------
  expectedCov <- c()
  
  # Add parameters that describe covariates
  outCovName <- .addCovParameter(covariate, covNames, groupName)

  if (length(.lixoftCall("getCovariateElements")) > 0) {
    expectedCov <- c(expectedCov, .getParameterNames("cov"))
  }

  # Print Warning in case something is missing ---------------------------------
  # warn in case of missing parameters
  ismlxproject <- any(grepl("mlx", names(.lixoftCall("getOutputElements"))))
  .checkMissingParameters(covNames, expectedCov, ismlxproject)
  
  # warn in case of extra parameters
  .checkExtraParameters(covNames, expectedCov)
  
  if (paramType == "individual") {
    warning("Covariate won't be used because individual parameters have been defined.", call.=F)
    return(invisible(TRUE))
  } else if (!is.null(outCovName)) {
    .lixoftCall("setGroupElement", list(group=groupName, elements=outCovName))
  }
  
  return(invisible(TRUE))
}

.addCovParameter <- function(covariate, covariateNames, groupName) {
  covElements <- .lixoftCall("getCovariateElements")
  covariate <- .filterParameter(covariate, "cov")

  if (length(covElements) == 0 | length(covariate) == 0) {
    return(NULL)
  }

  covName <- paste0('manCov', groupName, '_', length(.lixoftCall("getCovariateElements")) + 1)
  
  # default covariate parameters
  covParam <- covElements[[1]]$data
  covParamName <- .getParameterNames("cov")

  # check if covariate parameters are missing
  missingcov <- setdiff(covParamName, covariateNames)
  
  # input is a dataframe
  if (is.string(covariate)) {
    messageMatch <- "parameter data"
    names(messageMatch) <- paste0("'", covariate, "'")
    .lixoftCall("defineCovariateElement",
                list(name=covName, element=covariate),
                messageMatch=messageMatch)

  } else {
    covElement <- covariate
    # if multiple values, transform covariates to dataframe
    if (nrow(covElement) > 1) {
      covElement <- cbind(data.frame(id=1:nrow(covElement)), covElement)
      
    } else if (length(missingcov)) {
      # if missing covariate, replace values in default covariate element
      covParam[names(covariate)] <- covariate
      covElement <- covParam
      
    }
    
    # Define covariate element
    if (nrow(covElement) > 1) {
      # Convert in text file
      tempFile = .addDataFrameTemp(df=covElement)
      messageMatch <- "covariate parameters data"
      names(messageMatch) <- paste0("'", tempFile, "'")
      .lixoftCall("defineCovariateElement", list(name=covName, element=tempFile),
                  messageMatch=messageMatch)
      
    } else {
      .lixoftCall("defineCovariateElement", list(name=covName, element=covElement))
    }
  }
  return(covName)
}

# Add parameters that describe a population
.addPopParameter <- function(parameter, groupName) {
  popElements <- .lixoftCall("getPopulationElements")
  inputPop <- .filterParameter(parameter, type="pop")

  if (length(popElements) == 0 | length(inputPop) == 0) {
    return(NULL)
  }

  popName <- paste0('manualPop', groupName,'_', length(.lixoftCall("getPopulationElements")) + 1)
  
  # default pop parameters
  popParamName <- .getParameterNames("pop")
  popParam <- popElements[[1]]$data[popParamName]
  if (is.data.frame(popParam)) {
    popParam <- .transformToNumeric(popParam)
  }
  popParam <- unlist(popParam)
  
  # input is a dataframe
  if (is.string(inputPop)) {
    parameterValues <- utils::read.table(file=inputPop, header=T, sep=.getDelimiter(inputPop))
    if (! "pop" %in% names(parameterValues)) {
      stop("A data.frame of population parameters can only be used in combination with the argument 'npop'. ",
           "The data frame must have a column 'pop'.", call. = FALSE)
    }
    
    tempFile = .addDataFrameTemp(parameterValues[names(parameterValues) != "pop"])
    messageMatch <- "population parameters data"
    names(messageMatch) <- paste0("'", tempFile, "'")
    .lixoftCall("definePopulationElement", list(name=popName, element=tempFile),
                messageMatch=messageMatch)
    
  } else {
    popElement <- as.data.frame(matrix(popParam, ncol= length(popParam)))
    names(popElement) <- names(popParam)
    popElement[names(inputPop)] <- inputPop
    
    .lixoftCall("definePopulationElement", list(name=popName, element=popElement))
  }
  
  return(popName)
}

# Add parameters describe an individual
.addIndParameter <- function(parameter, groupName) {
  indivElements <- .lixoftCall("getIndividualElements")
  inputIndiv <- .filterParameter(parameter, type="indiv")
  
  if (length(indivElements) == 0 | length(inputIndiv) == 0) {
    return(NULL)
  }
  
  indivName <- paste0('manualIndiv', groupName, '_', length(.lixoftCall("getIndividualElements")) + 1)
  
  # default indiv parameters
  indivParamName <- .getParameterNames("indiv")
  indivParam <- unlist(indivElements[[1]]$data[indivParamName])
  
  # input is a dataframe
  if (is.string(inputIndiv)) {
    messageMatch <- "individual parameters data"
    names(messageMatch) <- paste0("'", inputIndiv, "'")
    .lixoftCall("defineIndividualElement", list(name=indivName, element=inputIndiv),
                messageMatch = messageMatch)

  } else {
    indivElement <- as.data.frame(matrix(indivParam, ncol=length(indivParam)))
    names(indivElement) <- names(indivParam)
    indivElement[names(inputIndiv)] <- inputIndiv
    
    .lixoftCall("defineIndividualElement", list(name=indivName, element=indivElement))
    
  }
  return(indivName)
}

.getParameterNames <- function(type="pop") {
  occasion_elements <- .lixoftCall("getOccasionElements")
  # Parameters that describes a covariate
  if (type == "cov") {
    elements <- .lixoftCall("getCovariateElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(elements[[1]]$data)
    paramNames <- setdiff(elementNames, c("ID", "id", unlist(occasion_elements$names)))
    
  } else if (type == "pop") {
    elements <- .lixoftCall("getPopulationElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(unlist(elements[[1]]$data))
    paramNames <- setdiff(elementNames, "id")
  } else {
    elements <- .lixoftCall("getIndividualElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(unlist(elements[[1]]$data))
    paramNames <- setdiff(elementNames, "id")
  }
  return(paramNames)
}

.filterParameter <- function(parameter, type = "pop", store_dataframe=T, unlistOutput=F){
  occasion_elements <- .lixoftCall("getOccasionElements")
  # extraNames <- c('id', 'pop', 'occ', 'occ1', 'occ2', 'occevid')
  extraNames <- c("id", "pop", "ID", unlist(occasion_elements$names))

  elementNames <- .getParameterNames(type)

  if (is.string(parameter)) {
    # filter dataframe with cov / ind / pop parameters only
    parameterValues <- utils::read.table(file = parameter, header = T, sep = .getDelimiter(parameter))
    # avoid F and T to be interpreted as FALSE and TRUE --> logical column to character
    idlog <- names(parameterValues[sapply(parameterValues, class) == "logical"])
    parameterValues[idlog] <- sapply(parameterValues[idlog], function(col) gsub("FALSE", "F", col))
    parameterValues[idlog] <- sapply(parameterValues[idlog], function(col) gsub("TRUE", "T", col))
    parameterValuesNames <- names(parameterValues)
    pName <- intersect(parameterValuesNames, elementNames)

    if (length(pName) == 0) {
      return(NULL)
    }

    pExtra = parameterValuesNames[parameterValuesNames %in% extraNames]
    parameterValues <- parameterValues[c(pExtra, pName)]
    # parameterValues <- parameterValues[, c(pExtra, pName)]
    # names(parameterValues) <- c(pExtra, pName)

    # if type = ind or type = cov --> remove pop column
    if (type %in% c("ind", "cov")) {
      # remove pop column
      popName <- parameterValuesNames[parameterValuesNames == "pop"]
      if (length(popName)) {
        parameterValues <- unique(parameterValues[names(parameterValues) != popName])
      }
    }

    # if type = pop --> remove id column
    if (type == "pop") {
      # remove id column
      idName <- parameterValuesNames[parameterValuesNames == "id"]
      if (length(idName)) {
        parameterValues <- unique(parameterValues[names(parameterValues) != idName])
      }
    }

    if (is.data.frame(parameterValues)) {
      parameterValues <- unique(parameterValues)
      if (nrow(parameterValues) == 1) {
        parameter <- as.data.frame(as.list(parameterValues))
      } else {
        if (store_dataframe) {
          parameter <- .addDataFrameTemp(parameterValues)
        } else {
          parameter <- parameterValues
        }
      }
    } else {
      parameter <- parameterValues
    }
  } else {
    parameter <- as.data.frame(as.list(parameter))
    parameter <- parameter[names(parameter) %in% elementNames]
    parameter <- .transformToNumeric(parameter)
    
    if (unlistOutput) {
      parameter <- unlist(parameter)
    }

  }
  return(parameter)
}

.getAllowedMlxParameterNames <- function(type="all") {
  if (type == "pop") {
    allowedParams <- c("mlx_Pop", "mlx_PopUncertainSA", "mlx_PopUncertainLin")
  } else if (type == "ind") {
    allowedParams <- c("mlx_PopIndiv","mlx_PopIndivCov","mlx_CondMean","mlx_EBEs","mlx_CondDistSample")
  } else {
    allowedParams <- c(.getAllowedMlxParameterNames("pop"), .getAllowedMlxParameterNames("ind"))
  }
  return(allowedParams)
}

.getMlxParameterNames <- function(type="all") {
  if (type == "pop") {
    popElements <- names(.lixoftCall("getPopulationElements"))
    extfiles_path <- file.path(.lixoftCall("getProjectSettings")$directory, "ExternalFiles")
    
    # 2021 ! To remove in 2022 when function getPopulationElements is fixed
    # check if popUncertainLin exists
    if (file.exists(file.path(extfiles_path, "mlx_PopUncertainLin.dat"))) {
      popElements <- c(popElements, "mlx_PopUncertainLin")
    }
    # check if popUncertainSA exists
    if (file.exists(file.path(extfiles_path, "mlx_PopUncertainSA.dat"))) {
      popElements <- c(popElements, "mlx_PopUncertainSA")
    }
    
    paramNames <- intersect(
      unique(popElements),
      .getAllowedMlxParameterNames("pop")
    )
  } else if (type == "ind") {
    paramNames <- intersect(
      names(.lixoftCall("getIndividualElements")),
      .getAllowedMlxParameterNames("ind")
    )
  } else {
    paramNames <- c(.getMlxParameterNames("pop"), .getMlxParameterNames("ind"))
  }
  return(paramNames)
}

.getAllowedMlxCovariateNames <- function() {
  allowedCov <- c("mlx_Cov", "mlx_CovDist")
  return(allowedCov)
}

.getMlxCovariateNames <- function() {
  covNames <- intersect(
    names(.lixoftCall("getCovariateElements")),
    .getAllowedMlxCovariateNames()
  )
  return(covNames)
}

.addMlxParameter <- function(parameter, groupName = NULL){
  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }
  if (! parameter %in% .getMlxParameterNames()) {
      stop("Invalid parameter. ", parameter, " not found in Monolix project. ",
           "Available monolix parameters are ", paste(.getMlxParameterNames(), collapse=", "),
           ".", call.=F)
  }
  .lixoftCall("setGroupElement", list(group = groupName, elements = parameter))

  # set remaining parameters
  if (parameter %in% .getMlxParameterNames("ind")) {
    remaining <- .lixoftCall("getGroupRemaining", list(group = groupName))
    popData <- .lixoftCall("getPopulationElements")$mlx_Pop$data
    
    namesPopData <- names(popData)
    for (indexParam in seq_along(remaining)) {
      remainingName <- names(remaining)[indexParam]
      remaining[indexParam] <- popData[which(namesPopData==remainingName)]
    }
    
    .lixoftCall("setGroupRemaining", list(group = groupName, remaining = remaining))

  }
  
  return(invisible(TRUE))
}

.addMlxCovariate <- function(covariate, groupName = NULL){
  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }
  if (! covariate %in% .getMlxCovariateNames()) {
    stop("Invalid covariate. ", covariate, " not found in Monolix project. ",
         "Available monolix covariates are ", paste(.getMlxCovariateNames(), collapse=", "),
         ".", call.=F)
  }
  .lixoftCall("setGroupElement", list(group = groupName, elements = covariate))

  return(invisible(TRUE))
}

################################################################################
# .addGroup add the element of the group to the group groupName
# If groupName is NULL, it will be added to the shared group
################################################################################
.addGroup <- function(group, groupName = NULL){
  if (is.null(group)) {
    return(invisible(TRUE))
  }

  if(is.null(groupName)){
    groupName = .lixoftCall("getGroups")[[1]]$name
  }

  # add parameters in group
  if ("parameter" %in% names(group)) {
    parameter = group$parameter
    
    if (parameter[1] %in% .getAllowedMlxParameterNames()) {
      .addMlxParameter(parameter, groupName)
      
    } else {
      .addParameter(parameter = parameter, groupName)
    }
    
  }

  # add covariates in group
  # RETRO 2020 - Include covariates from parameters argument
  if ("parameter" %in% names(group)) {
    p <- group$parameter
    if (! p %in% .getAllowedMlxParameterNames()) {
      covFromParameter <- .filterParameter(p, "cov", store_dataframe=F, unlistOutput=T)
      if (! is.null(covFromParameter)) {
        message("[INFO] Using 'parameter' argument to define covariates is deprecated. Use 'covariate' argument instead.")
        if (is.string(group$covariate)) {
          if (group$covariate[1] %in% .getAllowedMlxCovariateNames()) {
            stop("You must either define covariate with a string corresponding to the element generated by Monolix, or with a list.", call.=F)
          }
          group$covariate <- utils::read.table(file=group$covariate, header=T, sep=.getDelimiter(covariate))
        }
        group$covariate <- .transformParameter(list(
          covFromParameter,
          group$covariate
        ))
        
        if (is.data.frame(group$covariate)) {
          group$covariate <- .addDataFrameTemp(df=group$covariate)
        }
      }
      if (is.null(c(.filterParameter(p, "ind"), .filterParameter(p, "pop")))) {
        group$parameter <- NULL
      }
    }
  }
  
  if (! is.null(group$covariate)) {
    covariate = group$covariate

    if (covariate[1] %in% .getAllowedMlxCovariateNames()) {
      .addMlxCovariate(covariate, groupName)
    } else {
      .addCovariate(covariate=covariate, groupName)
    }
  }
  
  # add output in group
  if ("output" %in% names(group)) {
    .addOutput(output=group$output, groupName)
  }

  # add treatment in group
  if ("treatment" %in% names(group)) {
    treatment <- group$treatment
    .addTreatment(treatment, groupName)
  }

  # add regressor in group
  if ("regressor" %in% names(group)) {
    .addRegressor(group$regressor, groupName)
  }

  # set size of the group
  if ("size" %in% names(group)) {
    .lixoftCall("setGroupSize", list(groupName, group$size))
  }

  return(group)
}

################################################################################
# .addOutputToFile
################################################################################
.addOutputToFile <- function(model, outputName){
  tempFile <- tempfile(pattern = 'SimulxModel_', fileext = '.txt')
  tempFile <- gsub(x = tempFile, pattern = '\\\\', replacement = "/")
  file.copy(from = model, to = tempFile, overwrite = T)

  lines <- suppressMessages(suppressWarnings(readLines(tempFile, -1L)))

  isLONG <- F

  write(x = '', file = tempFile, append = FALSE)

  for (index in seq_along(lines)) {
    line <- lines[index]
    lineTest <- line
    lineTest <- sub(pattern = '[[]', replacement = '', x = lineTest)
    lineTest <- sub(pattern = ']', replacement = '', x = lineTest)
    if (grepl(x = lineTest, pattern = 'LONGITUDINAL')) {
      isLONG = T
    }

    if (grepl(x = lineTest, pattern = 'COVARIATE')) {
      if (isLONG) {
        write(x = paste0("\nOUTPUT:\noutput={", paste0(unique(outputName), collapse = ','),'}\n'),
              file = tempFile, append = TRUE)
      }
      isLONG <- F
    }

    if (grepl(x = lineTest, pattern = 'INDIVIDUAL')) {
      if (isLONG) {
        write(x = paste0("\nOUTPUT:\noutput={", paste0(unique(outputName), collapse = ','),'}\n'),
              file = tempFile, append = TRUE)
      }
      isLONG <- F
    }
    write(x = line, file = tempFile, append = TRUE)
  }

  if (isLONG) {
    write(x = paste0("\nOUTPUT:\noutput={", paste0(unique(outputName), collapse = ','),'}\n'),
          file = tempFile, append = TRUE)
  }

  return(tempFile)
}

################################################################################
# Get the output names
################################################################################
.getOutputNames <- function(output, group){
  outputName <- NULL

  # outputs defined in groups
  if (! is.null(group)) {
    for (g in group) {
      if ("output" %in% names(g)) {
        outputName <- c(outputName, sapply(g$output, function(o) o$name))
      }
    }
  }

  # outputs defined in output argument
  if (! is.null(output)) {
    outputName <- c(outputName, sapply(output, function(o) o$name))
  }

  if (is.list(outputName)) {
    outputName <- do.call(c, outputName)
  }
  outputName <- unique(outputName)
  return(outputName)
}

.getModelSection <- function(modelFile, section, block = NULL) {
  sections <- c("[LONGITUDINAL]", "[INDIVIDUAL]", "[COVARIATE]", "[POPULATION]")
  blocks <- c("DESCRIPTION:", "PK:", "EQUATION:", "DEFINITION:", "OUTPUT:")
  l <- readLines(modelFile, warn=FALSE)

  if (!grepl("^[[a-zA-z]*]$", section, perl = TRUE)) {
    section <- paste0("[", section, "]")
  }

  if (! section %in% l | ! section %in% sections) {
    return(NULL)
  }

  sections_idx <- which(l %in% sections)
  sections_idx <- c(sections_idx, length(l) + 1)
  isection <- which(l[sections_idx] == section)
  s <- l[sections_idx[isection]:sections_idx[isection + 1] - 1]

  if (is.null(block)) {
    return(s)
  }

  if (!grepl(":$", block, perl = TRUE)) {
    block <- paste0(block, ":")
  }
  if (! block %in% s | ! block %in% blocks) {
    return(s)
  }

  blocks_idx <- which(s %in% blocks)
  blocks_idx <- c(blocks_idx, length(s))
  iblock <- which(s[blocks_idx] == block)
  b <- s[blocks_idx[iblock]:blocks_idx[iblock + 1]]
  return(b)

}

################################################################################
# Transform Parameters
################################################################################
# When parameter is a list of list: merge parameters lists
.mergeParameter <- function(parameter) {
  if (is.null(parameter)) {
    return(parameter)
  }

  # if parameter is a list of parameters list: we concatenate lists
  if (.is_list_or_named_vector(parameter)) {
    if (all(sapply(parameter, .is_list_or_named_vector))) {
      dfparam <- parameter[sapply(parameter, is.data.frame)]
      listparam <- parameter[!sapply(parameter, is.data.frame)]
      # merge dataframes
      if (length(dfparam) > 1) {
        dfparam <- do.call(merge, dfparam)
      } else if (length(dfparam) == 1) {
        dfparam <- dfparam[[1]]
      } else {
        dfparam <- NULL
      }
      # merge lists
      if (length(listparam) > 1) {
        listparam <- as.list(do.call(c, sapply(listparam, as.list)))
      } else if (length(listparam) == 1) {
        listparam <- as.list(listparam[[1]])
      } else {
        listparam <- NULL
      }

      if (is.null(dfparam)) {
        parameter <- listparam
      } else if (is.null(listparam)) {
        parameter <- dfparam
      } else {
        parameter <- cbind(dfparam, listparam)
      }
    }
  }
  return(parameter)
}

# Transform parameters: merge parameters data
.transformParameter <- function(parameter) {
  if (is.null(parameter)) {
    return(parameter)
  }

  # if parameter is a list of parameters list: we concatenate lists
  if (.is_list_or_named_vector(parameter)) {
    # remove NULL in list
    parameter[sapply(parameter, is.null)] <- NULL
    if (length(parameter) == 0) {
      return(NULL)
    }
    for (i in seq_along(parameter)) {
      p <- parameter[[i]]
      if (is.string(p) && file.exists(p)) {
        .checkExtension(.getFileExt(p), "parameter file extension")
        parameter[[i]] <- utils::read.table(file=p, header=T, sep=.getDelimiter(p))
      }
    }
    if (all(sapply(parameter, .is_list_or_named_vector))) {
      parameter <- .mergeParameter(parameter)
    }
  }
  
  return(parameter)
}

################################################################################
# Transform Regressors
################################################################################
# When several regressors: merge them and match times with last value carried forward
.mergeRegressors <- function(regressor) {
  if (is.null(regressor)) {
    return(regressor)
  }

  dfreg <- regressor[sapply(regressor, is.data.frame)]
  listreg <- regressor[!sapply(regressor, is.data.frame)]

  # merge dataframes
  if (length(dfreg)) {
    if (length(dfreg) > 1) {
      dfreg <- do.call(merge, c(dfreg, list(all = TRUE)))
    } else {
      dfreg <- dfreg[[1]]
    }
    idName <- names(dfreg)[names(dfreg) == "id"]
    ids <- unique(dfreg[[idName]])
  } else {
    dfreg <- NULL
    ids <- c()
    idName <- NULL
  }

  # merge lists
  for (il in seq_along(listreg)) {
    l <- listreg[[il]]
    l <- .renameColumns(as.data.frame(l), "value", l$name)
    l <- l[names(l) != "name"]
    listreg[[il]] <- l
  }

  if (length(listreg)) {
    if (length(listreg) > 1) {
      listdata <- listreg[[1]]
      for (ireg in seq(2, length(listreg))) {
        listdata <- merge(listdata, listreg[[ireg]], all = TRUE)
      }
      listreg <- listdata
    } else {
      listreg <- listreg[[1]]
    }
    if (length(ids)) {
      nbids <- length(ids)
      nbtimes <- nrow(listreg)
      listreg <- do.call("rbind", replicate(nbids, listreg, simplify = FALSE))
      listreg[[idName]] <- rep(ids, each = nbtimes)
    }
  } else {
    listreg <- NULL
  }

  if (is.null(dfreg)) {
    regressor <- listreg
  } else if (is.null(listreg)) {
    regressor <- dfreg
  } else {
    regressor <- merge(dfreg, listreg, all = TRUE)
  }

  # fill na with last carried forward value
  columnswithna <- names(regressor)[sapply(regressor, function(h) any(is.na(h)))]
  if (length(ids)) {
    for (h in columnswithna) {
      for (id in ids) {
        col <- regressor[regressor[[idName]] == id, h]
        col <- .applyLastCarriedForward(col)
        regressor[regressor[[idName]] == id, h] <- col
      }
    }
  } else {
    for (h in columnswithna) {
      regressor[[h]] <- .applyLastCarriedForward(regressor[[h]])
    }
    regressor <- as.list(regressor)
  }
  return(regressor)
}

.applyLastCarriedForward <- function(vector) {
  if (is.null(vector)) return(vector)
  firstnonan <- which(!is.na(vector))[1]
  if (firstnonan > 1) vector[1: firstnonan - 1] <- vector[firstnonan]
  nonanId <- which(!is.na(vector))
  nanId <- which(is.na(vector))
  for (na in nanId) {
    vector[na] <- vector[tail(nonanId[nonanId < na], n = 1)]
  }
  return(vector)
}

.transformRegressor <- function(regressor) {
  # When several regressors, merge them and match times with last value carried forward
  regressor <- .mergeRegressors(regressor)
  return(regressor)
}

################################################################################
# Transform treatments
################################################################################
.splitTreatment <- function(treatment) {
  ntreat <- length(treatment)
  for (itr in seq_along(treatment)) {
    trt <- treatment[[itr]]
    
    if (is.string(trt)) {
      next
    }
    # if multiple admin types or multiple probaMissDose -> split treatments
    admColumn <- names(trt)[names(trt) %in% c("adm", "type")]
    complianceColumn <- names(trt)[names(trt) == "probaMissDose"]
    
    trt_repeat <- trt$repeats
    if (! is.data.frame(trt)) trt <- trt[names(trt) != "repeats"]

    if (! is.null(c(admColumn, complianceColumn))) {
      trt_list <- split(as.data.frame(trt), trt[c(admColumn, complianceColumn)])
      trt_list <- trt_list[sapply(trt_list, nrow) > 0]

      for (i in seq_along(trt_list)) {
        trt_i <- trt_list[[i]]
        idx <- ifelse(i == 1, itr, length(treatment) + 1)

        if (! is.data.frame(trt)) {
          trt_i <- as.list(trt_i)
          trt_i$repeats <- trt_repeat 
          if (length(admColumn) > 0) {
            trt_i[[admColumn]] <- trt_i[[admColumn]][1]
          }
          if (length(complianceColumn) > 0) {
            trt_i[[complianceColumn]] <- trt_i[[complianceColumn]][1]
          }
        }
  
        treatment[[idx]] <- trt_i
      }
    }
  }
  return(treatment)
}

################################################################################
# Censoring of output dataframe
################################################################################

# censor output dataframe
.censorOutput <- function(output, outputName, outputParam, groupParam) {
  outInfo <- NULL
  isGroup <- FALSE
  if (! is.null(outputParam)) {
    outId <- which(sapply(outputParam, function(o) ifelse("name" %in% names(o), outputName %in% o$name, o == paste0("mlx_", outputName))))
    if (length(outId)) {
      outInfo <- outputParam[[outId]]
    }
  } else if (!is.null(groupParam)) {
    outInfo <- sapply(groupParam, function(g) g$output[sapply(g$output, function(o) ifelse("name" %in% names(o), outputName %in% o$name, o == paste0("mlx_", outputName)))])
    isGroup <- TRUE
  }
  isCens <- any(sapply(outInfo, function(o) any(is.element(c("lloq", "limit", "uloq"), names(o)))))
  if (isCens) output$cens <- 0
  if (isGroup) {
    for (g in seq_along(outInfo)) {
      output[output$group == g,] <- .cens(output[output$group == g,], outputName, outInfo[[g]])
    }
  } else {
    output <- .cens(output, outputName, outInfo)
  }
  if (is.element("cens", names(output))) output$cens <- as.factor(output$cens)
  return(output)
}

.cens <- function(data, dataName, dataInfo) {
  if (is.element("lloq", names(dataInfo))) {
    lloq <- dataInfo$lloq
    if (!is.element("cens", names(data))) data$cens <- 0
    data$cens[data[[dataName]] <= lloq] <- 1
    data[[dataName]][data[[dataName]] <= lloq] <- lloq
    if (is.element("limit", names(dataInfo))) {
      limit <- dataInfo$limit
      data$limit <- "."
      data$limit[data$cens == 1] <- limit
    }
  }
  if (is.element("uloq", names(dataInfo))) {
    uloq <- dataInfo$uloq
    if (!is.element("cens", names(data))) data$cens <- 0
    data$cens[data[[dataName]] >= uloq] <- -1
    data[[dataName]][data[[dataName]] >= uloq] <- uloq
  }
  return(data)
}

# Return the number of ids in a dataframe
.getNbIds <- function(data) {
  indexID <- which(names(data) == 'id')
  if (length(indexID) > 0) {
    nbID <- length(unique(data[,indexID[1]]))
  } else {
    nbID <- NULL
  }
  return(nbID)
}

.getOverlapOccasion <- function() {
  occInfo <- .lixoftCall("getOccasionElements")
  overlap <- c()
  for (i in seq_along(occInfo$name)) {
    occasions <- sapply(occInfo$occasions, function(occ) occ[[i]])
    times <- occInfo$time
    occTime <- NULL
    occlevel <- sort(unique(occasions))
    for (o in occlevel) {
      occTime[[o]] <- unique(times[occasions == o])
    }
    if (! length(unique(occTime)) == length(occlevel)) {
      overlap <- c(overlap, occInfo$name[[i]])
    }
  }
  return(overlap)
}

.fill_occasion <- function(df, occ, occTime, occname) {
  df[[occname]] <- 1

  maxTime <- max(max(occTime + 1), max(df$time) + 1)
  occTime <- unique(c(occTime, maxTime))
  
  for (idx in seq_along(occ)) {
    df[(df$time >= occTime[idx]) & (df$time < occTime[idx + 1]), occname] <- occ[[idx]]
  }
  return(df)
}

