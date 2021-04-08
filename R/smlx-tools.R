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

  if (length(.lixoftCall("getOccasionElements")$occasions) > 1) {
    occLevel <- .lixoftCall("getOccasionElements")$occasions
    occLevelName <- .lixoftCall("getOccasionElements")$name
    occLevelTime <- .lixoftCall("getOccasionElements")$time
    occLevelId <- .lixoftCall("getOccasionElements")$id

    if (is.string(tt)) {
      df_occ <- utils::read.table(file = tt, header = T, sep = .getDelimiter(tt))
      df_occ$occ <- 1
      idName <- names(df_occ)[names(df_occ) == 'id']
      if (is.null(occLevelId)) {
        occLevelTime <- c(occLevelTime, max(df_occ$time) + 1)
        for (idx in seq_along(occLevel)) {
          df_occ[(df_occ$time >= occLevelTime[idx]) & (df_occ$time < occLevelTime[idx + 1]),] <- occLevelName
        }
      } else {
        for (id in unique(occLevelId)) {
          occTimeId <- unique(c(occLevelTime[occLevelId == id], max(df_occ$time[df_occ[[idName]] == id]) + 1))
          occId <- occLevel[occLevelId == id]
          for (idx in seq_along(occId)) {
            df_occ[(df_occ$id == id) & (df_occ$time >= occLevelTime[idx]) & (df_occ$time < occLevelTime[idx + 1]),] <- occLevelName
          }
        }
      }
    } else {
      df_occ <- NULL
      if (is.null(occLevelId)) {
        occLevelTime <- c(occLevelTime, max(tt) + 1)
        for (idx in seq_along(occLevel)) {
          occlist <- occLevel[[idx]]
          names(occlist) <- occLevelName
          df <- cbind(as.list(occlist), data.frame(time = tt[tt >= occLevelTime[idx] & tt < occLevelTime[idx + 1]]))
          df_occ <- rbind(df_occ, df)
        }
      } else {
        for (id in unique(occLevelId)) {
          occTimeId <- c(occLevelTime[occLevelId == id], max(tt) + 1)
          occId <- occLevel[occLevelId == id]
          for (idx in seq_along(occId)) {
            occlist <- occId[[idx]]
            names(occlist) <- occLevelName
            occlist <- c(id = id, occlist)
            df <- cbind(as.list(occlist), data.frame(time = tt[tt >= occTimeId[idx] & tt < occTimeId[idx + 1]]))
            df_occ <- rbind(df_occ, df)
          }
        }
      }
    }
    data <- .addDataFrameTemp(df_occ)
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
  sharedGroupName <- .lixoftCall("getGroups")[[1]]$name
  if(is.null(groupName)) {
    groupName <- sharedGroupName
  }

  outName_full <- NULL

  # in case of mlx project, add mlx outputs to shared group
  mlxOutputsId <- grep("mlx_", .lixoftCall("getGroups")[[1]]$output)
  if (groupName == sharedGroupName & length(mlxOutputsId)) {
    mlxOutputs <- .lixoftCall("getGroups")[[1]]$output[mlxOutputsId]
    mlxOutputsNames <- unname(sapply(.lixoftCall("getOutputElements")[mlxOutputs], function(o) o$output))
    outNames <- unname(sapply(output, function(o) o$name))
    mlxOutputs <- mlxOutputs[! mlxOutputsNames %in% outNames]
    if (length(mlxOutputs)) {
      # do not include outputs for which time is set to none
      isRemoved <- sapply(output, function(o) all(o$time == "none"))
      if (any(TRUE %in% isRemoved)) {
        outputToRemove <- do.call(c, lapply(output[isRemoved], function(o) o$name))
        mlxOutputs <- mlxOutputs[! mlxOutputs %in% paste0("mlx_", outputToRemove)]
      }
      outName_full <- mlxOutputs
    }
  }

  if (!is.null(output)) {
    output <- output[sapply(output, function(o) all(o$time != "none"))]
    for (iout in seq_len(length(output))) {
      outValue <- output[[iout]]
      if (is.element("time", names(outValue))) {
        outName <- .addUnitaryOutput(output = outValue, groupName)
        outName_full <- c(outName_full, outName)
      }
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

  df_treatment <- data.frame(time = treatment$time, amount = treatment$amount)

  # Add Tinf if present
  if ("tinf" %in% names(treatment)) {
    df_treatment$tInf <- treatment$tinf
  }

  if ("rate" %in% names(treatment)) {
    df_treatment$tInf <- df_treatment$amount/treatment$rate
  }

  # Same with reset
  trtName <- .addUnitaryTrtName(treatment = treatment, groupName = groupName)
  if (length(.lixoftCall("getOccasionElements")$occasions) > 0) {
    occLevel <- .lixoftCall("getOccasionElements")$occasions
    occLevelName <- .lixoftCall("getOccasionElements")$name
    occLevelTime <- .lixoftCall("getOccasionElements")$time
    occLevelId <- .lixoftCall("getOccasionElements")$id

    tt <- df_treatment$time
    df_occ <- NULL

    if (is.null(occLevelId)) {
      occLevelTime <- c(occLevelTime, max(tt) + 1)

      for (idx in seq_along(occLevel)) {
        occlist <- occLevel[[idx]]
        names(occlist) <- occLevelName
        df_trt <- df_treatment[tt >= occLevelTime[idx] & tt < occLevelTime[idx + 1],]
        if (nrow(df_trt) > 0) {
          df <- cbind(as.list(occlist), df_trt)
          df_occ <- rbind(df_occ, df)
        }
      }
    } else {

      for (id in unique(occLevelId)) {
        occTimeId <- c(occLevelTime[occLevelId == id], max(tt) + 1)
        occId <- occLevel[occLevelId == id]

        for (idx in seq_along(occId)) {
          occlist <- occId[[idx]]
          names(occlist) <- occLevelName
          occlist <- c(ID = id, occlist)
          df_trt <- df_treatment[tt >= occTimeId[idx] & tt < occTimeId[idx + 1],]
          if (nrow(df_trt) > 0) {
            df <- cbind(as.list(occlist), df_trt)
            df_occ <- rbind(df_occ, df)
          }
        }
      }
    }

    admID <- treatment$type
    treat <- .addDataFrameTemp(df = df_occ[names(df_occ) != "type"])
    messageMatch = "treatment data"
    names(messageMatch) <- paste0("'", treat, "'")
    .lixoftCall("defineTreatmentElement",
                list(name = trtName, element = list(admID = admID, data = treat)),
                messageMatch = messageMatch)

  } else {
    admID <- treatment$type
    .lixoftCall("defineTreatmentElement",
                list(name = trtName, element = list(admID = admID, data = df_treatment)))
  }

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

    if (is.string(treat)) {
      trtName <- .addUnitaryTrtName(treat, groupName)
      trt <- utils::read.table(file = treat, header = T, sep = .getDelimiter(treat))
      admID <- unique(trt$type)
      treat <- .addDataFrameTemp(trt[names(trt) != "type"])
      messageMatch = "treatment data"
      names(messageMatch) <- paste0("'", treat, "'")
      .lixoftCall("defineTreatmentElement", list(name = trtName,
                                                 element = list(admID = admID, data = treat)),
                  messageMatch = messageMatch)
      trtName_full <- c(trtName_full, trtName)

    } else {
      trtName <- .addUnitaryTrt(treatment = treat, groupName = groupName)
      trtName_full <- c(trtName_full, trtName)
    }
  }

  .lixoftCall("setGroupElement", list(group = groupName, elements = trtName_full))

  return(invisible(TRUE))
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
# It is split between individual and covariate
# If groupName is NULL, it will be added to the shared group
################################################################################
.addParameter <- function(parameter, groupName = NULL){
  if(is.null(parameter)) {
    return(invisible(TRUE))
  }

  covElements <- .lixoftCall("getCovariateElements")
  popElements <- .lixoftCall("getPopulationElements")
  indivElements <- .lixoftCall("getIndividualElements")

  outName <- NULL

  # paramtype: individual or population
  if (is.null(groupName)) {
    g <- .lixoftCall("getGroups")[[1]]
  } else {
    g <- .lixoftCall("getGroups")[sapply(.lixoftCall("getGroups"), function(g) g$name == groupName)][[1]]
  }
  paramType <- g$parameter$type

  if (is.string(parameter) ) {
    parameterValues <- utils::read.table(file = parameter, header = T, sep = .getDelimiter(parameter))
    namesParamValues <- names(parameterValues)
    nameIntersect = intersect(namesParamValues, c('id', 'pop', 'occ', 'occ1', 'occ2', 'occevid'))

    if (length(nameIntersect)) {
      indexID_OCC = match(nameIntersect, namesParamValues)
      paramNames <- namesParamValues[-indexID_OCC]
    } else {
      paramNames <- namesParamValues
    }

  } else {
    paramNames <- names(parameter)
  }

  expectedParams <- c()

  # if ind params & pop params defined togerther --> raise error
  parameterInd <- .getParameter(parameter, type = "ind")
  parameterPop <- .getParameter(parameter, type = "pop")

  if (length(parameterInd) > 0 & length(parameterPop) > 0) {
    stop("Population parameters and individuals parameters cannot be defined together.", call. = FALSE)
  }

  #---------------------------------------------------------------------------
  # If the parameter describe a covariate
  if (length(covElements) > 0) {
    # Add a covariate element
    outNameCov <- paste0('manCov', groupName, '_', length(.lixoftCall("getCovariateElements")) + 1)
    covParam <- covElements[[1]]$data
    covParamName <- names(covParam)[-1]

    parameterCov <- .getParameter(parameter, type = "cov")

    # check if cov parameters are missing
    missingcov <- setdiff(covParamName, paramNames)
    if (is.string(parameterCov)) {
      messageMatch <- "parameter data"
      names(messageMatch) <- paste0("'", parameterCov, "'")
      .lixoftCall("defineCovariateElement",
                  list(name = outNameCov, element = parameterCov),
                  messageMatch = messageMatch)
      outName <- c(outName, outNameCov)

    } else if (!is.null(parameterCov)) {
      covElement <- parameterCov

      # if multiple values, transform covariates to dataframe
      if (nrow(covElement) > 1) {
        covElement <- cbind(data.frame(id = 1:nrow(covElement)), covElement)

      } else if (length(missingcov)) {
        # if missing covariate, replace values in default covariate element
        covParam[names(parameterCov)] <- parameterCov
        # covElement <- as.data.frame(matrix(covParam, ncol = length(covParam)))
        covElement <- covParam

      }

      # Define covariate element
      if (nrow(covElement) > 1) {
        # Convert in text file
        tempFile = .addDataFrameTemp(df = covElement)
        messageMatch <- "covariate parameters data"
        names(messageMatch) <- paste0("'", tempFile, "'")
        .lixoftCall("defineCovariateElement", list(name = outNameCov, element = tempFile),
                    messageMatch = messageMatch)

      } else {
        .lixoftCall("defineCovariateElement", list(name = outNameCov, element = covElement))
      }
      outName <- c(outName, outNameCov)
    }
    expectedParams <- c(expectedParams, covParamName)
  }

  #---------------------------------------------------------------------------
  # If the parameter describe a population parameters
  if (length(popElements) > 0 & length(parameterPop) > 0) {
    # Add a population element
    outNamePop <- paste0('manualPop', groupName,'_', length(.lixoftCall("getPopulationElements")) + 1)
    popParam <- popElements[[1]]$data[-1]
    if (is.data.frame(popParam)) {
      popParam <- .transformToNumeric(popParam)
    }
    popParam <- unlist(popParam)
    popParamName <- names(popParam)
    if (is.string(parameterPop)) {
      parameterValues <- utils::read.table(file = parameterPop, header = T, sep = .getDelimiter(parameterPop))
      if (! "pop" %in% names(parameterValues)) {
        stop("A data.frame of population parameters can only be used in combination with the argument 'npop'. ",
             "The data frame must have a column 'pop'.", call. = FALSE)
      }

      tempFile = .addDataFrameTemp(parameterValues[names(parameterValues) != "pop"])
      messageMatch <- "population parameters data"
      names(messageMatch) <- paste0("'", tempFile, "'")
      .lixoftCall("definePopulationElement", list(name = outNamePop, element = tempFile),
                  messageMatch = messageMatch)
      outName <- c(outName, outNamePop)

    } else {
      popElement <- as.data.frame(matrix(popParam, ncol = length(popParam)))
      names(popElement) <- names(popParam)
      popElement[names(parameterPop)] <- parameterPop

      .lixoftCall("definePopulationElement", list(name = outNamePop, element = popElement))
      outName <- c(outName, outNamePop)
    }

    # if (paramType == "population") expectedParams <- c(expectedParams, popParamName)
    expectedParams <- c(expectedParams, popParamName)
  }

  #---------------------------------------------------------------------------
  # If the parameter describe an individual parameters
  if (length(indivElements) > 0 & length(parameterInd) > 0) {
    # Add a population element
    outNameIndiv <- paste0('manualIndiv', groupName, '_', length(.lixoftCall("getIndividualElements")) + 1)
    indivParam <- unlist(indivElements[[1]]$data[-1])
    indivParamName <- names(indivParam)

    if (is.string(parameterInd)) {
      messageMatch <- "individual parameters data"
      names(messageMatch) <- paste0("'", parameterInd, "'")
      .lixoftCall("defineIndividualElement", list(name = outNameIndiv, element = parameterInd),
                  messageMatch = messageMatch)
      outName <- c(outName, outNameIndiv)

    } else {
      indivElement <- as.data.frame(matrix(indivParam, ncol = length(indivParam)))
      names(indivElement) <- names(indivParam)
      indivElement[names(parameterInd)] <- parameterInd

      .lixoftCall("defineIndividualElement", list(name = outNameIndiv, element = indivElement))
      outName <- c(outName, outNameIndiv)
    }
    # if (paramType == "individual") expectedParams <- c(expectedParams, indivParamName)
    expectedParams <- c(expectedParams, indivParamName)
  }

  # warn in case of missing parameters
  ismlxproject <- any(grepl("mlx", names(.lixoftCall("getOutputElements"))))
  .checkMissingParameters(paramNames, expectedParams, ismlxproject)

  # warn in case of extra parameters
  .checkExtraParameters(paramNames, expectedParams)

  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }

  if (!is.null(outName)) {
    for (index in seq_along(outName)) {
      .lixoftCall("setGroupElement", list(group=groupName, elements = outName[index]))
    }
  }

  return(invisible(TRUE))
}


.getParameter <- function(parameter, type = "pop"){
  extraNames <- c('id', 'pop', 'occ', 'occ1', 'occ2', 'occevid')

  # Parameters that describes a covariate
  if (type == "cov") {
    elements <- .lixoftCall("getCovariateElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(elements[[1]]$data)[-1]

  } else if (type == "pop") {
    elements <- .lixoftCall("getPopulationElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(unlist(elements[[1]]$data[-1]))

  } else {
    elements <- .lixoftCall("getIndividualElements")
    if (length(elements) == 0) return(NULL)
    elementNames <- names(unlist(elements[[1]]$data[-1]))
  }

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
        parameter <- .addDataFrameTemp(parameterValues)
      }
    } else {
      parameter <- parameterValues
    }
  } else {
    parameter <- as.data.frame(as.list(parameter))
    parameter <- parameter[names(parameter) %in% elementNames]
    parameter <- .transformToNumeric(parameter)

    # filter dataframe with cov / ind / pop parameters only
    # if (all(sapply(parameter, function(p) length(p) == 1))) {
    #   parameter <- unlist(parameter)
    # }
    # parameter <- parameter[names(parameter) %in% elementNames]
    #
    # # transform data to numeric
    # parameter <- sapply(parameter,
    #                     function(p) ifelse(suppressWarnings(is.na(as.numeric(p))), p, as.numeric(p)))
  }
  return(parameter)
}

.addMlxParameter <- function(parameter, groupName = NULL){
  if(is.null(parameter)) {
    return(invisible(TRUE))
  }

  if (! parameter[1] %in% c("mode", "mean", "pop")) {
    return(invisible(TRUE))
  }

  if (is.null(groupName)) {
    groupName <- .lixoftCall("getGroups")[[1]]$name
  }

  if (parameter[1] == "mode") {
    if (! "mlx_EBEs" %in% names(.lixoftCall("getIndividualElements"))) {
      stop("Invalid parameter mode. EBEs parameters were not found in monolix project.", call. = FALSE)
    }
    .lixoftCall("setGroupElement", list(group = groupName, elements = "mlx_EBEs"))

  } else if(parameter[1] == "mean") {
    if (! "mlx_CondMean" %in% names(.lixoftCall("getIndividualElements"))) {
      stop("Invalid parameter mean. Conditional mean parameters were not found in monolix project.", call. = FALSE)
    }
    .lixoftCall("setGroupElement", list(group = groupName, elements = "mlx_CondMean"))

  } else {
    if (! "mlx_CondMean" %in% names(.lixoftCall("getIndividualElements"))) {
      stop("Invalid parameter pop. Individual population parameters were not found in monolix project.", call. = FALSE)
    }
    .lixoftCall("setGroupElement", list(group = groupName, elements = "mlx_PopIndiv"))
  }

  remaining <- .lixoftCall("getGroupRemaining", list(group = groupName))
  popData <- .lixoftCall("getPopulationElements")$mlx_Pop$data

  namesPopData <- names(popData)
  for (indexParam in seq_along(remaining)) {
    remainingName <- names(remaining)[indexParam]
    remaining[indexParam] <- popData[which(namesPopData==remainingName)]
  }

  .lixoftCall("setGroupRemaining", list(group = groupName, remaining = remaining))

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

    if(parameter[1] %in% c("mode", "mean", "pop")) {
      .addMlxParameter(parameter, groupName)
    } else {
      .addParameter(parameter = parameter, groupName)
    }
  }

  if ("output" %in% names(group)) {
    .addOutput(output = group$output, groupName)
  }

  if ("treatment" %in% names(group)) {
    .addTreatment(treatment = group$treatment, groupName)
  }

  if ("regressor" %in% names(group)) {
    .addRegressor(group$regressor, groupName)
  }

  if ("size" %in% names(group)) {
    .lixoftCall("setGroupSize", list(groupName, group$size))
  }

  return(invisible(TRUE))
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
    # if multiple admin types -> split treatments
    admColumn <- names(trt)[names(trt) %in% c("adm", "type")]
    tColumn <- names(trt)[names(trt) == "time"]
    amtColumn <- names(trt)[names(trt) %in% c("amt", "amount")]

    if (length(admColumn)) {
      admns <- unique(trt[[admColumn]])
      if (length(admns) > 1) {
        for (iadmn in seq_along(admns)) {
          admn <- admns[iadmn]
          if (iadmn == 1) {
            idx <- itr
          } else {
            idx <- length(treatment) + 1
          }
          if (is.data.frame(trt)) {
            newtrt <- trt[trt[[admColumn]] == admn,]
          } else {
            newtrt <- list()
            if (length(trt[[amtColumn]]) > 1) {
              newtrt[[amtColumn]] <- trt[[amtColumn]][trt[[admColumn]] == admn]
            } else {
              newtrt[[amtColumn]] <- trt[[amtColumn]][1]
            }
            newtrt[[tColumn]] <- trt[[tColumn]][trt[[admColumn]] == admn]
            newtrt[[admColumn]] <- admn
          }
          treatment[[idx]] <- newtrt
        }
      } else if (!is.data.frame(trt) & length(trt[[admColumn]]) > 1) {
        treatment[[itr]][[admColumn]] <- trt[[admColumn]][1]
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
    outId <- which(sapply(outputParam, function(o) outputName %in% o$name))
    if (length(outId))
      outInfo <- outputParam[[outId]]
  } else if (!is.null(groupParam)) {
    outInfo <- sapply(groupParam, function(g) g$output[sapply(g$output, function(o) outputName %in% o$name)])
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
