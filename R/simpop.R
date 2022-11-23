#' Population parameters simulation
#' 
#' Draw population parameters using the covariance matrix of the estimates
#' 
#' See http://simulx.webpopix.org/mlxr/simpopmlx/ for more details.
#' @param n (\emph{integer}) the number of vectors of population parameters (default = 1), 
#' @param project (\emph{string}) a Monolix project, assuming that the Fisher information Matrix was estimated by Monolix.
#' @param fim the (\emph{string}) Fisher Information Matrix estimated by Monolix. fim={"sa", "lin"} (default="sa") 
#' @param parameter (\emph{data.frame}) a data frame with the following columns
#' \itemize{
#'   \item \code{pop.param} (no default) population parameters 
#'   \item \code{sd} (no default) standard deviation of the distribution
#'   \item \code{trans} (default ='N') distribution (N: normal, L: logNormal, G: logitnormal, P: probitnormal, R) 
#'   \item \code{lim.a}: lower bound of logit distribution (if trans != G set lim.a to NA)
#'   \item \code{lim.b}: upper bound of logit distribution (if trans != G set lim.b to NA) 
#' }
#' Only when project is not used.
#' @param corr (\emph{matrix}) correlation matrix of the population parameters (default = identity). Only when project is not used.
#' @param kw.max (\emph{integer}) maximum number of trials for generating a positive definite covariance matrix (default = 100) 
#' @param outputFilename (\emph{string}) when defined, path where the population parameters dataframe will be saved
#' It must be a a file with a csv or txt extension.
#' If no extension is specified, file will be saved by default in csv format
#' @param sep (\emph{string}) file separator when outputFilename is defined (default = ",")
#' @param seed (\emph{integer}) initialization of the random number generator (integer) (by default a random seed will be generated)
#' @return dataframe object with generated population parameters 
#' @examples
#' \dontrun{
#' param <- data.frame(pop.param=c(1.5, 0.5, 0.02, 0.4, 0.15, 0.2, 0.7),
#'                     sd=c(0.2, 0.05, 0.004, 0.05, 0.02, 0.02, 0.05),
#'                     trans=c('N','N','N','L','L','L','N'))
#' pop <- simpopmlx(n=3, parameter=param)
#' }
#' @importFrom stats dnorm qnorm pnorm rnorm
#' @export
simpopmlx <- function(n = 1, project = NULL, fim = NULL, parameter = NULL,
                      corr = NULL, kw.max = 100, outputFilename = NULL, sep = ",", seed = NULL) {
  # connectors
  if (!initRsSimulx()$status)
    return()

  # Check arguments ------------------------------------------------------------
  # output filename
  if (!is.null(outputFilename)) {
    ext <- .getFileExt(outputFilename)
    .checkExtension(ext)
    .checkDelimiter(sep, "sep")
    if (is.null(ext)) {
      outputFilename <- paste0(outputFilename, ".csv")
    }
  }
  
  # Project file and parameter
  if (is.null(parameter) & is.null(project)) {
    stop("You must define either a set of parameters or a Monolix project file.", call. = FALSE)
  }
  if (!is.null(parameter) & !is.null(project)) {
    stop("You must define either a set of parameters or a Monolix project file, not both of them.", call. = FALSE)
  }

  .check_strict_pos_integer(n, "n")
  .check_strict_pos_integer(kw.max, "kw.max")

  if (!is.null(seed)) {
    .check_strict_pos_integer(seed, "seed")
  }
  .check_in_vector(fim, "fim", c("sa", "lin"))

  fixedParameter <- NULL
  if (!is.null(project)) {
    tryCatch( {
      outParamCorr = .getParamCorrProjectFromSimulx(project, fim)
      suppressMessages(initRsSimulx(force=TRUE))
      },
      error = function(e) {
        suppressMessages(initRsSimulx(force=TRUE))
        stop(gsub("Error: ", "", as.character(e)), call. = FALSE)
      }
    )
    parameter <- outParamCorr$parameter
    corr <- outParamCorr$corr
    fixedParameter <- outParamCorr$fixedParameter
  }
  pname <- row.names(parameter)

  # check parameters
  parameter <- .checkSimpopParameter(parameter)

  # check correlation
  nbParams <- length(parameter$pop.param)
  if (is.null(corr)) {
    corr <- diag(nbParams)
    isd0 <- which(parameter$sd == 0)
    if (length(isd0) > 0){
      corr[isd0,] <- NaN
      corr[,isd0] <- NaN
    }
  }
  corr <- .checkSimpopCorr(corr, nbParams)

  mu <- parameter$pop.param
  sd <- parameter$sd
  trans <- parameter$trans
  np <- nrow(parameter)
  bound_min =  parameter$lim.a
  bound_max = parameter$lim.b

  # Generate population parameters ---------------------------------------------
  tryCatch(
    s.res <- .generatePopParams(mu, sd, trans, bound_min, bound_max, corr, pname, kw.max, n, seed),
    error = function(e) {
      acceptedErrors <- c(
        "The correlation matrix of the estimates contains NaN.",
        "You should not use parameter names with '_'.",
        "Maximum number of iterations reached: could not draw definite positive correlation matrix."
      )
      e <- gsub("\n$", "", gsub("^Error: ", "", as.character(e)))
      if (any(sapply(acceptedErrors, function(err) grepl(err, e)))) {
        stop(e, call. = F)
      }
      stop("The Fisher Information matrix does not allow to simulate population parameters.", call. = FALSE)
    }
  )
  # s.res <- .generatePopParams(mu, sd, trans, bound_min, bound_max, corr, pname, kw.max, n, seed)
  
  # Prepare results ------------------------------------------------------------
  s.res <- as.data.frame(s.res)
  names(s.res) <- pname
  
  if (nrow(s.res) > 1)
    s.res <- cbind(list(pop = (1:nrow(s.res))), s.res)
  
  for (index in seq_along(names(s.res))) {
    indexInf <- which(s.res[index,] == 'Inf')
    if (length(indexInf) > 0) {s.res[index,indexInf] <- 1e16}
    indexInf <- which(s.res[index,] == '-Inf')
  }

  # Add fixed parameters
  if (!is.null(fixedParameter)) {
    s.res <- suppressWarnings(cbind(s.res, fixedParameter))
  }
  
  # pop column as factor
  if (is.element("pop", names(s.res))) {
    s.res$pop <- as.factor(s.res$pop)
  }

  # save dataframe
  if (!is.null(outputFilename)) {
    utils::write.table(
      x = s.res[names(s.res) != "pop"], file = outputFilename,
      row.names = FALSE, sep = sep,
      quote = FALSE
    )
  }
  
  return(s.res)
}

.getParamCorrProjectFromSimulx = function(projectFile, fim){

  # load monolix project and hide info messages
  .loadProject(projectFile, software = "monolix")

  # error if population parameter estimation not launched
  if (.lixoftCall("getLaunchedTasks")[["populationParameterEstimation"]] == FALSE) {
    stop("Population parameter estimation has not been launched in Monolix project.", call. = FALSE)
  }
  #----------------------------------------------------------------------------------------
  # Prepare the population parameters
  param <- .lixoftCall("getPopulationParameterInformation")

  # remove latent probability parameters
  covType <- .lixoftCall("getCovariateInformation")$type
  latentCov <- names(covType)[covType == "latent"]
  pattern <- paste0("^", paste(paste0("p", latentCov), collapse="|"))
  platent <- param$name[grep(pattern, param$name)]
  param <- param[!param$name %in% platent,]

  # fixed parameters
  fixedParam <- param[param$method == "FIXED",]
  # remove c error parameters for continuous models
  if (! is.null(.lixoftCall("getContinuousObservationModel"))) {
    # remove c error parameters in fixedParam
    errorParams <- unname(do.call(c, .lixoftCall("getContinuousObservationModel")$parameters))
    cerror <- errorParams[grep("^c", errorParams)]
    fixedParam <- fixedParam[! fixedParam$name %in% cerror,]
  }
  if (nrow(fixedParam)) {
    row.names(fixedParam) <- fixedParam$name
    fixedParam <- t(fixedParam["initialValue"])
    row.names(fixedParam) <- 1
  } else {
    fixedParam <- NULL
  }

  # Remove fixed parameters
  param <- param[param$method != "FIXED",]

  param$pop.param <- param$initialValue
  for (index in seq_along(param$pop.param)) {
    param$pop.param[index] <- .lixoftCall("getEstimatedPopulationParameters",
                                          list(param$name[index]))
  }

  # check fim
  namesFish <- .lixoftCall("getLaunchedTasks")[["standardErrorEstimation"]]
  if (any(namesFish == FALSE)) {
    stop("Standard errors have not been calculated in Monolix project.", call. = FALSE)
  }
  if (is.null(fim)) {
    if ("stochasticApproximation" %in% namesFish) {
      fim <- "sa"
    } else {
      fim <- "lin"
    }
  }
  if (fim == "lin" & ! "linearization" %in% namesFish) {
    warning("Invalid fim argument. Linearization method not used in Monolix project. fim set to 'sa'.", call. = FALSE)
    fim <- "sa"
  }
  if (fim == "sa" & ! "stochasticApproximation" %in% namesFish) {
    warning("Invalid fim argument. StochasticApproximation method not used in Monolix project. fim set to 'lin'.", call. = FALSE)
    fim <- "lin"
  }

  if (tolower(fim) == "lin") {
    sd <- .lixoftCall("getEstimatedStandardErrors")$linearization
  } else {
    sd <- .lixoftCall("getEstimatedStandardErrors")$stochasticApproximation
  }

  # indexNaN <- which(sd == 'NaN')
  # if (!is.null(indexNaN)) {
  #   sd[indexNaN] <- as.numeric(1e6)
  # }
  if (any(is.na(sd))) {
    stop("The Fisher Information Matrix contains NaN.", call. = F)
  }

  param$sd <- param$initialValue
  for (index in seq_along(param$pop.param)) {
    param_name = param$name[index]
    if ("parameter" %in% names(sd)) {
      param$sd[index] <- sd[sd$parameter == param_name, "se"]
    } else if (param_name %in% names(sd)) {
      param$sd[index] <- sd[[param_name]]
    }
  }
  param$trans <- "L"
  param$lim.a <- rep(NA, nrow(param))
  param$lim.b <- rep(NA, nrow(param))
  # param$bound_min <- rep(0, length(param[,1]))
  # param$bound_max <- rep(Inf, length(param[,1]))

  # Update the distribution
  for (indexParam in seq_along(param$name)) {
    paramName <- param$name[indexParam]

    if (nchar(paramName) > 4) {
      endString <- substr(paramName, nchar(paramName)-4+1, nchar(paramName))

      if (endString == '_pop') {
        paramName_pop <- substr(paramName, 1, nchar(paramName)-4)
        distribution <- .lixoftCall("getIndividualParameterModel")$distribution[which(paramName_pop == .lixoftCall("getIndividualParameterModel")$name)]
        if (length(distribution) > 0) {
          if (tolower(distribution) == 'normal') {
            param$trans[indexParam] <- 'N';
            # param$bound_min[indexParam] <- -Inf
          } else if (tolower(distribution) == 'logitnormal') {
            param$trans[indexParam] <- 'G'
            paramLimits <- .lixoftCall("getIndividualParameterModel")$limits[[paramName_pop]]
            param$lim.a[indexParam] <- paramLimits[1]
            param$lim.b[indexParam] <- paramLimits[2]
          } else if (tolower(distribution) == 'probitnormal') {
            param$trans[indexParam] <- 'P'
            # param$bound_min[indexParam] <- 0
            # param$bound_max[indexParam] <- 1
          }
        }
      }
    }
    if (grepl(x = paramName, pattern = 'beta_')) {
      param$trans[indexParam] <- 'N'
      # param$bound_min[indexParam] <- -Inf
    }

    if (grepl(x = paramName, pattern = 'corr_')||grepl(x = paramName, pattern = 'corr1_')||grepl(x = paramName, pattern = 'corr2_')){
      param$trans[indexParam] = 'G'
      param$lim.a[indexParam] = -1
      param$lim.b[indexParam] = 1
    }
  }
  row.names(param) <- param$name
  param <- param[names(param) %in% c("pop.param", "sd", "trans", "lim.a", "lim.b")]

  #----------------------------------------------------------------------------------------
  # Prepare the correlation
  # read correlation between parameter estimates
  if (fim == "lin") {
    corr <- utils::read.csv(file=file.path(.lixoftCall("getProjectSettings")$directory,
                                           "FisherInformation", "correlationEstimatesLin.txt"), header=F)
  } else{
    corr <- utils::read.csv(file=file.path(.lixoftCall("getProjectSettings")$directory,
                                           "FisherInformation", "correlationEstimatesSA.txt"), header=F)
  }
  # remove latent probability parameters
  if (length(platent)) {
    idxlatent <- which(corr[,1] %in% platent)
    corr <- corr[-idxlatent, - idxlatent]
  }
  corr <- corr[,-1]
  #eigenCorr = -min(eigen(corr)$values,0)

  corr <- corr + 1e-4 * diag(x=1, nrow=nrow(param), ncol=nrow(param))

  return(list(parameter=param, corr=corr, fixedParameter=fixedParam))
}

.generatePopParams <- function(mu, sd, trans, bound_min, bound_max, corr, pname,
                               kw.max = 100, n = 1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  np <- length(mu)

  i.omega <- c(grep("^omega_",pname), grep("^omega2_", pname))
  i.corr <- unique(c(grep("^r_",pname), grep("^corr_", pname)))

  #
  inan <- which(is.nan(as.matrix(corr)), arr.ind=TRUE)
  if (dim(inan)[1] > 0) {
    i1 <- which(as.vector(table(inan[,1])) < np)
  } else {
    i1 <- (1:np)
  }
  
  if (!is.null(corr)) {
    corr1 <- corr[i1, i1]
  } else {
    corr1 <- diag(rep(1,length(i1)))
  }
  
  if (length(which(is.na(corr1))) > 0) {
    stop("The correlation matrix of the estimates contains NaN.", call. = F)
  }

  # Generate population parameters ---------------------------------------------
  se1 <- sd[i1]
  tr1 <- trans[i1]
  mu1 <- mu[i1]
  bound_max1 <- bound_max[i1]
  bound_min1 <- bound_min[i1]
  #
  set <- se1
  mut <- mu1
  iL <- which(tr1 == "L")
  
  #  Log normal distribution
  set[iL] <- sqrt(log(1+(se1[iL]/mu1[iL])^2))
  mut[iL] <- log(mu1[iL]) - (set[iL]^2)/2
  
  # logit normal distribution
  iG <- which(tr1 == "G")
  set[iG] <- se1[iG] * (bound_max1[iG] - bound_min1[iG]) / ((mu1[iG] - bound_min1[iG]) * (bound_max1[iG] - mu1[iG]))
  mut[iG] <- log((mu1[iG] - bound_min1[iG]) / (bound_max1[iG] - mu1[iG]))
  
  # R distribution
  iR <- which(tr1 == "R")
  set[iR] <- se1[iR] * 2 / (1 - mu1[iR]^2)
  mut[iR] <- log((mu1[iR] + 1) / (1 - mu1[iR]))
  
  # Probit normal distribution
  iP <- which(tr1 == "P")
  set[iP] <- se1[iP] / dnorm(qnorm(mu1[iP]))
  mut[iP] <- qnorm(mu1[iP])
  Rt <- chol((set%*%t(set)) * corr1)
  #Rt <- chol(corr1)*set

  K <- length(i1)
  
  n.corr <- length(i.corr)
  if (n.corr > 0){
    corr.name <- pname[i.corr]
    g <- gregexpr("_", corr.name)
    nk1 <- vector(length = n.corr)
    nk2 <- vector(length = n.corr)
    for (k in (1:n.corr)){
      if (length(g[[k]]) > 2) {
        stop("You should not use parameter names with '_'.", call. = F)
      }
      cnk <- corr.name[k]
      gk <- g[[k]][1:2]
      nk1[k] <- substr(cnk, gk[1] + 1, gk[2] - 1)
      nk2[k] <- substr(cnk, gk[2] + 1, nchar(cnk))
    }
    nvar <- unique(c(nk1, nk2))
    ir1 <- match(nk1, nvar)
    ir2 <- match(nk2, nvar)
    ind1.r <- matrix(c(ir2, ir1) ,ncol = 2)
    ind2.r <- matrix(c(ir1, ir2), ncol = 2)
    n.r <- length(nvar)
    R.r <- diag(rep(1, n.r))
  }

  #
  n1 <- n
  s.res <- NULL
  kw <- 0
  while (n1 > 0) {
    kw <- kw + 1
    if (kw > kw.max) {
      stop("Maximum number of iterations reached: could not draw definite positive correlation matrix.", call. = F)
    }
    x <- matrix(rnorm(K * n1), ncol = K)
    st <- t(t(x %*% Rt) + mut)
    st[,iL] <- exp(st[,iL])
    st[,iG] <- t(bound_min1[iG] + bound_max1[iG] * t(exp(st[,iG]))) / (1 + exp(st[,iG]))
    st[,iP] <- pnorm(st[,iP])
    st[,iR] <- (exp(st[,iR]) - 1) / (exp(st[,iR]) + 1)
    s <- t(replicate(n1, mu))
    s[,i1] <- st
    if (n.corr > 0) {
      for (i in (1:n1)) {
        sri <- s[i, i.corr]
        R.r[ind1.r] <- sri
        R.r[ind2.r] <- sri
        R.eig <- eigen(R.r, symmetric=TRUE, only.values=TRUE)
        if (min(R.eig$values) > 0) {
          n1 <- n1 - 1
          s.res <- rbind(s.res,s[i,])
        }
      }
    } else {
      n1 <- 0
      s.res <- s
    }
  }
  return(s.res)
}
