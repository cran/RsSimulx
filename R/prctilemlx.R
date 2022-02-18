#' Percentiles of the empiricial distribution of longitudinal data
#'
#' Compute and display percentiles of the empiricial distribution of longitudinal data.
#' 
#'
#' See http://simulx.webpopix.org/mlxr/prctilemlx/ for more details.
#' @param r a data frame with a column \samp{id}, a column \samp{time} and a column with values.
#' The times should be the same for each individual. 
#' @param col a vector with the three column indexes for \samp{id}, \samp{time/x} and \samp{y}. Default = c(1, 2,3).
#' @param project simulx project filename (with extension ".smlx")
#' @param outputVariableName name of the output to consider. By default the first output will be consider.
#' You must define either a 'r' dataframe and the associated 'col' argument or a simulx project and the name of the output 'outputVariableName"
#' @param number the number of intervals (i.e. the number of percentiles minus 1).
#' @param level the largest interval (i.e. the difference between the lowest and the highest percentile).
#' @param plot if \code{TRUE} the empirical distribution is displayed, if \code{FALSE}
#' the values are returned
#' @param color colors to be used for the plots
#' In case of one group or facet = TRUE, only the first color will be used
#' @param group  variable to be used for defining groups (by default, \samp{group} is used when it exists)
#' @param facet  makes subplots for different groups if \code{TRUE} 
#' @param labels  vector of strings 
#' @param band is deprecated (use number and level instead) ; a list with two fields
#' \itemize{
#'   \item \code{number} the number of intervals (i.e. the number of percentiles minus 1).
#'   \item \code{level} the largest interval (i.e. the difference between the lowest and the highest percentile).
#' }
#' 
#' @return 
#' a ggplot object if \code{plot=TRUE} ; otherwise, a list with fields:
#' \itemize{
#'   \item proba a vector of probabilities of length \code{band$number+1} 
#'   \item color a vector of colors used for the plot of length \code{band$number}
#'   \item y a data frame with the values of the empirical percentiles computed at each time point
#' }
#' @examples
#' \dontrun{
#'   myModel <- inlineModel("
#'   [LONGITUDINAL]
#'   input = {ka, V, Cl}
#'   EQUATION:
#'   C = pkmodel(ka,V,Cl)
#'   
#'   [INDIVIDUAL]
#'   input = {ka_pop, V_pop, Cl_pop, omega_ka, omega_V, omega_Cl}
#'   DEFINITION:
#'   ka = {distribution=lognormal, reference=ka_pop, sd=omega_ka}
#'   V  = {distribution=lognormal, reference=V_pop,  sd=omega_V }
#'   Cl = {distribution=lognormal, reference=Cl_pop, sd=omega_Cl}
#'   ")
#'   
#'   N=2000
#'   
#'   pop.param   <- c(
#'     ka_pop  = 1,    omega_ka  = 0.5,
#'     V_pop   = 10,   omega_V   = 0.4,
#'     Cl_pop  = 1,    omega_Cl  = 0.3)
#'     
#'   res <- simulx(model     = myModel,
#'                 parameter = pop.param,
#'                 treatment = list(time=0, amount=100),
#'                 group     = list(size=N, level='individual'),
#'                 output    = list(name='C', time=seq(0,24,by=0.1)))
#'   # res$C is a data.frame with 2000x241=482000 rows and 3 columns
#'   head(res$C)
#'   # we can display the empirical percentiles of C using the default 
#'   # settings (i.e. percentiles of order 10%, 20%, ... 90%)
#'   prctilemlx(res$C)
#'   # The 3 quartiles (i.e. percentiles of order 25%, 50% and 75%) are displayed by 
#'   # selecting a 50% interval splitted into 2 subintervals
#'   prctilemlx(res$C, number=2, level=50)
#'   # A one 90% interval can be displayed using only one interval
#'   prctilemlx(res$C, number=1, level=90)
#'   # or 75 subintervals in order to better represent the continuous distribution 
#'   # of the data within this interval
#'   prctilemlx(res$C, number=75, level=90)
#'   # prctilemlx produces a ggplot object that can be modified
#'   pl <- prctilemlx(res$C, number=4, level=80) 
#'   pl + ylab("concentration") + ggtitle("predictive distribution")
#'   # The percentiles are not plotted by setting plot=FALSE
#'   pr.out <- prctilemlx(res$C, number=4, level=80, plot=FALSE)
#'   print(pr.out$proba)
#'   print(pr.out$color)
#'   print(pr.out$y[1:5,])
#' }
#' @importFrom ggplot2 ggplot geom_point theme aes geom_line xlab ylab facet_wrap facet_grid
#' scale_color_manual scale_alpha_manual guide_legend geom_polygon
#' @importFrom stats quantile 
#' @export         
prctilemlx <- function(r = NULL, col=NULL, project = NULL, outputVariableName = NULL,
                        number=8, level=80, plot=TRUE, color=NULL,
                        group=NULL, facet=TRUE, labels=NULL, band=NULL) {
  
  # connectors
  if (!initRsSimulx()$status)
    return()

  if (is.null(r) & is.null(project))
    stop("You must define either dataframe 'r' together with argument 'col' or",
         " a  simulx project and the name of the output 'outputVariableName'.", call.=FALSE)
  if (! is.null(r) & !is.null(project))
    stop("You must define either dataframe 'r' together with argument 'col' or",
         " a  simulx project and the name of the output 'outputVariableName'.", call.=FALSE)
  
  if (!is.null(r)) {
    if (!is.null(outputVariableName)) warning("Argument 'outputVariableName' is ignored when 'r' dataframe is specified.")
  }
  if (!is.null(project)) {
    if (!is.null(col)) warning("Argument 'col' is ignored when a simulx 'project' is specified.")
    res <- .processProject(project, outputVariableName)
    r <- res$r
    col <- res$col
  }
  
  if (!is.null(band)) {
    level <- band$level
    number <- band$number
  } 
  m <- number
  
  if (level < 1) level <- 100 * level
  
  q1 <- (1 - level / 100) / 2
  q2 <- 1 - q1
  
  time <- NULL
  if (m %% 2 !=0 ) {
    m.test <- 0
    q <- seq(q1, q2, length.out=(m + 1))
    q <- append(q, 0.5,(m + 1) / 2) 
    m <- m + 1
  } else {
    m.test <- 1
    q <- seq(q1, q2, length.out=m + 1)
    q <- (append(q, 0.5, m / 2))
    q[m / 2 + 2] <- 0.5
    m <- m + 1
  }
  
  # column names
  if (is.null(col)) {
    r.names <- names(r)
    if (any(r.names=="id")) {
      col[1] <- which(r.names=="id")
    } else {
      col[1] <- 1
    }
    if (any(r.names=="time")) {
      col[2] <- which(r.names=="time")
    } else {
      col[2] <- 2
    }
    if (!is.null(attr(r,"name"))) {
      col[3] <- max(which(r.names==attr(r,"name")))
    } else {
      col[3] <- 3
    }
  }
  
  id <- r[,col[1]]
  d <- col[3]
  t <- unique(r[,col[2]])
  n <- length(t)
  
  # n <- length(which(id==id[1]))
  # t <- r[,col[2]][1:n]
  x.label=names(r)[col[2]]
  y.label=names(r)[col[3]]
  
  # define levels of transparency for percentiles
  nq <- length(q)
  ncol <- trunc(nq / 2)
  if (number <= 2) {
    alphas <- c(0.4)
  } else {
    alphas <- seq(0.2, 0.8, length.out=ncol) 
  }
  if (m.test == 1)
    alphas[ncol] <- 1
  alphas <- c(alphas, rev(alphas))
  
  if (m%%2==0){
    alphas <- alphas
  }else{
    alphas <- alphas[-(nq+1)/2]
  }

  # groups
  if (!is.null(r$group) & is.null(group)) {
    group <- "group"
  } else if (length(group)==1 && group=="none") {
    group = NULL
  }
  
  if (is.data.frame(group)) {
    attr.name <- attr(r,"name")
    r <- merge(r,group,by="id",sort=FALSE)
    attr(r,"name") <- attr.name
    group <- names(group)
    group <- group[-which(group=="id")]
  }
  
  if (!is.null(labels)) {
    if (length(group)==1) 
      labels <- ifelse(is.list(labels),labels, list(labels))
    for (k in (1: length(group)))
      r[[group[k]]] <- factor(r[[group[k]]], labels=labels[[k]])
  }
  
  if (!is.null(group)) {
    ig <- interaction(r[group])
  } else {
    n.tot <- dim(r)[1]
    ig <- rep(1, n.tot)
  }
  ig <- factor(ig)
  ug <- levels(ig)
  ng <- length(ug)
  y <- list()
  for (k in (1:ng)) {
    jk <- which(ig==ug[k])
    vk <- matrix(r[jk,col[3]], nrow = n, byrow = FALSE)
    y[[k]] <- apply(vk,1,quantile, probs = round(q,digits=3),  na.rm = TRUE)
  }
  
  # colors
  colors <- gg_color_hue(n = ng)
  if (facet == F) {
    if (! is.null(color)) {
      if (length(color) >= ng) {
        colors <- color[1:ng]
      } else {
        colors <- rep(color, ng)[1:ng]
      }
    }
    if (ng > 1) {
      calphas <- c(sapply(colors, function(col) .addColorAlpha(col, alpha = alphas)))
      groups <- 
      dfCols <- as.data.frame(list(
        color = c(sapply(colors, function(col) .addColorAlpha(col, alpha = alphas))),
        group = rep(unique(Reduce(interaction, r[group])), each = length(alphas)),
        proba = paste(utils::head(q, -1), utils::tail(q, -1), sep = " - ")
      ))
    }
  } else {
    if (! is.null(color)) {
      colors <- rep(color[1], ng)
    } else {
      colors <- rep(colors[1], ng)
    }
  }
  
  if (plot==TRUE){
    nt <- length(t)
    if (m%%2==0){
      q <- q[-(nq+1)/2]
    }else{
      q[nq/2] <- (q[nq/2]+q[nq/2+1])/2
      q <- q[-(nq/2+1)]
    }
    q[nq] <- 1
    q <- round(q,2)
    vf <- rep(as.factor(q[(nq-1):1]), each=2*nt)
    x <- rep(c(t,rev(t)),nq-1)
    
    nbq.max <- 13
    if (nq>nbq.max){
      if (m.test==0){
        iq1 <- round(seq(1,nq/2,length.out=(nbq.max+1)/2))
      }else{
        iq1 <- round(seq(1,nq/2,length.out=(nbq.max+1)/2))
      }
      iq2 <- nq-iq1
      iq <- sort(unique(c(iq1,iq2)))
      vq <- q[iq]
    }else{
      vq <- q[1:length(alphas)]
    }
    
    # color guides
    guide_sfm <- F
    if (facet == F) {
      if (length(unique(colors)) > 1 & ng > 1) {
        guide_sfm <- guide_legend()
        guide_sam <- guide_legend()
      } else {
        guide_sam <- guide_legend(override.aes = list(fill = colors[1]))
      }
    } else {
      guide_sam <- guide_legend(override.aes = list(fill = colors[1]))
    }
    
    bq <- as.character(rev(vq))
    sfm <- scale_fill_manual(name=group, values=colors, guide = guide_sfm)
    scm <- scale_color_manual(name="color", values=colors, guide = FALSE)
    sam <- scale_alpha_manual(name="proba", values=alphas, breaks=bq, guide = guide_sam)
    
    datapoly <- NULL
    for (k in (1:ng)) {
      pr <- NULL
      for (j in (1:(nq-1)))
        pr <- c(pr,y[[k]][j,],rev(y[[k]][j+1,]))
      dk <- data.frame(x,pr,vf)
      if (!is.null(group)) {
        jk <- which(ig==ug[k])
        dk[group] <- r[group][jk[1],]
      }
      datapoly <- rbind( datapoly, dk)
    }
    if (ng > 1) {
      datapoly$groupAll <- as.factor(Reduce(interaction, datapoly[group]))
    }
    pk <- ggplotmlx(data=datapoly)
    if (ng > 1) {
      groupAll <- NULL
      pk <- pk + geom_polygon(aes(x=x, y=pr, fill=groupAll, alpha=vf))
    } else {
      pk <- pk + geom_polygon(aes(x=x, y=pr, fill=colors, alpha=vf))
    }
    pk <- pk +  xlab(x.label)+ylab(y.label)
    pk <- pk + sfm + sam + scm

    # add line
    if (m.test == 1){
      data0 <- NULL
      for (k in (1:ng)) {
        datak <- data.frame(y=y[[k]][(nq+1)/2,],x=t)
        if (!is.null(group)) {
          jk <- which(ig==ug[k])
          datak[group] <- r[group][jk[1],]
        }
        data0 <- rbind(data0, datak)
      }
      if (ng > 1) {
        groupAll <- NULL
        data0$groupAll <- as.factor(Reduce(interaction, data0[group]))
        pk <- pk + geom_line(data=data0, aes(x=x, y=y, color = groupAll))
      } else {
        pk <- pk + geom_line(data=data0, aes(x=x, y=y, color = colors))
      }
    }
    if (facet==TRUE) {
      if (length(group)==1)
        pk <- pk + facet_wrap(group)
      if (length(group)==2)
        pk <- pk + facet_grid(paste(group[1],"~",group[2]))
    }
    res <- pk
  } else {
    dy <- NULL
    for (k in (1:ng)) {
      tyk <- as.data.frame(cbind(round(t,digits=6),t(y[[k]])))
      if (!is.null(group) && nlevels(factor(r[[group]]))>1) {
        jk <- which(ig==ug[k])
        tyk[group] <- factor(r[group][jk[1],])
      }
      dy=rbind(dy, tyk)
    } 
    names(dy)[1]="time"
    if (facet == F & ng > 1) {
      res <- list(proba=q, color=dfCols,y=dy)
    } else {
      colq = unname(sapply(alphas, function(a) .addColorAlpha(colors[1], alpha = a)))
      res <- list(proba=q, color=colq,y=dy)
    }
  }
  return(res)
}

.processProject <- function(project, outputVariableName) {
  .loadProject(project, software = "simulx")

  # run simulation if needed
  if (is.null(.lixoftCall("getSimulationResults"))) {
    .lixoftCall("runSimulation")
  }

  res <- .lixoftCall("getSimulationResults")$res
  if (is.null(outputVariableName)) {
    outputVariableName <- names(res)[1]
    message("No output has been specified, 'outputVariableName' set to ", names(res)[1], ".")
  } else if (! is.element(outputVariableName, names(res))) {
    stop("Output '", outputVariableName, "' does not exist.")
  }

  result <- res[[outputVariableName]]
  col = c(which(names(result) == "id"), which(names(result) == "time"), which(names(result) == outputVariableName))
  return(list(r = result, col = col))
}

.addColorAlpha <- function(color, alpha=1){
  apply(sapply(color, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
}
