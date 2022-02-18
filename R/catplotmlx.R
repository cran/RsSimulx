#'  Plot Categorical Longitudinal Data
#'
#'  Plot the empirical distribution of categorical longitudinal data.
#'
#' See http://simulx.webpopix.org/mlxr/catplotmlx/ for more details.      
#' @param r a data frame with a column \samp{id}, a column \samp{time}, 
#' a column with values and possibly Hk[ja column \samp{group}.
#' @param col a vector of 3 column numbers: (\samp{id}, \samp{time/x}, \samp{y}. Default = c(1, 2,3).
#' @param breaks one of:
#' \itemize{
#'   \item a vector giving the breakpoints,
#'   \item a single number giving the number of segments.
#' }
#' @param plot if \code{TRUE} the empirical distribution is displayed, if \code{FALSE}
#' the values are returned
#' @param color a color to be used for the plots (default="#194280")
#' @param group  variable to be used for defining groups (by default, \samp{group} is used when it exists)
#' @param facet  makes subplots for different groups if \code{TRUE} 
#' @param labels  vector of strings 
#' 
#' @return 
#' a ggplot object if \code{plot=TRUE} ; otherwise, a list with fields:
#' \itemize{
#'   \item color a vector of colors used for the plot 
#'   \item y a data frame with the values of the empirical distribution computed at each time point
#' }
#' @examples
#' \dontrun{
#'   catModel <- inlineModel("
#'   [LONGITUDINAL]
#'   input =  {a,b}
#'   EQUATION:
#'   lp1=a-b*t
#'   lp2=a-b*t/2
#'   DEFINITION:
#'   y = {type=categorical, categories={1,2,3}, 
#'   logit(P(y<=1))=lp1, logit(P(y<=2))=lp2}
#'   ")
#'   
#'   y.out  <- list(name='y', time=seq(0, 100, by=4))
#' 
#'   Ng  <- 1000
#'   g1 <- list(size=Ng, parameter=c(a=6,b=0.2))
#'   res <- simulx(model=catModel, output=y.out, group=g1)
#'   catplotmlx(res$y)
#'   catplotmlx(res$y, breaks=seq(-2,102,by=8), color="purple") 
#'   catplotmlx(res$y, breaks=5, color="#490917") 
#'   
#'   g2 <- list(size=Ng, parameter=c(a=10,b=0.2))
#'   res <- simulx(model=catModel, output=y.out, group=list(g1,g2))
#'   catplotmlx(res$y) 
#'   catplotmlx(res$y, group="none")
#'   
#'   g3 <- list(size=Ng, parameter=c(a=6,b=0.4))
#'   g4 <- list(size=Ng, parameter=c(a=10,b=0.4))
#'   res <- simulx(model=catModel, output=y.out, group=list(g1,g2,g3,g4))
#'   catplotmlx(res$y)
#'    
#'   cov <- data.frame(id=levels(res$y$id), a=rep(c(6,10,6,10),each=Ng), 
#'                     b=rep(c(0.2,0.2,0.4,0.4),each=Ng))
#'   catplotmlx(res$y, group=cov) 
#' }
#' @importFrom ggplot2 ggplot aes xlab ylab ylim ggtitle scale_fill_manual facet_wrap facet_grid geom_ribbon
#' @importFrom graphics hist 
#' @importFrom grDevices hsv rgb2hsv col2rgb 
#' @export         
catplotmlx <- function(r, col=NULL, breaks=NULL, plot=TRUE, color="#194280", 
                           group=NULL, facet=TRUE, labels=NULL) {
  if (is.null(r))
    stop("input of catplotmlx is empty!", call.=FALSE)
  
  r.names <- names(r)
  
  if (is.null(col)) {
    col <- c(
      ifelse(any(r.names=="id"), which(r.names=="id"), 1),
      ifelse(any(r.names=="time"), which(r.names=="time"), 2),
      ifelse(!is.null(attr(r,"name")), max(which(r.names==attr(r,"name"))), 3)
    )
  }
  
  if (is.null(group)) {
    if ("group" %in% names(r)) {
      group <- "group"
    }
  } else {
    if (length(group) == 1 && group=="none") {
      group = NULL
    } else if (is.data.frame(group)) {
      group_df <- group
      group <- setdiff(names(group), names(r)[col[1]])
      r <- merge(r, group_df, sort=FALSE)
    }
    if (!is.null(group)) {
      group <- intersect(group, names(r))
    }
    if (length(group) == 0) {
      group <- NULL
    }
  }

  data <- r[unique(c(col, which(names(r) %in% group)))]
  match_name <- c(id = names(data)[1], time = names(data)[2], y = names(data)[3])
  yname <- names(data)[3]
  names(data)[1:3] <- c("id", "time", "y")

  if (is.null(breaks)){
    bins_middle <- sort(unique(data$time))
    nbins <- length(bins_middle)
    breaks <- c(bins_middle[1] - 1, bins_middle, bins_middle[nbins] + 1)
    breaks <- utils::head(breaks, -1) + diff(breaks) / 2
  } else{
    if (length(breaks) > 1){
      nbins <- length(breaks) - 1
    } else{
      nbins  <- breaks
      zt1 <- min(data$time)
      zt2 <- max(data$time)
      dzt <- (zt2 - zt1) / (10 * nbins)
      breaks  <- seq(zt1 - dzt, zt2 + dzt, length.out=(nbins + 1))
    }
    bins_middle <- utils::head(breaks, -1) + diff(breaks) / 2
  }

  data$y <- factor(data$y)

  sfm <- list()
  col.hsv <- rgb2hsv(col2rgb(color))
  color <- hsv(col.hsv[1], col.hsv[2], col.hsv[3], seq(0.3, 0.9, length.out=nlevels(data$y)))
  sfm <- scale_fill_manual(name=match_name[["y"]], values=color)

  if (!is.null(labels)) {
    if (length(group) == 1) {
      labels <- ifelse(is.list(labels), labels, list(labels))
    }

    for (k in seq_along(group)) {
      data[[group[k]]] <- factor(data[[group[k]]], labels=labels[[k]])
    }
  }
  
  data$bin <- cut(data$time, breaks=breaks, include.lowest=TRUE, labels=bins_middle)
  perc <- stats::aggregate(
    data$y, by = data[c("bin", group)],
    function(y) cumsum(table(y) / length(y))
  )
  perc <- as.data.frame(as.list(perc))
  perc$baseline <- 0
  names(perc) <- c("bin", group, levels(data$y), "baseline")
  perc <- perc[c("bin", group, "baseline", levels(data$y))]
  
  if (plot == T) {
    levels <- c("baseline", levels(data$y))
    perc_ <- stats::reshape(perc, idvar = "id", ids = row.names(perc),
                           times = levels, timevar = "y_level",
                           varying = list(levels), direction = "long")
    perc_ <- .renameColumns(perc_, "baseline", "perc_values")
    row.names(perc_) <- NULL
    perc_$y_level <- factor(perc_$y_level, levels = levels)
    
    upperData <- perc_[perc_$y_level %in% utils::tail(levels, -1),]
    lowerData <- perc_[perc_$y_level %in% utils::head(levels, -1),]
    upperData <- .renameColumns(upperData, c("y_level", "perc_values"), c("upper_perc", "upper_perc_value"))
    lowerData <- .renameColumns(lowerData, c("y_level", "perc_values"), c("lower_perc", "lower_perc_value"))
    bandsData <- upperData
    bandsData$lower_perc <- lowerData$lower_perc
    bandsData$lower_perc_value <- lowerData$lower_perc_value
    bandsData$bin <- as.numeric(as.character(bandsData$bin))
    
    bin <- lower_perc_value <- upper_perc_value <- upper_perc <- NULL
    p <- ggplotmlx()
    p <- p + geom_ribbon(data = bandsData,
                         aes(x = bin, ymin = lower_perc_value,
                             ymax = upper_perc_value, fill = upper_perc))
    
    p <- p  +  xlab("time") + ylab("probability") + ylim(c(0,1)) + sfm
    
    if (facet == T) {
      if (length(group) == 1) {
        p <- p + facet_wrap(group)
      } else if (length(group) == 2) { 
        p <- p + facet_grid(paste(group[1], "~", group[2]))
      } else if (length(group) > 2) {
        p <- p + facet_wrap(stats::as.formula(paste(".~", paste(group, collapse = " + "))))
      }
    }
    res <- p
  } else {
    res <- list(color=color, y=.renameColumns(perc, "bin", "time"))
  }
  return(res)
}  


