require(ggplot2)
require(grid)
require(gridExtra)

#' Generate scatter plot of qcMetrics according to samples
#'
#' @param sds sample dataset object
#' @export
sampleQcPlot <- function (...) UseMethod('sampleQcPlot')

#' @export
sampleQcPlot.sampleDataset <- function(
  object, batch = NULL, qcMetrics, pca = F, imputedRace = T,
  geom = 'scatter', outliers = NULL, legend = T, main = 'QC'
) {
  library(ggplot2)
  if (pca) {
    if (imputedRace) {
      strat = 'imputedRace'
    } else {
      strat = 'RACE'
    }
    plot = list()
    plot[[1]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = strat)
    plot[[2]] = .scatter(data = object$df, x = 'PC1', y = 'PC3', strat = strat)
    plot[[3]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = strat)
    .multiplot(plotlist = plot, cols = 2)
  } else {
    if (!is.null(batch)) {
      if (geom == 'scatter') {
        # scatter plot stratified by sample
        object = sort(object, by = batch)
        plt = .scatter(data = object$df, x='index', y=qcMetrics, strat = batch, xlab = 'samples',
                       outliers = outliers, legend = legend, main = main)
      }
      if (geom == 'violin') {
        object$df[[batch]] = factor(.toSameLength(object$df[[batch]]))
        plt = ggplot(object$df, aes_string(batch, qcMetrics, color = batch)) +
          geom_violin() + geom_jitter(height = 0, width = 0.3) + ggtitle(main)
      }
      if(!is.null(outliers)) {
        plt = plt + geom_point(data = object$df[object$df$sampleId %in% outliers, ], colour = 'black', size = 3)
      }
    } else {
      if (geom == 'hist')  {
        plt = qplot(object$df[[qcMetrics]], geom = 'histogram', bins = 100, xlab = qcMetrics)
      }
    }
    return(plt)
  }
}

#' Plot outliers
#'
#' @param tab input table
#' @param qcMetrics which QC metrics
#' @param
#' @export
outlierPlots <- function(tab, qcMetrics, strat, main, outliers, type = 'violin'){
  plots <- list()  # new empty list
  plots[[1]] = .scatter(tab, x = 'sample', y = qcMetrics, strat = strat, xlab = 'sample',
                        outliers = outliers, main = main, legend = F)

  if (type == 'density') {
    plots[[2]] = qplot(tab[, qcMetrics], color = tab[, strat], geom = "density",
                       main= main, xlab = qcMetrics) +
      geom_vline(linetype="dashed", xintercept = tab[outlier.index, qcMetrics])
  } else {
    plots[[2]] = ggplot(data = tab, aes_string(strat, qcMetrics, color = strat)) +
      geom_violin() + geom_jitter(height = 0, width = 0.3, alpha = 0.3) +
      ggtitle(main) + theme(axis.text.x = element_blank())
    if(!is.null(outliers)) {
      plots[[2]] = plots[[2]] +
        geom_point(data = tab[tab$sampleId %in% outliers, ], colour = 'black', size = 3)
    }
  }
  .multiplot(plotlist = plots, cols = 2)
}

.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

.toSameLength <- function(vec) {
  maxLen = max(nchar((vec)))
  format(vec, width = maxLen)
}

.scatter <- function(
  data, x, y, strat, xlab = NULL, ylab = NULL, outliers = NULL, main = 'QC',
  legend = T
) {
  if (is.null(xlab)) xlab = x
  if (is.null(ylab)) ylab = y
  if (!any(names(data) %in% x)) stop('X not exist in data frame')
  if (!any(names(data) %in% y)) stop('y not exist in data frame')
  if (!any(names(data) %in% strat)) stop('strat not exist in data frame')
  if (!is.numeric(data[[strat]])) { data[[strat]] = .toSameLength(data[[strat]]) }
  color = factor(data[[strat]])

  if (!is.null(outliers)) {
    plt = ggplot(data = data, aes_string(x, y, color = color)) + labs(color = strat) + geom_point() +
      geom_point(data = data[data$sampleId %in% outliers, ], color = 'black', size = 3) + ggtitle(main)
  } else {
    plt = qplot(data[[x]], data[[y]], colour = color, xlab = xlab, ylab = ylab, main = main)
  }
  #plt = plt + scale_color_manual(values = color24)
  if(!legend) {
    plt = plt + guides(color = F)
  }
  return(plt)
}
