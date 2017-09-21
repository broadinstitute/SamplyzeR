#' Generate scatter plot of QC metrics according to samples
#'
#' @param object sample dataset object
#'
#' @export
#'
sampleQcPlot <- function (object, ...) UseMethod('sampleQcPlot')

#' Generate scatter plot of an SampleDataset Object
PCplots <- function (object, showPlot = T) {
  if(!(hasAttr(object, c('PC', 'inferredAncestry')))) stop("Sample Dataset must have PC and inferred ancestry attributes.")

  plt = list()
  plt[[1]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = .scatter(data = object$df, x = 'PC1', y = 'PC3', strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if(showPlot) { .multiplot(plotlist = plt, cols = 3) }
  return(plt)
}

#' Generate scatter plot of QC metrics
#'
#' @param object sample dataset object
#' @param annotation which annotation to visualize
#' @param pca whether perform pca plot
#'
#' @export

sampleQcPlot.sampleDataset <- function(
  object, annotation = NULL, qcMetrics, geom = c('scatter', 'violin', 'hist'),
  outliers = NULL, legend = T, main = 'QC'
) {
  if(!(geom %in% c('scatter', 'violin', 'hist'))) stop("Geom should only be 'scatter', 'violin' or 'hist'.")
  if (!is.null(annotation)) {
    if (geom == 'scatter') {
      # scatter plot stratified by sample
      object = sort(object, by = annotation)
      plt = .scatter(data = object$df, x='index', y=qcMetrics, strat = annotation, xlab = 'samples',
                     outliers = outliers, legend = legend, main = main)
    }
    if (geom == 'violin') {
      object$df[[annotation]] = factor(.toSameLength(object$df[[annotation]]))
      plt = ggplot2::ggplot(object$df, aes_string(annotation, qcMetrics, color = annotation)) +
        ggplot2::geom_violin() + ggplot2::geom_jitter(height = 0, width = 0.3) + ggplot2::ggtitle(main)
    }
    if(!is.null(outliers)) {
      plt = plt + ggplot2::geom_point(data = object$df[object$df$sampleId %in% outliers, ], colour = 'black', size = 3)
    }
  } else {
    if (geom == 'hist')  {
      plt = ggplot2::qplot(object$df[[qcMetrics]], geom = 'histogram', bins = 100, xlab = qcMetrics)
    }
  }
  return(plt)
}

outlierPlots <- function(tab, qcMetrics, strat, main, outliers, type = 'violin'){
  plots <- list()  # new empty list
  plots[[1]] = .scatter(tab, x = 'sample', y = qcMetrics, strat = strat, xlab = 'sample',
                        outliers = outliers, main = main, legend = F)

  if (type == 'density') {
    plots[[2]] = ggplot2::qplot(tab[, qcMetrics], color = tab[, strat], geom = "density",
                       main= main, xlab = qcMetrics) +
      geom_vline(linetype="dashed", xintercept = tab[outlier.index, qcMetrics])
  } else {
    plots[[2]] = ggplot2::ggplot(data = tab, aes_string(strat, qcMetrics, color = strat)) +
      ggplot2::geom_violin() + ggplot2::geom_jitter(height = 0, width = 0.3, alpha = 0.3) +
      ggplot2::ggtitle(main) + ggplot2::theme(axis.text.x = element_blank())
    if(!is.null(outliers)) {
      plots[[2]] = plots[[2]] +
        ggplot2::geom_point(data = tab[tab$sampleId %in% outliers, ], colour = 'black', size = 3)
    }
  }
  .multiplot(plotlist = plots, cols = 2)
}

.multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # This function is modified from multiplot of Cookbook of R
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
    grid::grid.newpage()
    grid::pushViewport(
      grid::viewport(
        layout = grid::grid.layout(nrow(layout), ncol(layout))
      )
    )

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

#'
#' Produce a scatter plot from a data frame
#' @param data a data.frame
#' @param strat by which column to stratify
#' @param outliers a vector of outliers
#' @param
#' @param
#'
.scatter <- function(
  data, x, y, strat, xlab = NULL, ylab = NULL, outliers = NULL, main = '',
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
    plt = ggplot2::ggplot(data = data, aes_string(x, y, color = color)) + ggplot2::labs(color = strat) + ggplot2::geom_point() +
      ggplot2::geom_point(data = data[data$sampleId %in% outliers, ], color = 'black', size = 3) + ggplot2::ggtitle(main)
  } else {
    plt = ggplot2::qplot(data[[x]], data[[y]], colour = color, xlab = xlab, ylab = ylab, main = main)
  }
  #plt = plt + scale_color_manual(values = color24)
  if(!legend) {
    plt = plt + ggplot2::guides(color = F)
  }
  return(plt)
}
