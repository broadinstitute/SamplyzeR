#' Generate scatter plot of QC metrics according to samples
#'
#' @param object sample dataset object
#' @param ... other arguments
#'
#' @export
sampleQcPlot <- function (object, ...) UseMethod('sampleQcPlot')

#' Generate scatter plot of QC metrics
#'
#' @param object sample dataset object
#' @param annotation which annotation to visualize
#' @param pca whether perform pca plot
#'

sampleQcPlot.default <- function(
  data, primaryID, qcMetric, annotation = NULL, geom = c('scatter', 'violin', 'hist'),
  outliers = NULL, legend = T, main = 'QC'
) {
  geom <- match.arg(geom)
  if (!is.null(annotation)) {
    if (geom == 'scatter') {
      # scatter plot stratified by sample
      plt = .scatter(data = data, x = 'index', y = qcMetric, strat = annotation, xlab = 'samples',
                     legend = legend, main = main, outliers = outliers, primaryID = sds$primaryID)
    }
    if (geom == 'violin') {
      data[[annotation]] = factor(.toSameLength(data[[annotation]]))
      plt = ggplot2::ggplot(data, ggplot2::aes_string(annotation, qcMetric, color = annotation)) +
        ggplot2::geom_violin() + ggplot2::geom_jitter(height = 0, width = 0.3)
    }
  } else {
    if (geom == 'hist')  {
      plt = ggplot2::qplot(data[[qcMetric]], geom = 'histogram', bins = 100, xlab = qcMetric)
    }
  }
  plt = plt + ggplot2::ggtitle(main)
  return(plt)
}

#' Generate a panel of sample QC plots across a list of qcMetrics
#'
#' @param object SampleDataset object
#' @param annotations a character string of sample annotation to stratify by
#' @param qcMetrics a character string or a vector includes QC metrics to explore; if unspecified, all QC metrics in the SDS will be used.
#' @param geom a character string indicating which visualization pattern to be used. One of strings 'scatter', 'violin' or 'hist' can be used.
#' @param outliers a vector of IDs of outliers to show on the figure
#' @param legend whether to include a legend or not
#' @param position position of the legend
#' @return a list of grob objects
#' @export
#'

sampleQcPlot.sampleDataset <- function(
  object, qcMetrics, annotation = NULL, geom = c('scatter', 'violin', 'hist'),
  outliers = NULL, legend = T, main = 'QC', position = c('right', 'bottom'), ncols = 5,
  show = FALSE
) {
  if(length(qcMetrics) == 1) { ncols = 1 }
  if(!all(outliers %in% object$df[[object$primaryID]])) stop("Not all outliers are in the Sample Dataset. Please double check.")
  geom <- match.arg(geom)
  position <- match.arg(position)
  if(is.null(qcMetrics)) {
    qcMetrics = object$qcMetrics
  }
  if(!is.null(annotation)) {
    object = sort(object, by = annotation)
    plots = sapply(qcMetrics,
                   function(x) sampleQcPlot(data = object$df, annotation = annotation, geom = 'scatter',
                   legend = T, main = x, qcMetric = x, outliers = outliers, primaryID = object$primaryID),
                   simplify = F
                  )
    grobList = .multiplotWithSharedLegend(plots, ncols, position, show)
  }
  return(grobList)
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
    plots[[2]] = ggplot2::ggplot(data = tab, ggplot2::aes_string(strat, qcMetrics, color = strat)) +
      ggplot2::geom_violin() + ggplot2::geom_jitter(height = 0, width = 0.3, alpha = 0.3) +
      ggplot2::ggtitle(main) + ggplot2::theme(axis.text.x = element_blank())
    if(!is.null(outliers)) {
      plots[[2]] = plots[[2]] +
        ggplot2::geom_point(data = tab[tab$sampleId %in% outliers, ], colour = 'black', size = 3)
    }
  }
  .multiplot(plotlist = plots, cols = 2)
}

#' Generate PC plot of an SampleDataset Object
#'
#' @export

PCplots <- function (object, showPlot = T) {
  if(!(hasAttr(object, c('PC', 'inferredAncestry')))) stop("Sample Dataset must have PC and inferred ancestry attributes.")
  plt = list()
  plt[[1]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = .scatter(data = object$df, x = 'PC1', y = 'PC3', strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if(showPlot) { .multiplotWithSharedLegend(plots = plt, ncols = 3) }
  return(plt)
}

.multiplot <- function(..., plotlist=NULL, file, ncols=1, layout=NULL) {
  # This function is modified from multiplot of Cookbook of R
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/ncols)),
                     ncol = cols, nrow = ceiling(numPlots/ncols))
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
  maxLen = max(nchar(as.character(vec)))
  format(vec, width = maxLen)
}

#' Produce a scatter plot from a data frame
#'
#' param data a data.frame
#' param strat by which column to stratify
#' param outliers a vector of outliers

.scatter <- function(
  data, x, y, strat, xlab = NULL, ylab = NULL, outliers = NULL, main = NULL,
  legend = T, primaryID = NULL
) {
  if (!any(names(data) %in% x)) stop('X not exist in data frame')
  if (!any(names(data) %in% y)) stop('y not exist in data frame')
  if (!any(names(data) %in% strat)) stop('strat not exist in data frame')

  if (is.null(xlab)) xlab = x
  if (is.null(ylab)) ylab = y

  if (!is.numeric(data[[strat]])) data[[strat]] = .toSameLength(data[[strat]])
  color = factor(data[[strat]])

  plt = ggplot2::ggplot(data = data, ggplot2::aes_string(x, y, color = color)) +
    ggplot2::labs(color = strat) + ggplot2::geom_point() + ggplot2::xlab(xlab) +
    ggplot2::ylab(ylab)

  if (!is.null(outliers)) {
    plt = plt + ggplot2::geom_point(data = data[ data[[primaryID]] %in% outliers, ],
                                    color = 'black', size = 3)
  }
  if (!legend) plt = plt + ggplot2::guides(color = F)
  return(plt)
}

#' Generate multiple plots with shared figure legend
#'
#' :param plots a list of grob objects
#' :param nrows number of rows
#'
#' :Returns a grid graphical object (grob)
#'
.multiplotWithSharedLegend <- function(
  plots, ncols = 5, position = c("bottom", "right"), show = T) {

  nrows = ceiling(length(plots)/ncols)
  position <- match.arg(position)
  # extract legend info from first plot
  g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)

  gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
  gl <- c(gl, nrow = nrows, ncol = ncols)

  combined <- switch(position,
                     "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)),
                     "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth)))
  if(show) {
    grid::grid.newpage()
    grid::grid.draw(combined)
  }
  return(combined)
}
