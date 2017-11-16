#' Generate scatter plot of QC metrics according to samples
#'
#' @param object sample dataset object
#' @param ... other arguments
#'
#' @export
#'
sampleQcPlot <- function (object, ...) UseMethod('sampleQcPlot')

#' Generate scatter plot of QC metrics
#'
#' @param object sample dataset object
#' @param annotation which annotation to visualize
#' @param pca whether perform pca plot
#'

sampleQcPlot.default <- function(
  data, primaryID, qcMetric, annotation = NULL, outliers = NULL, legend = T,
  main = 'QC', geom = c('scatter', 'violin', 'hist')
) {
  geom <- match.arg(geom)
  if (!is.null(annotation)) {
    if (geom == 'scatter') {
      # scatter plot stratified by sample
      plt = .scatter(data = data, x = 'index', y = qcMetric, strat=annotation,
                     xlab = 'samples', legend = legend, main = main,
                     outliers = outliers, primaryID = sds$primaryID)
    }
    if (geom == 'violin') {
      data[[annotation]] = factor(.toSameLength(data[[annotation]]))
      plt <- (ggplot2::ggplot(data, ggplot2::aes_string(annotation, qcMetric,
                                                      color = annotation))
              + ggplot2::geom_violin()
              + ggplot2::geom_jitter(height = 0, width = 0.3))
    }
  } else {
    if (geom == 'hist')  {
      plt = ggplot2::qplot(data[[qcMetric]], geom = 'histogram', bins = 100,
                           xlab = qcMetric)
    }
  }
  plt = plt + ggplot2::ggtitle(main)
  return(plt)
}

#' Generate a panel of sample QC plots across a list of qcMetrics
#'
#' @param object SampleDataset object
#' @param annotations a character string of sample annotation to stratify by
#' @param qcMetrics a character string or a vector includes QC metrics to
#'                  explore; if unspecified, all QC metrics in the SDS will
#'                  be used.
#' @param geom a character string indicating which visualization pattern to be
#'             used. One of strings 'scatter', 'violin' or 'hist' can be used.
#' @param outliers a vector of IDs of outliers to show on the figure
#' @param legend whether to include a legend or not
#' @param position position of the legend
#' @param sort whether to sort by annotation when ploting
#' @return a list of grob objects
#' @export
#'

sampleQcPlot.sampleDataset <- function(
  object, qcMetrics, annotation = NULL, geom = c('scatter', 'violin', 'hist'),
  outliers = NULL, legend = T, main = 'QC', position = c('right', 'bottom'),
  ncols = 5, show = FALSE, sort = TRUE
) {
  if(length(qcMetrics) == 1) { ncols = 1 }
  if(!all(outliers %in% object$df[[object$primaryID]])) {
    stop("Not all outliers are in the Sample Dataset. Please double check.")
  }
  geom <- match.arg(geom)
  position <- match.arg(position)
  if(is.null(qcMetrics)) {
    qcMetrics = object$qcMetrics
  }
  if(!is.null(annotation)) {
    if (sort) object = sort(object, by = annotation)
    plots = sapply(
      qcMetrics,
      function(x) sampleQcPlot(
        data = object$df, annotation = annotation, geom = 'scatter', legend = T,
        main = x, qcMetric=x, outliers=outliers, primaryID=object$primaryID),
      simplify = F
    )
    grobList = multiplotWithSharedLegend(plots, ncols, position, show)
  }
  return(grobList)
}


#' Produce outlier plot
#'
outlierPlots <- function(...) UseMethod('outlierPlots')

#' @param tab input table with
#' @param qcMetrics which metrics to plot
#' @param strat by which factor to stratify the dots
#' @param main title of the
#' @return

outlierPlots.default <- function(tab, qcMetrics, strat, main, outliers,
                         primaryID=NULL, type='violin'){
  plots <- list()
  plots[[1]] = .scatter(tab, x = 'sample', y = qcMetrics, strat = strat,
                        xlab = 'sample', outliers = outliers, main = main,
                        legend = T, primaryID = primaryID)
  if (type == 'density') {
    plots[[2]] = (ggplot2::qplot(tab[, qcMetrics], color = tab[, strat],
                                geom = "density", main= main, xlab = qcMetrics)
                  + geom_vline(linetype="dashed",
                               xintercept = tab[outlier.index, qcMetrics]))
  } else {
    plots[[2]] = (ggplot2::ggplot(data = tab,
                                  ggplot2::aes_string(x=strat, y=qcMetrics,
                                                      color = strat))
                  + ggplot2::geom_violin()
                  + ggplot2::geom_jitter(height = 0, width = 0.3, alpha = 0.3)
                  + ggplot2::ggtitle(main)
                  + ggplot2::theme(axis.text.x = ggplot2::element_blank()))
    if(!is.null(outliers)) {
      plots[[2]] = (plots[[2]]
                    + ggplot2::geom_point(data=tab[tab$sampleId %in% outliers,],
                                          colour = 'black', size = 3))
    }
  }
  .multiplot(plotlist = plots, ncols = 2)
}

#' Produce outlier plots for a sampleDataset
#'
#' @return A pdf with all plots

outlierPlots.sampleDataset <- function(
  object, title, width=15, height=6
) {
  if(grepl('-', object$zscoreBy)) {
    stop("zscoreBy should not contain '-', please refine that")
  }
  pdf(file=paste(title, outlier, "pdf", sep="."), width=width, height=height)
  strat <- sds$zscoreBy
  sapply(sds$qcMetrics,
         function(qcMetr)
            outlierPlots(
              object$df, qcMetr, strat, main, outlier, primaryID=sds$primaryID
            )
  )


  for (qcMetrics in sds$qcMetrics){
    tab = tab[order(tab[, strat], na.last=T), ]
    tab$sample = 1:dim(tab)[1]
    outlier.index = which(tab[primaryKey] == outlier)
    category = tab[outlier.index, strat]
    zscore = tab[outlier.index, paste(qcMetrics, 'Zscore', sep = '')]
    main = paste(tech, release, outlier, "\n", category, "\n", reason, "\n",
                 qcMetrics, zscore, "by", strat, sep = " ")

  }
  PCplots(object, outliers)
  dev.off()
}

#' Generate PC plot of an SampleDataset Object
#'
#' @param object sample data set
#' @param showPlot whether to show plot
#'
#' @return a list of ggplot objects
#'
#' @export

PCplots <- function (object, showPlot=T, cor=F, outliers=NULL) {
  if(!(hasAttr(object, c('PC', 'inferredAncestry')))) {
    stop("Sample Dataset must have PC and inferred ancestry attributes.")
  }
  plt = list()
  plt[[1]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', outliers=outliers,
                      strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = .scatter(data = object$df, x = 'PC1', y = 'PC3',
                      strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = .scatter(data = object$df, x = 'PC1', y = 'PC2',
                      strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if (showPlot) multiplotWithSharedLegend(plots = plt, ncols = 3)
  plt[[1]] = .scatter(data = object$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = .scatter(data = object$df, x = 'PC1', y = 'PC3', strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = .scatter(data = object$df, x = 'PC2', y = 'PC3', strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if(showPlot) { multiplotWithSharedLegend(plots = plt, ncols = 3) }
  return(plt)
}

#' generate multi panel plots
#'
#' @param plotlist a list of plot
#' @param file
#' @param ncols number of columns in the plot
#'

.multiplot <- function(..., plotlist=NULL, file=NULL, ncols=1, layout=NULL) {
  # This function is modified from multiplot of Cookbook of R
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, ncols * ceiling(numPlots/ncols)),
                     ncol = ncols, nrow = ceiling(numPlots/ncols))
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
      print(plots[[i]], vp = grid::viewport(layout.pos.row = matchidx$row,
                                            layout.pos.col = matchidx$col))
    }
  }
}

#' Normalize elements of a vector to similar size

.toSameLength <- function(vec) {
  maxLen = max(nchar(as.character(vec)))
  format(vec, width = maxLen)
}

#' Produce a scatter plot from a data frame
#'
#' @param data a data.frame
#' @param strat by which column to stratify
#' @param xlab label of x axis
#' @param ylab label of y axis
#' @param outliers a vector of outliers
#' @param main main title of the plot
#' @param legend binary, whether to plot legend or not
#' @param primaryID the primary ID used to

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

  plt = (ggplot2::ggplot(data = data, ggplot2::aes_string(x, y, color = color))
         + ggplot2::labs(color = strat) + ggplot2::geom_point()
         + ggplot2::xlab(xlab) + ggplot2::ylab(ylab))

  if (!is.null(outliers)) {
    if (is.null(primaryID)) stop("Need to specific primary ID of the data set")
    plt = plt + ggplot2::geom_point(data=data[data[[primaryID]] %in% outliers,],
                                    color='black', size=3)
  }
  if (!is.null(main)) plt = plt + ggplot2::ggtitle(main)
  if (!legend) plt = plt + ggplot2::guides(color = F)
  return(plt)
}

#' Generate multiple plots with shared figure legend
#'
#' @param plots a list of grob objects
#' @param ncols number of rows
#' @param position Position of the legend, between "bottom" and "right"
#' @param show whether show the plot or not
#'
#' @return a grid graphical object (grob)
#' @export
#'
multiplotWithSharedLegend <- function(
  plots, ncols = 5, position = c("bottom", "right"), show = T) {

  nrows = ceiling(length(plots)/ncols)
  position <- match.arg(position)

  # extract legend info from first plot
  g <- (ggplot2::ggplotGrob(plots[[1]]
                            + ggplot2::theme(legend.position=position))$grobs)
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)

  gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position="none"))
  gl <- c(gl, nrow = nrows, ncol = ncols)

  combined <- switch(
    position,
    "bottom" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl), legend, ncol = 1,
      heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight)
    ),
    "right" = gridExtra::arrangeGrob(
      do.call(gridExtra::arrangeGrob, gl),
      legend, ncol = 2,
      widths = grid::unit.c(grid::unit(1, "npc") - lwidth, lwidth))
  )
  if(show) {
    grid::grid.newpage()
    grid::grid.draw(combined)
  }
  return(combined)
}
