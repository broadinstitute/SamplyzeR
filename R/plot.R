#' Generate scatter plot of QC metrics according to samples
#'
#' @param sds sample dataset sds
#' @param ... other arguments
#'
#' @export
#'
sampleQcPlot <- function (sds, ...) UseMethod('sampleQcPlot')

#' Generate scatter plot of QC metrics
#'
#' @param sds sample dataset sds
#' @param annot which annot to visualize
#' @param pca whether perform pca plot
#'

sampleQcPlot.default <- function(
  data, primaryID, qcMetric, annot = NULL, outliers = NULL, legend = T,
  main = 'QC', geom = c('scatter', 'violin', 'hist')
) {
  geom <- match.arg(geom)
  if (!is.null(annot)) {
    if (geom == 'scatter') {
      # scatter plot stratified by sample
      plt = scatter(data = data, x = 'index', y = qcMetric, strat=annot,
                     xlab = 'samples', legend = legend, main = main,
                     outliers = outliers, primaryID = sds$primaryID)
    }
    if (geom == 'violin') {
      data[[annot]] = factor(.toSameLength(data[[annot]]))
      plt <- (ggplot2::ggplot(data, ggplot2::aes_string(annot, qcMetric,
                                                      color = annot))
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
#' @param sds SampleDataset sds
#' @param annot optional, a character string of sample annot to stratify by
#' @param qcMetrics a character string or a vector includes QC metrics to
#'                  explore; if unspecified, all QC metrics in the SDS will
#'                  be used.
#' @param geom a character string indicating which visualization pattern to be
#'             used. One of strings 'scatter', 'violin' or 'hist' can be used.
#' @param outliers a vector of IDs of outliers to show on the figure
#' @param legend whether to include a legend or not
#' @param position position of the legend
#' @param sort whether to sort by annot when ploting
#' @return a list of grob sdss
#' @export

sampleQcPlot.sampleDataset <- function(
  sds, annot, qcMetrics = sds$qcMetrics, geom = c('scatter', 'violin', 'hist'),
  outliers = NULL, legend = T, main = 'QC', position = c('right', 'bottom'),
  ncols = 5, show = FALSE, sort = TRUE
) {
  if(length(qcMetrics) == 1) { ncols = 1 }
  if (!is.null(outliers)) {
    if(!all(outliers %in% sds$df[[sds$primaryID]])) {
      stop("Not all outliers are in the Sample Dataset. Please double check.")
    }
  }
  geom <- match.arg(geom)
  position <- match.arg(position)
  if(is.null(qcMetrics)) {
    qcMetrics = sds$qcMetrics
  }
  if (sort) sds = sort(sds, by = annot)
  plots = sapply(
    qcMetrics,
    function(x) sampleQcPlot(
      data = sds$df, annot = annot, geom = geom, legend = T,
      main = x, qcMetric=x, outliers=outliers, primaryID=sds$primaryID
    ),
    simplify = F
  )
  grobList = multiplotWithSharedLegend(plots, ncols, position, show)
  return(grobList)
}


#' Produce outlier plot
#'
outlierPlots <- function(...) UseMethod('outlierPlots')

#' Produce outlier plot
#'
#' @param tab input table with sample information and metrics
#' @param qcMetrics which metrics to plot
#' @param strat by which factor to stratify the dots
#' @param main title of the plot
#' @param outliers vector of sample IDs to highlight as outliers
#' @param primaryID the primary ID for labeling
#' @param type type of plot, either 'violin' or 'density'
#' @return a list of outlier plots
#'
#' @examples
#' outlierPlots.default(tab, qcMetrics = c("Metric1", "Metric2"), strat = "Group",
#'                       main = "Outlier Plots", outliers = c("SampleA", "SampleB"),
#'                       primaryID = "SampleID", type = "violin")
#'

outlierPlots.default <- function(tab, qcMetrics, strat, main, outliers,
                         primaryID=NULL, type='violin'){
  plots <- list()
  plots[[1]] = scatter(tab, x = 'sample', y = qcMetrics, strat = strat,
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
  multiplot(plotlist = plots, ncols = 2)
}

#' Produce outlier plots for a sampleDataset
#'
#' @return A pdf with all plots

outlierPlots.sampleDataset <- function(
  sds, title, width=15, height=6
) {
  if(grepl('-', sds$zscoreBy)) {
    stop("zscoreBy should not contain '-', please refine that")
  }
  pdf(file=paste(title, outlier, "pdf", sep="."), width=width, height=height)
  strat <- sds$zscoreBy
  sapply(sds$qcMetrics,
         function(qcMetr)
            outlierPlots(
              sds$df, qcMetr, strat, main, outlier, primaryID=sds$primaryID
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
  PCplots(sds, outliers)
  dev.off()
}

#' Generate PC plot of an SampleDataset sds
#'
#' @param sds sample data set
#' @param showPlot whether to show plot
#'
#' @return a list of ggplot sdss
#'
#' @export

PCplots <- function (sds, showPlot=T, cor=F, outliers=NULL) {
  if(!(hasAttr(sds, c('PC', 'inferredAncestry')))) {
    stop("Sample Dataset must have PC and inferred ancestry attributes.")
  }
  plt = list()
  plt[[1]] = scatter(data = sds$df, x = 'PC1', y = 'PC2', outliers=outliers,
                      strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = scatter(data = sds$df, x = 'PC1', y = 'PC3',
                      strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = scatter(data = sds$df, x = 'PC1', y = 'PC2',
                      strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if (showPlot) multiplotWithSharedLegend(plots = plt, ncols = 3)
  plt[[1]] = scatter(data = sds$df, x = 'PC1', y = 'PC2', strat = 'inferredAncestry', main = 'PC1 vs. PC2')
  plt[[2]] = scatter(data = sds$df, x = 'PC1', y = 'PC3', strat = 'inferredAncestry', main = 'PC1 vs. PC3')
  plt[[3]] = scatter(data = sds$df, x = 'PC2', y = 'PC3', strat = 'inferredAncestry', main = 'PC2 vs. PC3')
  if(showPlot) { multiplotWithSharedLegend(plots = plt, ncols = 3) }
  return(plt)
}

#' generate multi panel plots
#'
#' @param plotlist a list of plot
#' @param file file
#' @param ncols number of columns in the plot
#'

multiplot <- function(..., plotlist=NULL, file=NULL, ncols=1, layout=NULL) {
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
#' @export
#'
scatter <- function(
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
         + ggplot2::labs(color = strat)
         + ggplot2::geom_point()
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
#' @param plots a list of grob sdss
#' @param ncols number of rows
#' @param position Position of the legend, between "bottom" and "right"
#' @param show whether show the plot or not
#'
#' @return a grid graphical sds (grob)
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


GeomSplitViolin <- ggplot2::ggproto(
  "GeomSplitViolin", ggplot2::GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL){
    data <- transform(data, xminv = x - violinwidth * (x - xmin),
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv),
      if(grp%%2==1) y else -y)
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ],
                     newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x'])
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)),
                         setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname(
        "geom_split_violin",
        grid::grobTree(ggplot2::GeomPolygon$draw_panel(newdata, ...),
                       quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin",
                       ggplot2::GeomPolygon$draw_panel(newdata, ...))
    }
  })

#' @export
geom_split_violin <- function (
  mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
  draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
    position = position, show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(trim = trim, scale = scale,
                  draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

