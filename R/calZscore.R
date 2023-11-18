#' Calculate z-score of a vector or QC metrics of a SampleDataset
#'
#' @description Calculate z-score of a vector or QC metrics of a SampleDataset,
#'              see details in calZscore.default and calZscore.sampleDataset
#' @return The form of the value returned by predict depends on the class of
#'         its argument. See the documentation of the particular methods for
#'         details of what is produced by that method.
#' @export

calZscore <- function(...) UseMethod("calZscore")

#' Calculate z-score of a vector
#'
#' @description Calculate z-scores of a vector
#' @param x   a vector of values or a sample dataset object
#' @param mad binary, whether use Median Absolute Deviation to calculate the
#'            standard deviation
#' @return    a vector of z-scores
#' @export

calZscore.default <- function(x, mad = T) {
  if (mad) {
    dev <- mad(x, na.rm = T)
  } else {
    dev <- sd(x, na.rm = T)
  }
  # Avoid division by zero
  if (dev  == 0) {
    warning("Standard deviation is zero. Returning original values.")
    return(x)
  }

  x <- round((x-median(x, na.rm = T))/dev, 3)
  return(x)
}

#' Calculate z-scores of QC metrics of a SampleDataset
#'
#' @param object a sampleDataset object
#' @param strat optional, logical, variable or vector of attributes the sample
#'              will be stratified by
#' @param mad optional, binary, whether use Median Absolute Deviation to
#'            calculate z-score or not, default is True
#' @param qcMetrics optional, a vector of names of qc metrics to compute zscore
#' @param maxVcfZ optional, logical, calculate to report maximun Vcf Zscore
#' @return sampleDataset object, with additional attributes: zscoreBy, zscore
#'         and maximumVcfZscore
#' @export

calZscore.sampleDataset <- function (
    object, strat = NULL, qcMetrics = NULL, mad = T, maxVcfZ = F) {
  # use all QC metrics if not specified
  if (is.null(qcMetrics)) {
    qcMetrics = object$qcMetrics
  }

  # initialize output matrix: re-write with setAttr()
  object$zscore = .zscoreColNames(qcMetrics)
  object$df[object$zscore] = NA


  if (is.null(strat)) {
    # calculate z-score across all qcMetrics
    object$df[, object$zscore] = apply(object$df[, qcMetrics], 2, calZscore)
  } else {
    # to do: re-write with setAttr()
    object$zscoreBy = paste(strat, collapse = '-')
    object$df[object$zscoreBy] = apply(object$df[strat], 1, paste, collapse='')

    # calculate z-score within each stratification
    stratLevels = unique(object$df[object$zscoreBy])
    stratLevels = stratLevels[!is.na(stratLevels)]
    for (i in stratLevels) {
      index = which(object$df[object$zscoreBy] == i)
      object$df[index, object$zscore] = apply(object$df[index, qcMetrics], 2,
                                              calZscore)
    }
  }
  if (maxVcfZ) {
    object = .calMaxVcfZ(object, qcMetrics)
  }
  return(object)
}

#' Calculate maximum VCF z-score and update the object with a maxVcfZscore column
#'
#' @param object a sampleDataset object
#' @param qcMetrics a vector of names of QC metrics
#' @return updated sampleDataset object with a new maxVcfZscore column
#'
#' @export
.calMaxVcfZ <- function(object, qcMetrics) {
  vcfQcMetrInUse = .zscoreColNames(intersect(object$vcfQcMetr, qcMetrics))
  object$df$maxVcfzscore = apply(abs(object$df[vcfQcMetrInUse]), 1, max)
  return(object)
}


#' Get z-score column names
#'
#' @param qcMetrics a vector of QC metric names
#' @return a vector of z-score column names
#'
#' @export
.zscoreColNames <- function (qcMetrics) {
  return(paste(qcMetrics, 'Zscore', sep = ''))
}

# Calculate extreme outliers
#
# @description  Defining the functions that will find your lower and upper
# extreme outliers. Extreme outliers are identified using an adjusted Extreme
# threshold, which adjusts the traditional box + whisker upper and lower
# bounds also accounting for the skewness of the distribution.

#library(robustbase)

#.adj.lower.extrm <- function(dat){adjbox(dat,range=3,plot=F)$stats[1]}
#.adj.upper.extrm <- function(dat){adjbox(dat,range=3,plot=F)$stats[5]}

#lower_adj_1 = adj.lower.extrm(metricsSorted_1$HET_HOMVAR_RATIO)
#upper_adj_1 = adj.upper.extrm(metricsSorted_1$HET_HOMVAR_RATIO)
