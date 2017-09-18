#' @title Calculate z-score of a vector
#'
#' @description Calculate z-score of a vector
#' @param x a vector of values.
#' @param mad binary, whether use median absolute deviation, if false, standard deviation will be used.
#' @return a vector of z-scores
#' @export

calZscore <- function(x, ...) UseMethod("calZscore")

#' @export
calZscore.default <- function(x, mad = T) {
  if(mad){
    dev = mad(x, na.rm = T)
  } else {
    dev = sd(x, na.rm = T)
  }
  x = round((x-median(x, na.rm = T))/dev, 3)
  x
}

#' @title Calculate z-score of sample dataset
#'
#' @description Calculate z-score of sample dataset
#'
#' @param object a sample data set
#' @param strat variable or vector of attributes the sample will be stratified by
#' @param qcMetrics optional, which QC metrics should be stratified on.
#'
#' @return sampleDataset object
#' @export

calZscore.sampleDataset <- function (object, strat = NULL, qcMetrics = NULL) {
  if (is.null(qcMetrics)) { qcMetrics = object$qcMetrics }
  object$zscoreBy = strat
  # initialize output matrix
  object$zscore = .zscoreColNames(qcMetrics)
  object$df[object$zscore] = NA
  if (is.null(strat)) {
    object$df[,object$zscore] = apply(object$df[,qcMetrics], 2, calZscore)
  } else {
    stratLevels = unique(object$df[[strat]])
    stratLevels = stratLevels[!is.na(stratLevels)]
    for (i in stratLevels) {
      index = which(object$df[[strat]] == i)
      object$df[index, object$zscore] = apply(object$df[index, qcMetrics], 2, calZscore)
    }
  }
  vcfQcMetrInUse = .zscoreColNames(intersect(object$vcfQcMetr, qcMetrics))
  object$df$maxVcfzscore = apply(abs(object$df[vcfQcMetrInUse]), 1, max)
  return(object)
}

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
