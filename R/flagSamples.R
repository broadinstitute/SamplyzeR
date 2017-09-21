#' Flag a sample
#'
#' Flag a sample from a data frame or a sampleDataset
#' @export

flagSamples <- function(...) UseMethod('flagSamples')

#' Flag a sample from a data frame
#'
#' @param df a data frame contained summary statistics
#'
#' @return Return a data frame
#' @export

flagSamples.default <- function (df, stat, cutoff, greater = TRUE, abs = F) {
  stat = as.character(stat)
  if(abs) { df[stat] = abs(df[stat]) }
  if (greater) {
    flag = paste(stat, cutoff, sep = '>=')
    index = which(df[stat] >= cutoff)
  } else {
    flag = paste(stat, cutoff, sep = '<=')
    index = which(df[stat] <= cutoff)
  }
  df$flaggedReason[index] = paste(flag, df$flaggedReason[index], sep = ',')
  return(df)
}

#' Flag a sample from a sample data set
#'
#' @param object sample data set
#' @param cutoffs a data frame that contains three columns, including "qcMetrics", "value" and "greater"
#' @param zscore zscore cutoff used
#'
#' @return an updated sample dataset object with flags added.
#' @export

flagSamples.sampleDataset <- function(object, cutoffs, zscore = NULL){
  object$df$flaggedReason = ''
  if(!all(cutoffs$qcMetrics %in% object$qcMetrics)){
    stop("QC metrics from cutoff table is not consistent with that from sample dataset. ")
  }
  nCut = dim(cutoffs)[1]
  for(i in 1:nCut) {
    object$df = flagSamples(object$df, cutoffs$qcMetrics[i], cutoffs$value[i], cutoffs$greater[i])
  }
  if(!is.null(zscore)) {
    for (i in object$zscore) {
      object$df = flagSamples(object$df, i, 4, abs = T)
    }
  }
  object['flaggedReason'] = 'flaggedReason'
  return(object)
}
