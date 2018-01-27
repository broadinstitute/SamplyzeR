#' Flag a sample
#'
#' Flag a sample from a data frame or a sampleDataset
#' @export

flagSamples <- function(...) UseMethod('flagSamples')

#' Flag rows of a data.frame based on values of a column
#'
#' Compare values in a column of data.frame, and add a flaggedReason column
#' in the data
#'
#' Caveat: this function is not able to define greater or equal
#'
#' @param df a data frame contained summary statistics
#' @param column character, column of a data frame
#' @param cutoff numberic,
#' @param greater logical, whether to flag rows that are greater or equal to
#'                the cutoff
#' @param abs
#'
#' @return Return a data frame
#' @export

flagSamples.default <- function (df, column, cutoff, greater) {
  column = as.character(column)
  if (greater) {
    flag = paste(column, cutoff, sep = '>')
    index = which(df[column] > cutoff)
  } else {
    flag = paste(column, cutoff, sep = '<=')
    index = which(df[column] <= cutoff)
  }
  df$flaggedReason[index] = paste(flag, df$flaggedReason[index], sep = ',')
  return(df)
}

#' Flag a sample from a sample data set
#'
#' @param object a sampleDataset object
#' @param cutoffs a data.frame that specifies cutoff values for qcMetrics.
#'                Contains three columns, including "qcMetrics", "value"
#'                and "greater"
#' @param zscore zscore cutoff used, will not flag samples based on z-score
#'               if not specified. Default value is NULL.
#'
#' @return an updated sample dataset object with flags added.
#' @export

flagSamples.sampleDataset <- function(object, cutoffs, zscore = NULL){
  if(!all(cutoffs$qcMetrics %in% object$qcMetrics)){
    warning("Not all QC metrics from cutoff table is presented in the
            SampleDataset. ")
  }
  if(all(!cutoffs$qcMetircs %in% object$qcMetrics)) {
    stop("None of metrics in the cutoff table is in the SampleDataset,
         please double check your input data.")
  }

  object$df$flaggedReason = ''
  object$df = sapply(
    1:dim(cutoffs)[1],
    function(i) flagSamples(object$df, cutoffs$qcMetrics[i], cutoffs$value[i],
                            cutoffs$greater[i]))
  if(!is.null(zscore)) {
    for (i in object$zscore) {
      object$df = flagSamples(object$df, i, zscore, greater = T)
      object$df = flagSamples(object$df, i, -zscore, greater = F)
    }
  }
  object['flaggedReason'] = 'flaggedReason'
  return(object)
}
