#' Flag a sample
#'
#' Flag a sample from a data frame or a sampleDataset
#' @export
#' @seealso{\link{flagSamples.default}}
#' @seealso{\link{flagSamples.sampleDataset}}

flagSamples <- function(...) UseMethod('flagSamples')

#' Flag rows of a data.frame based on values of a column
#'
#' @description Compare values in a column of data.frame, and add a
#' flaggedReason column in the data
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
#' @seealso{\link{flagSamples}}
#' @seealso{\link{flagSamples.sampleDataset}}

flagSamples.default <- function (df, column, cutoff, greater) {
  column = as.character(column)
  if (greater) {
    flag = paste(column, cutoff, sep = '>')
    index = which(df[, column] > cutoff)
  } else {
    flag = paste(column, cutoff, sep = '<=')
    index = which(df[, column] <= cutoff)
  }
  if (!('flaggedReason' %in% names(df))) {
    df$flaggedReason = ''
  }
  # to do: rewrite with sapply
  for (i in index) {
    if (df$flaggedReason[i] == '') {
      df$flaggedReason[i] = flag
    } else {
      df$flaggedReason[i] = paste(df$flaggedReason[i], flag, sep = ',')
    }
  }
  return(df)
}

#' Flag a sample from a sampleDataset
#'
#' @param sds a sampleDataset sds
#' @param cutoffs a data.frame that specifies cutoff values for qcMetrics.
#'                Contains three columns, including "qcMetrics", "value"
#'                and "greater"
#' @param zscore a scalar value that zscore cutoff used, will not flag samples
#'               based on z-score if not specified. Default value is NULL.
#' @return an updated sample dataset sds with flags added.
#' @export
#' @seealso{\link{flagSamples.default}}
#' @seealso{\link{flagSamples}}

flagSamples.sampleDataset <- function(sds, cutoffs, zscore = NULL){
  # conditions
  if(!all(cutoffs$qcMetrics %in% sds$qcMetrics)){
    warning("Not all QC metrics from cutoff table is presented in the
            SampleDataset. ")
  }
  if(all(!cutoffs$qcMetrics %in% sds$qcMetrics)) {
    stop("None of metrics in the cutoff table is in the SampleDataset,
         please double check your input data.")
  }
  if (!is.null(zscore) && !('zscore' %in% attributes(sds)$names)) {
    stop("Input dataset has no z-score, please calculate z-score before
         applying z-score filters.")
  }

  sds$df$flaggedReason = ''
  for (i in 1:dim(cutoffs)[1]) {
    # to do: re-write with sapply
    sds$df = flagSamples(sds$df, cutoffs$qcMetrics[i], cutoffs$value[i],
                            cutoffs$greater[i])
  }

  if(!is.null(zscore)) {
    for (i in sds$zscore) {
      sds$df = flagSamples(sds$df, i, zscore, greater = T)
      sds$df = flagSamples(sds$df, i, -zscore, greater = F)
    }
  }
  sds['flaggedReason'] = 'flaggedReason'
  return(sds)
}
