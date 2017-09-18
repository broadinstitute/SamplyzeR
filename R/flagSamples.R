#' Flag a sample
#'
#' Flag a sample

flagSamples <- function(...) UseMethod('flagSamples')

#' Flag a sample from a data frame
#'
#' @usage flagSamples(df, stat, cutoff, greater = TRUE, abs = F)
#'
#' @param df a data frame contained summary statistics
#'
#' @return Return a data frame
#'

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
#' @param cutoffs a data frame
#' @param zscore zscore cutoff
#'
#' @return an updated sample dataset object with flags added.
#'

flagSamples.sampleDataset <- function(object, cutoffs = NULL, zscore = NULL){
  # flag samples with GTEx default PctLt30bp 0.05, chim 0.09, Comtam 0.02
  object$df$flaggedReason = ''
  if(is.null(cutoffs) & is.null(zscore)) stop("Zscore and cutoffs should be provided.")
  if(!is.null(cutoffs)) {
    nCut = dim(cutoffs)[1]
    for(i in 1:nCut) {
      object$df = flagSamples(object$df, cutoffs$qcMetrics[i], cutoffs$value[i], cutoffs$greater[i])
    }
  }
  if(!is.null(zscore)) {
    for (i in object$vcfStratZscore) {
      object$df = flagSamples(object$df, i, 4, abs = T)
    }
  }
  return(object)
}
