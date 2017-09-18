#' Predict ancestry of samples from genotype Principle Components
#'
#' @description Principle Components of genotypes
#'
#' @param object Sample Dataset object for ancestry predition
#' @param trainSet optional, a data frame with sample Id and ethnicity
#' @param nPC Number of genotype PCs used
#'
#' @return updated sample Dataset object with inferredancestry tag
#' @export

inferAncestry <- function(object, ...) UseMethod('ancestryPred', object)

#' @export

inferAncestry.sampleDataset <- function(
  object, trainSet = NULL, nPC = 3, method = 'KNN', k = 5
) {
  PC = paste('PC', 1:nPC, sep = '')
  if(is.null(trainSet)) {
    # training without an external reference panel
    if(!(any('knownancestry' %in% names(object$df))))
      stop("No knownancestry column available in sample dataset, please provide a reference panel")
    isTrainSet = !is.na(object$df$knownancestry)
    trainSet = object$df[isTrainSet, PC]
    testSet = object$df[!isTrainSet, PC]
    cl = object$df$knownancestry[isTrainSet]
  } else {
    if(!any(trainSet$sampleId %in% object$df[[object$primaryID]]))
      stop("sample IDs of training set did not match that of sample Dataset")
    if(!any('ancestry' %in% names(trainSet)))
      stop("no ancestry column presented in the trainSet")
    trainSet = trainSet[ ,PC]
    testSet = object$df[ ,PC]
    cl = trainSet$ancestry
  }
  if (method == 'KNN') {
    library(class)
    inferredancestry = knn(train = trainSet, test = testSet, cl = cl, k = k)
  }
  if (exists("isTrainSet")) {
    object$df$inferredancestry = as.character(object$df$knownancestry)
    object$df[!isTrainSet, 'inferredancestry'] = as.character(inferredancestry)
  } else {
    object$df$inferredancestry = as.character(inferredancestry)
  }
  return(object)
}
