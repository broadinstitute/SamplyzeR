#' Predict sample Ancestry from genotype principle components
#'
#' @description Principle Components of genotypes with k nearest neighbours.
#' @return Updated sample Dataset object with inferredAncestry tag
#' @export

inferAncestry <- function(...) UseMethod('inferAncestry')


#' @param object matrix for testPCs
#' @param trainSet matrix for training PCs
#' @param ancestry ancestries correspond to the training PCs
#' @param k Number of nearest neighbour to use in this classfication
#' @export
inferAncestry.default <- function(
  object, trainSet, ancestry, k = 5
) {
  inferredAncestry = knn(train = trainSet, test = object, cl = ancestry, k = k)
  return(inferredAncestry)
}

#' @param object Sample Dataset object for Ancestry predition
#' @param trainSet optional, a data frame with known accestry (with column name 'Ancestry') and genotype PCs
#' @param nPC Number of genotype PCs used in this analysis
#' @export

inferAncestry.sampleDataset <- function(
  object, trainSet = NULL, nPC = 3, k = 5
) {
  require(class)

  PC = paste('PC', 1:nPC, sep = '')
  if(is.null(trainSet)) {
    # training without an external reference panel
    if(!(any('knownAncestry' %in% names(object$df))))
      stop("No knownAncestry column available in sample dataset, please provide a reference panel")
    isTrainSet = !is.na(object$df$knownAncestry)
    trainSet = object$df[isTrainSet, PC]
    testSet = object$df[!isTrainSet, PC]
    cl = object$df$knownAncestry[isTrainSet]
  } else {
    if(!any('Ancestry' %in% names(trainSet)))
      stop("'Ancestry' column is not presented in the trainSet")
    cl = trainSet$Ancestry
    trainSet = trainSet[, PC]
    testSet = object$df[, PC]
  }
  inferredAncestry = knn(train = trainSet, test = testSet, cl = cl, k = k)
  if (exists("isTrainSet")) {
    object$df$inferredAncestry = as.character(object$df$knownAncestry)
    object$df[!isTrainSet, 'inferredAncestry'] = as.character(inferredAncestry)
  } else {
    object$df$inferredAncestry = as.character(inferredAncestry)
  }
  return(object)
}
