#' Predict sample ancestry from genotype Principle Components
#'
#' @description Principle Components of genotypes with k nearest neighbours.
#' @return Updated sample Dataset object with inferredAncestry tag
#' @export

inferAncestry <- function(...) UseMethod('inferAncestry')

#' Infer anestry with KNN
#'
#' @param testSet matrix used for test
#' @param trainSet matrix for training PCs
#' @param ancestry ancestries correspond to the training PCs
#' @param k Number of nearest neighbour to use in this classfication
#' @export

inferAncestry.default <- function(testSet, trainSet, ancestry, k = 5) {
  require(class)
  inferredAncestry = knn(train = trainSet, test = testSet, cl = ancestry, k = k)
  return(inferredAncestry)
}

#' Infer sample ancestry
#'
#' @param object Sample Dataset object for Ancestry predition
#' @param trainSet optional, a data frame with known accestry (with column name 'Ancestry') and genotype PCs
#' @param nPC Number of genotype PCs used in this analysis
#' @export

inferAncestry.sampleDataset <- function(
  object, trainSet = NULL, nPC = 3, k = 5
) {

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
    ancestry = trainSet$Ancestry
    trainSet = trainSet[, PC]
    testSet = object$df[, PC]
  }
  inferredAncestry = inferAncestry(testSet, trainSet, ancestry = ancestry, k = k)
  if (exists("isTrainSet")) {
    object$df$inferredAncestry = as.character(object$df$knownAncestry)
    object$df[!isTrainSet, 'inferredAncestry'] = as.character(inferredAncestry)
  } else {
    object$df$inferredAncestry = as.character(inferredAncestry)
  }
  return(object)
}

