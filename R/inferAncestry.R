#' Predict sample ancestry from genotype Principle Components
#'
#' Principle Components of genotypes with k nearest neighbours.
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
  inferredAncestry = class::knn(train = trainSet, test = testSet, cl = ancestry, k = k)
  return(inferredAncestry)
}

#' Infer sample ancestry
#'
#' @param object Sample Dataset object for Ancestry predition
#' @param trainSet data frame with genotype PCs for each sample with known ancestry
#' @param knownAncestry a vector with known ancestry for each sample in same order of trainSet
#' @param nPC Use first n PCs to predict ancestry
#'
#' @export

inferAncestry.sampleDataset <- function(
  object, trainSet, knownAncestry, nPC = 3, k = 5
) {
  if(dim(trainSet)[1] != length(knownAncestry)) stop("Number of rows of training set is different from knownAncestry.")

  PC = paste('PC', 1:nPC, sep = '')
  if(!all(PC %in% names(trainSet))) stop("Train Set does not contain nPC specified for the analysis.")
  if(!all(PC %in% names(sds$df[sds$PC]))) stop("Test Set does not contain nPC specified for this analysis.")

  trainSet = trainSet[, PC]
  testSet = object$df[, PC]

  inferredAncestry = inferAncestry(testSet, trainSet, ancestry = knownAncestry, k = k)
  if (exists("isTrainSet")) {
    object$df$inferredAncestry = as.character(object$df$knownAncestry)
    object$df[!isTrainSet, 'inferredAncestry'] = as.character(inferredAncestry)
  } else {
    object$df$inferredAncestry = as.character(inferredAncestry)
  }
  object$inferredAncestry = 'inferredAncestry'
  object$annotations = c(object$annotations, 'inferredAncestry')
  return(object)
}
