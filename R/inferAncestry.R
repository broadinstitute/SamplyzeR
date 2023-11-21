#' Predict sample ancestry from genotype Principle Components
#'
#' Principle Components of genotypes with k nearest neighbours.
#'
#' @param ... Additional parameters for future expansion.
#' @return Updated sample Dataset sds with inferredAncestry tag.
#' @export
#'
#' @seealso{\link{inferAncestry.sampleDataset}}
#' @seealso{\link{inferAncestry.default}}

inferAncestry <- function(...) UseMethod('inferAncestry')

#' Infer ancestry with KNN
#'
#' @param testSet Matrix used for test.
#' @param trainSet Matrix for training PCs.
#' @param ancestry Ancestries corresponding to the training PCs.
#' @param k Number of nearest neighbors to use in this classification.
#'
#' @return A vector of inferred ancestries.
#' @export
#'
#' @examples
#' # Usage of inferAncestry.default
#' inferredAncestry <- inferAncestry.default(testSet, trainSet, ancestry, k = 5)
#' @seealso{\link{inferAncestry.sampleDataset}}
#' @seealso{\link{inferAncestry}}

inferAncestry.default <- function(testSet, trainSet, ancestry, k = 5) {
  inferredAncestry = class::knn(train = trainSet, test = testSet, cl = ancestry, k = k)
  return(inferredAncestry)
}

#' Infer sample ancestry
#'
#' @param sds Sample Dataset sds for ancestry prediction.
#' @param trainSet Data frame with genotype PCs for each sample with known ancestry.
#' @param knownAncestry A vector with known ancestry for each sample in the same order as trainSet.
#' @param nPC Use the first n PCs to predict ancestry.
#'
#' @return Updated sample Dataset sds with inferredAncestry tag.
#' @export
#'
#' @examples
#' # Usage of inferAncestry.sampleDataset
#' sds <- inferAncestry.sampleDataset(sds, trainSet, knownAncestry, nPC = 3, k = 5)
#'
#' @seealso{\link{inferAncestry}}
#' @seealso{\link{inferAncestry.default}}

inferAncestry.sampleDataset <- function(
  sds, trainSet, knownAncestry, nPC = 3, k = 5
) {
  if(dim(trainSet)[1] != length(knownAncestry)) stop("Number of rows of training set is different from knownAncestry.")

  PC = paste('PC', 1:nPC, sep = '')
  if(!all(PC %in% names(trainSet))) stop("Train Set does not contain nPC specified for the analysis.")
  if(!all(PC %in% names(sds$df[sds$PC]))) stop("Test Set does not contain nPC specified for this analysis.")

  trainSet = trainSet[, PC]
  testSet = sds$df[, PC]

  inferredAncestry = inferAncestry(testSet, trainSet, ancestry = knownAncestry, k = k)
  if (exists("isTrainSet")) {
    sds$df$inferredAncestry = as.character(sds$df$knownAncestry)
    sds$df[!isTrainSet, 'inferredAncestry'] = as.character(inferredAncestry)
  } else {
    sds$df$inferredAncestry = as.character(inferredAncestry)
  }
  sds$inferredAncestry = 'inferredAncestry'
  sds$annot = c(sds$annot, 'inferredAncestry')
  return(sds)
}
