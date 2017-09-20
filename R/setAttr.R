#' Update a Sample Data Set
#'
#' Update entries of sample data set, such as the df, bamQcMetr, vcfQcMetr
#'
#' @param object sample dataset object
#' @param ... other arguments
#'
#' @return object
#' @export
#'
setAttr <- function (object, ...) UseMethod('setAttr', object)

#' Set attributes of a sample dataset
#'
#' @param object sampleDataset
#' @param entry which entry to add to sample Dataset
#' @param dataframe content to add to the dataframe
#' @param by primary key of input data frame
#'
#' @return updated sampleDataset object
#' @export

setAttr.sampleDataset <- function(
  object, attributes, inputDf, primaryID = NULL
) {
  if (!is.data.frame((inputDf))) stop("Input should be dataframe!")
  if (is.null(primaryID)) stop("Primary ID of input should be specified.")
  if (!any(inputDf[[primaryID]] %in% object$df[[object$primaryID]])) {
    stop("There's no overlap between input and object primary keys. please double check your input")
  }

  object$df = merge(object$df, inputDf, by.x = object$primaryID, by.y = primaryID,
                    all.x = T)
  object[attributes] = names(inputDf)[which(names(inputDf) != primaryID)]
  return(object)
}
