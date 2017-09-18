#' Set attributes of a sample dataset
#'
#' Set attributes of a sample dataset
#'
#' @param object sample dataset object
#' @param ... other argumentsoxyg
#'
#' @return object
#' @export
setAttr <- function (object, ...) UseMethod('setAttr', object)

#' Set attributes of a sample dataset
#'
#' @param object sampleDataset
#' @param attributes which attributes to add to sample Dataset
#' @param dataframe content to add to the dataframe
#' @param by primary key of input data frame
#'
#' @return updated sampleDataset object
#' @export

setAttr.sampleDataset <- function(
  object, attributes, input, primaryID = NULL, add = T
) {
  if (attributes == 'df') {
    if(!is.data.frame((input))) stop("Input should be dataframe!")
    if(is.null(primaryID)) stop("Primary ID of input should be specified.")
    if(!any(input[[primaryID]] %in% object$df[[object$primaryID]])) {
      stop("There's no overlap between input and object primary keys. please double check your input")
    }
    if (add) {
      object$df = merge(object$df, input, by.x = object$primaryID, by.y = primaryID,
                        all.x = T)
    } else {
      object$df = input
      object$primaryID = primaryID
    }
  }
  return(object)
}
