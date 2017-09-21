#' Get attributes of an object
#'
#' @param object sample dataset object
#' @export

getAttr <- function (object, ...) UseMethod('getAttr', object)

#' Get attributes of a SampleDataset
#'
#' @param object sample data set
#' @param attribute name of attribute to return
#' @param showID whether to display sample IDs or not in the output data fram
#' @param return a data frame that contains values of specific attribute
#'
#' @return data frame
#' @export

getAttr.sampleDataset <- function(object, attribute, showID = F) {
  if(!(attribute %in% attributes(object)$names)) {
    stop("Input attributes did not exist in provided data object, please double check it.")
  }
  if(showID) {
    return(object$df[c(object$primaryID, unlist(object[attribute]))])
  } else {
    return(object$df[unlist(object[attribute])])
  }
}
