#' Get attributes of an object
#'
#' @param object sample dataset object
#' @export

getAttr <- function (object, ...) UseMethod('getAttr', object)

#' @param object sample data set
#' @param attribute name of attribute to return
#' @param return a data frame that contains values of specific attribute
#'
#' @return a data frame with
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
