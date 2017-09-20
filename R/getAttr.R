getAttr <- function (object, ...) UseMethod('getAttr', object)

#' @param
#' @param return
#' @return a data frame with
getAttr.sampleDataset <- function(object, attribute) {
  return(object$df[unlist(object[attribute])])
}
