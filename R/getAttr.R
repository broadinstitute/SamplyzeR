#' Get attributes of an sds
#'
#' @param sds sample dataset sds
#' @export

getAttr <- function (sds, ...) UseMethod('getAttr', sds)

#' Get attributes of a SampleDataset
#'
#' @param sds sample data set
#' @param attribute name of attribute to return
#' @param showID whether to display sample IDs or not in the output data fram
#' @param return a data frame that contains values of specific attribute
#'
#' @return data frame
#' @export

getAttr.sampleDataset <- function(sds, attribute, showID = F) {
  if(!(attribute %in% attributes(sds)$names)) {
    stop("Input attributes did not exist in provided data sds, please double check it.")
  }
  if(showID) {
    return(sds$df[c(sds$primaryID, unlist(sds[attribute]))])
  } else {
    return(sds$df[unlist(sds[attribute])])
  }
}
