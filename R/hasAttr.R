#' Check whether the sampleDataset has attributes provided
#'
#' @param object a sampleDataset object
#' @param attr attributes
#' @return a boolean value
#' @export
hasAttr <- function(object, attr) {
  if(!.is.sds(object)) stop('input should be a SampleDataset Object.')
  return(all(attr %in% attributes(object)$names))
}
