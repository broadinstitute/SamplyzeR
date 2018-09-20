#' Check whether the sampleDataset has attributes provided
#'
#' @param sds a sampleDataset sds
#' @param attr attributes
#' @return a boolean value
#' @export
hasAttr <- function(sds, attr) {
  if(!.is.sds(sds)) stop('input should be a SampleDataset sds.')
  return(all(attr %in% attributes(sds)$names))
}
