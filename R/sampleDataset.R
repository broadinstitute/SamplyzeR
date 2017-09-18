#' Sample dataset object
#'
#' Create Object sample dataset object to explore outliers.
#'
#' @param df an R data.frame that contains all sample level data, including QC metrics and
#' @param bamQcMetr if df is provided, this is a vector of names
#' @param vcfQcMetr a data frame contain
#' @param attributes
#'
#' @return an sampleDataset object
#'
#' @export

sampleDataset <- function(bamQcMetr, vcfQcMetr, attributes, primaryID = NULL, df = NULL) {
  if(is.null(primaryID)) {
    stop("Primary ID of QC metrics and sample attributes should be provided.")
  }
  if(is.null(df)) {
    df = merge(bamQcMetr, vcfQcMetr, by = primaryID)
    df = merge(df, attributes, by = primaryID)
    bamQcMetr = names(bamQcMetr)[-which(names(bamQcMetr) == primaryID)]
    vcfQcMetr = names(vcfQcMetr)[-which(names(vcfQcMetr) == primaryID)]
    attributes = names(attributes)[-which(names(attributes) == primaryID)]
  } else {
    if(is.data.frame(bamQcMetr)) { stop("bamQcMetr should not be a data.frame when df is presented." )}
    if(is.data.frame(vcfQcMetr)) { stop("vcfQcMetr should not be a data.frame when df is presented." )}
    if(is.data.frame(attributes)){ stop("attributes should not be a data.frame when df is presented." )}
  }
  object = list(df = df, batch = attributes,
                qcMetrics = c(bamQcMetr, vcfQcMetr),
                bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                primaryID = primaryID)
  object$df$index = 1:length(df[[primaryID]])

  class(object) <- 'sampleDataset'
  return(object)
}
