#' Sample dataset object
#'
#' Create a sample dataset object that contains all information needed to perform a sample
#' quality control. The Dataset object is a list with one dataframe and
#'
#' @param bamQcMetr a data frame that contains BAM level QC metrics
#' @param vcfQcMetr a data frame that contains VCF level QC metrics
#' @param annotations a data frame that contains annotations of the samples.
#' @param df optional, an R dataframe that contains all sample level data, including QC metrics and
#'           sample annotations. When df is provided, bamQcMetr, vcfQcMetr and annotations should
#'           be vectors with names of corresponding fields.
#' @return an sampleDataset object.
#' @export

sampleDataset <- function(bamQcMetr = NULL, vcfQcMetr = NULL, annotations, primaryID = NULL, df = NULL) {
  if (is.null(primaryID)) stop("Primary ID of QC metrics and sample attributes should be provided.")
  if (is.null(bamQcMetr) & is.null(vcfQcMetr)) stop("at least one set of QC metrics should be provided.")
  if (is.null(df)) {
    df = annotations
    if (!is.null(bamQcMetr)) df = merge(df, bamQcMetr, by = primaryID)
    if (!is.null(vcfQcMetr)) df = merge(df, vcfQcMetr, by = primaryID)
    bamQcMetr = names(bamQcMetr)[-which(names(bamQcMetr) == primaryID)]
    vcfQcMetr = names(vcfQcMetr)[-which(names(vcfQcMetr) == primaryID)]
    annotations = names(annotations)[-which(names(annotations) == primaryID)]
  } else {
    if (is.data.frame(bamQcMetr)) stop("bamQcMetr should not be a data.frame when df is presented." )
    if (is.data.frame(vcfQcMetr)) stop("vcfQcMetr should not be a data.frame when df is presented." )
    if (is.data.frame(annotations)) stop("attributes should not be a data.frame when df is presented." )
  }
  object = list(df = df, annotations = annotations,
                qcMetrics = c(bamQcMetr, vcfQcMetr),
                bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                primaryID = primaryID)
  object$df$index = 1:length(df[[primaryID]])

  class(object) <- 'sampleDataset'
  return(object)
}
