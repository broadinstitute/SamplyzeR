library(tools)

.loadData <- function (input) {
  if (is.data.frame(input)) {
    df = input
  } else if (is.character(input)) {
    df = read_csv(input, sep='\t')
  } else {
    stop("Input is not a tsv file or a data.frame")
  }
  return(df)
}

.mergeMetr <- function(df, metrDf, primaryID) {
  df = merge(df, metrDf, by = primaryID)
  return(df)
}

.getNames <- function(df, primaryID) {
  df = names(df)[-which(names(df) == primaryID)]
}

#' Sample dataset object
#' @description Construct an Sample Dataset object
#' A Sample Data Set object contains several attributes, including a data frame,
#' which contains all the data for samples and several other attributes that
#' contains metadata, such as qcMetrics, bamQcMetr, vcfQcMetr and PrimaryID.
#' To check attributes of your current SDS
#' Create a sample dataset object that contains all information needed to
#' perform a sample quality control. The Dataset object is a list with one
#' dataframe.
#'
#' @param bamQcMetr a data frame that contains BAM level QC metrics
#' @param vcfQcMetr a data frame that contains VCF level QC metrics
#' @param annotations a data frame that contains annotations of the samples.
#' @param df optional, an R dataframe that contains all sample level data, including QC metrics and
#'           sample annotations. When df is provided, bamQcMetr, vcfQcMetr and annotations should
#'           be vectors with names of corresponding fields.
#' @param stratify a scalar or vector containing annotations to stratify against
#' @return an sampleDataset object.
#' @export

sampleDataset <- function(
  annotInput, primaryID, bamQcInput=NULL, vcfQcInput=NULL, stratify=NULL,
  df=NULL
){
  if (!exists(annotInput) || !exists(primaryID))
    stop("Primary ID of QC metrics and sample attributes should be provided.")
  if (is.null(bamQcMetr) & is.null(vcfQcMetr))
    stop("at least one set of QC metrics should be provided.")
  # import annotation
  df = .loadInput(annotInput)
  annotNames = .getNames(df)
  if (!all(stratify %in% annotNames)) {
    stop("Not all stratified factors are in the annotation file,
         please double check your input stratify option.")
  }

  if (!is.null(bamQcInput)) {
    bamQcMetr = .loadInput(bamQcInput)
    df = mergeMetr(df, bamQcMetr, primaryID)
    bamQcMetrName = .getNames(bamQcMetr)
  }
  if (!is.null(vcfQcInput)) {
    vcfQcMetr = .loadInput(vcfQcInput)
    df = .mergeMetr(df, vcfQcMetr, primaryID)
    vcfQcMetrName = .getNames(vcfQcMetr)
  }
  sds = list(df = df, annotations = annotations,
                qcMetrics = c(bamQcMetrName, vcfQcMetrName),
                bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                primaryID = primaryID)
  sds$df$index = 1:length(df[[primaryID]])
  class(object) <- 'sampleDataset'
  return(object)
}
