.loadInput <- function (input) {
  if (is.data.frame(input)) {
    df = input
  } else if (is.character(input)) {
    df = read.csv(input, sep='\t')
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
#' @param annotInput a tsv file or data.frame that contains sample annot,
#'                   when df was defined, this item could be a vector containing
#'                   names of anntations.
#' @param primaryID ID used for each sample
#' @param bamQcInput a tsv file data frame that contains BAM QC metrics.
#' @param vcfQcInput a tsv file or a data frame that contains VCF QC metrics
#' @param stratify a scalar or vector containing annot to stratify against
#' @param df optional, an R dataframe that contains all sample level data,
#'           including QC metrics and sample annot. When df is provided,
#'           bamQcMetr, vcfQcMetr and annot should be vectors with names
#'           of corresponding fields.
#' @return an sampleDataset object.
#' @export

sampleDataset <- function(
  annotInput, primaryID, bamQcInput=NULL, vcfQcInput=NULL, stratify=NULL,
  df=NULL
){
  if (!exists('annotInput')) { stop("AnnotTsv should be provided.") }
  if (!exists('primaryID')){ stop("PrimaryID should be provided.") }
  if (is.null(bamQcInput) & is.null(vcfQcInput))
    stop("at least one set of QC metrics should be provided.")

  # import annotation
  if (is.null(df)) {
    df = .loadInput(annotInput)
    annotNames = .getNames(df, primaryID)

    if (!all(stratify %in% annotNames)) {
      stop("Not all stratified factors are in the annotation file,
           please double check your input stratify option.")
    }
    bamQcMetrName = NULL
    vcfQcMetrName = NULL
    if (!is.null(bamQcInput)) {
      bamQcMetr = .loadInput(bamQcInput)
      df = .mergeMetr(df, bamQcMetr, primaryID)
      bamQcMetrName = .getNames(bamQcMetr, primaryID)
    }
    if (!is.null(vcfQcInput)) {
      vcfQcMetr = .loadInput(vcfQcInput)
      df = .mergeMetr(df, vcfQcMetr, primaryID)
      vcfQcMetrName = .getNames(vcfQcMetr, primaryID)
    }
  } else {
    # directly create SDS from an already defined data.frame
    bamQcMetrName = bamQcInput
    vcfQcMetrName = vcfQcInput
    annot = annotInput
  }

  sds = list(df = df, qcMetrics = c(bamQcMetrName, vcfQcMetrName),
             bamQcMetr = bamQcMetrName, vcfQcMetr = vcfQcMetrName,
             annot = annotNames, primaryID = primaryID)

  sds$df$index = 1:length(df[[primaryID]])
  class(sds) <- 'sampleDataset'
  return(sds)
}
