
.is.sds <- function(x) 'sampleDataset' %in% class(x)

#' default bam QC parameters from picard tools
#'
#' @return a vector of QC metrics names

#' Create a demo sample dataset with 5 Samples
#' @return SampleDataset object
#' @export

.sds <- function () {
  bamQcMetr = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'bam' = c(1, 2, 3, 4, 5))
  vcfQcMetr = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'vcf' = c(1, 2, 3, 4, 5))
  annotations = data.frame(
    'SampleID' = c('A', 'B', 'C', 'D', 'E'),
    'Anno' = c('A1', 'A2', 'A3', 'A4', 'A5'))
  sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                      annotations = annotations, primaryID = 'SampleID')
  return(sds)
}


.bamQcMetr <- function(){
  return(c('mean_coverage', "pct_contamination", "pct_chimerism",
           'mean_insert_size', 'gc_dropout', 'at_dropout', 'MedInsertSize',
           'StdInsertSize', 'PctLt30bp'))
}

.vcfQcMetr <- function(){
  return(c("callRate", "nCalled", "nNotCalled", "nHomRef", "nHet", "nHomVar",
           "nSNP", "nInsertion", "nDeletion", "nSingleton", "nTransition",
           "nTransversion","nNonRef", "rTiTv", "rHetHomVar",
           "rInsertionDeletion"))
}

.stratQcMetr <- function() {
  return(c("nDeletion", "nInsertion", "nSNP", "rTiTv", "rHetHomVar",
           "rInsertionDeletion"))
}

.cutoff <- function(){
  data.frame(qcMetrics = c('PctLt30bp', 'pct_contamination', 'pct_chimerism',
                           'callRate'),
             value = c(5, 0.02, 0.05, 0.99),
             greater = c(T, T, T, F),
             stringsAsFactors = F)
}

.checkIds <-function (vec1, vec2) {
  n1 = length(vec1)
  n2 = length(vec2)
  n3 = length(intersect(vec1, vec2))
  cat(paste(n1, n2, n3))
}

.stratQcMetrHail <- function() {
  return(c("nDeletion", "nInsertion", "nSNP", "rTiTv", "rHetHomVar",
           "rInsertionDeletion"))
}

.stratQcMetrGATK <- function() {
  return(c("nEvalVariants", "nInsertions", "nDeletions", "TiTvRatio",
           "InsDelRatio", "HetHomeRatio"))
}
