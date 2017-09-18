
.is.sds <- function(x) 'sds' %in% class(x)

#' default bam QC parameters from picard tools
#'
#' @return a vector of QC metrics names
#'
.bamQcMetr <- function(){
  return(c('mean_coverage', "pct_contamination", "pct_chimerism",
           'mean_insert_size', 'gc_dropout', 'at_dropout', 'MedInsertSize',
           'StdInsertSize', 'PctLt30bp'))
}

.vcfQcMetr <- function(){
  return(c("callRate", "nCalled", "nNotCalled", "nHomRef", "nHet", "nHomVar",
           "nSNP", "nInsertion", "nDeletion", "nSingleton", "nTransition",
           "nTransversion","nNonRef", "rTiTv", "rHetHomVar", "rInsertionDeletion"))
}

.cutoff <- function(){
  data.frame(qcMetrics = c('PctLt30bp', 'pct_contamination', 'pct_chimerism', 'callRate'),
             value = c(5, 0.02, 0.05, 0.99),
             greater = c(T, T, T, F), stringsAsFactors = F)
}

.checkIds <-function (vec1, vec2) {
  n1 = length(vec1)
  n2 = length(vec2)
  n3 = length(intersect(vec1, vec2))
  cat(paste(n1, n2, n3))
}

