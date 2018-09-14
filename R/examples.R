#' Load example data
load_examples <- function () {
  bamQcMetr = read.csv('bamQcMetr.tsv', sep = '\t')
  vcfQcMetr = read.csv('vcfQcMetr.tsv', sep = '\t')
  annotations = read.csv('sampleAnnotations.tsv', sep = '\t')
  samplepc = read.csv('samplePCs.tsv', sep = '\t')
  refpc = read.csv('refPCs.tsv', sep ='\t')
  stratify = c('ANCESTRY', 'SeqTech')
  sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                      annotations = annotations, primaryID = 'SampleID')
  return(sds)
}
