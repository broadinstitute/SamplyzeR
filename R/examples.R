#' Load example data
load_examples <- function () {
  bamQcMetr = read.csv('bamQcMetr.tsv', sep = '\t')
  vcfQcMetr = read.csv('vcfQcMetr.tsv', sep = '\t')
  annot = read.csv('sampleAnnotations.tsv', sep = '\t')
  samplepc = read.csv('samplePCs.tsv', sep = '\t')
  refpc = read.csv('refPCs.tsv', sep ='\t')
  stratify = c('ANCESTRY', 'SeqTech')
  sds = sampleDataset(bamQcMetrInput = bamQcMetr, vcfQcMetrInput = vcfQcMetr,
                      annot = annot, primaryID = 'SampleID')
  return(sds)
}
