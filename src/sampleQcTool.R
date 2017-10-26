#!/usr/bin/env Rscript
library(devtools)
install_github('xiaolicbs/SamplyzeR')
library(optparse)
library(SamplyzeR)
.stratQcMetrHail <- function() {
  return(c("nDeletion", "nInsertion", "nSNP", "rTiTv", "rHetHomVar", "rInsertionDeletion"))
}

.stratQcMetrGATK <- function() {
  return(c("nEvalVariants", "nInsertions", "nDeletions", "TiTvRatio", "InsDelRatio", "HetHomeRatio"))
}
option_list <- list(
  make_option(c("-b", "--bamQcMetr"), help = "A tsv for BAM Level quality control metrics", default = NULL),
  make_option(c("-v", "--vcfQcMetr"), help = "A tsv for VCF Level quality control metrics", default = NULL),
  make_option(c("-a", "--annotations", help = "A tsv for sample annotation file")),
  make_option(c("-d", "--primaryID"), help = "primary IDs used to specific each sample"),
  make_option(c("-s", "--stratify"), help = "A list of attributes used in the stratified QC metrics analysis, separated by comma"),
  make_option(c("-g", "--samplepc"), help = 'A tsv file with genotype PCs for each sample', default = NULL),
  make_option(c("-r", "--refpc"), help = 'A tsv file with genotype PCs from a reference set of samples', default = NULL),
  make_option(c("-p", "--prefix", help = "Prefix of output files")),
  make_option(c("--cutoffs", help = 'a tsv file that contains hard cutoffs for QC metrics', default = NULL))
)

opt <- parse_args(OptionParser(option_list=option_list))

if(!is.null(opt$bamQcMetr)) {
  bamQcMetr = read.csv(opt$bamQcMetr, sep = '\t')
} else {
  bamQcMetr = NULL
}
if(!is.null(opt$vcfQcMetr)) {
  vcfQcMetr = read.csv(opt$vcfQcMetr, sep = '\t')
} else {
  vcfQcMetr = NULL
}
annotations = read.csv(opt$annotations, sep = '\t')

stratify = unlist(strsplit(opt$stratify, ','))

if (!all(stratify %in% names(annotations))) {
  stop("Not all stratified factors are in the annotation file, please double check your input stratify option.")
}

## Construct an Sample Dataset object
## A Sample Data Set object contains several attributes, including a data frame, which contains all the data for samples and
## several other attributes that contains metadata, such as qcMetrics, bamQcMetr, vcfQcMetr and PrimaryID. To check attributes of your current SDS
sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    annotations = annotations, primaryID = opt$primaryID)

## Perform BAM sample QC
# flag samples with cutoffs
if (is.null(opt$cutoff)) {
   cutoffs = data.frame(
      qcMetrics = c('Percent_lt_30bp_frags', 'Contamination_Estimation',
                    'Percent_Chimeric_Reads'),
      value = c(0.01, 0.02, 0.05),
      greater = c(T, T, T),
      stringsAsFactors = F
   )
} else {
   cutoffs = read.csv(opt$cutoffs, sep = '\t')
}
#sds = flagSamples(sds, cutoffs = cutoffs)
# Save output to tables
cat("\nSave output to RDS and write out sample info.\n")
save(sds, RDS = paste(opt$prefix, 'RDS', sep = '.'),
     tsv = paste(opt$prefix, 'tsv', sep = '.'),
     xls = paste(opt$prefix, 'xls', sep = '.'))
if(F) {
#cat("Save BAM QC plot to pdf.\n")
#grobList = sampleQcPlot(sds, qcMetrics = sds$bamQcMetr, annotation = stratify,
 #                         geom = 'scatter', ncols = 2, show = F)
#ggplot2::ggsave(paste(opt$prefix, stratify,'QcPlot.pdf', sep = '.'), grobList, width = 12, height = 15)
}
if (!all(is.null(opt$refpc), is.null(opt$samplepc))) {
  cat(" o Load Sample and Reference genotype PCs\n")
  samplepc = read.csv(opt$samplepc, sep = '\t')
  refpc = read.csv(opt$refpc, sep = '\t')

  cat(" o Add genotype PCs to sample Dataset\n")
  sds = setAttr(sds, attributes = 'PC', data = samplepc, primaryID = opt$primaryID)

  cat(" o Infer ancestry from PCA results\n")
  sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')], knownAncestry = refpc$knownAncestry)

  cat(" o Perform a stratified QC metrics analysis\n")
  ## Calculate z-scores for each batch across different ancestry
  sds = calZscore(sds, strat=c(stratify,'inferredAncestry'), qcMetrics = .stratQcMetrGATK())

  cat(" o Filter samples with hard cutoffs and z-scores\n")
  sds = flagSamples(sds, cutoffs = cutoffs, zscore = 4)
}
cat(" o Done.")
