#!/usr/bin/env Rscript


library(devtools)
install_github('xiaolicbs/SamplyzeR')
library(optparse)
library(SamplyzeR)

option_list <- list(
  make_option(c("-b", "--bamQcMetr"), help = "A tsv for BAM Level quality control metrics", default = NULL),
  make_option(c("-v", "--vcfQcMetr"), help = "A tsv for VCF Level quality control metrics", default = NULL),
  make_option(c("-a", "--annotations", help = "A tsv for sample annotation file")),
  make_option(c("-d", "--primaryID"), help = "primary IDs used to specific each sample"),
  make_option(c("-s", "--stratify"), help = "A list of attributes used in the stratified QC metrics analysis, separated by comma"),
  make_option(c("-g", "--samplepc"), help = 'A tsv file with genotype PCs for each sample', default = NULL),
  make_option(c("-r", "--refpc"), help = 'A tsv file with genotype PCs from a reference set of samples', default = NULL),
  make_option(c("-o", "--outputDir", help = "Output directory")),
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
  stop("Not all stratified factors are in the annotation file, please double check your input.")
}

## Construct an Sample Dataset object
## A Sample Data Set object contains several attributes, including a data frame, which contains all the data for samples and
## several other attributes that contains metadata, such as qcMetrics, bamQcMetr, vcfQcMetr and PrimaryID. To check attributes of your current SDS
sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    annotations = annotations, primaryID = opt$primaryID)

if (!all(is.null(opt$refpc), is.null(opt$samplepc))) {
  samplepc = read.csv(opt$samplepc, sep = '\t')
  refpc = read.csv(opt$refpc, sep = '\t')

  ## add genotype PCs to sample Dataset
  sds = setAttr(sds, attributes = 'PC', data = samplepc, primaryID = opt$primaryID)

  ### Infer ancestry from PCA results
  sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')], knownAncestry = refpc$group )

  ## Perform a stratified QC metrics analysis
  ## Calculate z-scores for each bach within differnt ancestry
  sds = calZscore(sds, strat =stratify, qcMetrics = sds$vcfQcMetr)

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
     cutoffs = opt$cutoffs
  }
  ### Filter samples with hard cutoffs and z-scores
  sds = flagSamples(sds, cutoffs = cutoffs, zscore = 4)
}

cat("\nSave output to RDS and write out sample info.\n")
setwd(opt$outputDir)
save(sds, RDS = paste(opt$prefix, 'RDS', sep = '.'),
     tsv = paste(opt$prefix, 'tsv', sep = '.'),
     xls = paste(opt$prefix, 'xls', sep = '.'))

cat("Save output to pdf.\n")
grobList = sampleQcPlot(sds, qcMetrics = sds$bamQcMetr, annotation = 'LCSETMax',
                        geom = 'scatter', ncols = 3, outliers = 'Sample-001', show = F)
ggplot2::ggsave(paste(opt$prefix, 'QcPlot.pdf', sep = '.'), grobList, width = 12, height = 6)
cat("Done.")
