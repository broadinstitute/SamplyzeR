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
  make_option(c("-g", "--samplepc"), help = 'A tsv file with genotype PCs for each sample'),
  make_option(c("-r", "--refpc"), help = 'A tsv file with genotype PCs from a reference set of samples'),
  make_option(c("-o", "--outputDir", help = "Output directory")),
  make_option(c("-p", "--prefix", help = "Prefix of output files"))
)
opt <- parse_args(OptionParser(option_list=option_list))
bamQcMetr = read.csv(opt$bamQcMetr, sep = '\t')
vcfQcMetr = read.csv(opt$vcfQcMetr, sep = '\t')
annotations = read.csv(opt$annotations, sep = '\t')
samplepc = read.csv(opt$samplepc, sep = '\t')
refpc = read.csv(opt$refpc, sep = '\t')
stratify = unlist(strsplit(opt$stratify, ','))

if (!all(stratify %in% names(annotations))) {
  stop("Not all stratified factors are in the annotation file, please double check your input.")
}
## Construct an Sample Dataset object
## A Sample Data Set object contains several attributes, including a data frame, which contains all the data for samples and
## several other attributes that contains metadata, such as qcMetrics, bamQcMetr, vcfQcMetr and PrimaryID. To check attributes of your current SDS
sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    annotations = annotations, primaryID = opt$primaryID)

## add genotype PCs to sample Dataset
sds = setAttr(sds, attributes = 'PC', data = samplepc, primaryID = opt$primaryID)

### Infer ancestry from PCA results
sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')], knownAncestry = refpc$group )

## Perform a stratified QC metrics analysis
## Calculate z-scores for each bach within differnt ancestry
sds = calZscore(sds, strat =stratify, qcMetrics = sds$vcfQcMetr)

# flag samples with cutoffs
cutoffs = data.frame(
  qcMetrics = c('Percent_lt_30bp_frags', 'Contamination_Estimation',
                'Percent_Chimeric_Reads'),
  value = c(0.01, 0.02, 0.05),
  greater = c(T, T, T),
  stringsAsFactors = F
)
### Filter samples with hard cutoffs and z-scores

sds = flagSamples(sds, cutoffs = cutoffs, zscore = 4)

# save output to RDS and write out sample info
save(sds, RDS = paste(opt$prefix, 'RDS', sep = '.'),
     tsv = paste(opt$prefix, 'tsv', sep = '.'),
     xls = paste(opt$prefix, 'xls', sep = '.'))

#
grobList = sampleQcPlot(sds, qcMetrics = sds$bamQcMetr, annotation = 'LCSETMax',
                        geom = 'scatter', ncols = 3, outliers = 'Sample-001')
ggplot2::ggsave('exampleQcPlot.pdf', grobList, width = 12, height = 6)
