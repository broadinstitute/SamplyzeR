#!/usr/bin/env Rscript


library(devtools)
install_github('xiaolicbs/genotypeQC')
library(optparse)
library(genotypeqc)

option_list <- list(
  make_option(c("-b", "--bamQcMetr"), help = "A tsv for BAM Level quality control metrics"),
  make_option(c("-v", "--vcfQcMetr"), help = "A tsv for VCF Level quality control metrics"),
  make_option(c("-a", "--annotations", help = "A tsv for sample annotation file")),
  make_option(c("-d", "--primaryID"), help = "primary IDs used to specific each sample"),
  make_option(c("-s", "--stratify"), help = "A list of attributes used in the stratified QC metrics analysis, separated by comma"),
  make_option(c("-g", "--samplepc"), help = 'A tsv file with genotype PCs for each sample'),
  make_option(c("-r", "--refpc"), help = 'A tsv file with genotype PCs from a reference set of samples'),
  make_option(c("-o", "--outputDir", help = "Output directory")),
  make_option(c("-p", "--prefix", help = "Prefix of output files"))
)
opt <- parse_args(OptionParser(option_list=option_list))

# construct a sample data set
if(F){
  setwd('~/genotypeqc/examples')
  opt = list(bamQcMetr = 'bamQcMetr.tsv', vcfQcMetr = 'vcfQcMetr.tsv',
             attributes = 'sampleAnnotations.tsv', stratify = 'ANCESTRY',
             primaryID = 'SampleID', outputDir = '.', samplepc = 'samplePCs.tsv',
             refpc = 'refPCs.tsv'
             )
}

bamQcMetr = read.csv(opt$bamQcMetr, sep = '\t')
vcfQcMetr = read.csv(opt$vcfQcMetr, sep = '\t')
annotations = read.csv(opt$annotations, sep = '\t')
samplepc = read.csv(opt$samplepc, sep = '\t')
refpc = read.csv(opt$refpc, sep = '\t')
stratify = unlist(strsplit(opt$stratify, ','))

sds = sampleDataset(bamQcMetr = bamQcMetr, vcfQcMetr = vcfQcMetr,
                    annotations = annotations, primaryID = opt$primaryID)

# calculate z-scores
sds = calZscore(sds, strat = opt$stratify)

head(sds$df[sds$zscore])

# flag samples with cutoffs
cutoffs = data.frame(
  qcMetrics = c('Percent_lt_30bp_frags', 'Contamination_Estimation', 'Percent_Chimeric_Reads'),
  value = c(0.01, 0.02, 0.05),
  greater = c(T, T, T),
  stringsAsFactors = F
)

sds = flagSamples(sds, cutoffs = cutoffs, zscore = 4)
attributes(sds)
getAttr(sds, 'flaggedReason')
### add genotype PCs to sample Dataset
sds = setAttr(sds, attributes = 'PC', data = samplepc, primaryID = 'SampleID')

### predict ancestry
sds = inferAncestry(sds, trainSet = refpc[,c('PC1', 'PC2', 'PC3')], knownAncestry = refpc$group )

## output
setwd(opt$outputDir)
# output to RDS database
save(sds, RDS = paste(opt$prefix, 'RDS', sep = '.'),
     tsv = paste(opt$prefix, 'tsv', sep = '.'),
     xls = paste(opt$prefix, 'xls', sep = '.'))

